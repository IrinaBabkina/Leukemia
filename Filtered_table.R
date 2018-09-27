library(readxl)
library(dplyr)
#Загрузим сырые данные от Жени и Оксаны#####
raw_data<-read_excel(path = "data/data_base_IB.xls")

#Отберем те колонки, которые нам интересны
need_collumns<-c(2:17,19,24, 25,51,52,54:64,72,83)
first_filter<- raw_data[,need_collumns]

#Переназовем отобранные колонки (иначе так замучаемся.. )####
name_col_eng<-c("Num_TKM","Surname","Name","Gender","Born","Growth","Weight","Diagn_date","Age_diagn","Phaze_do_TKM","Date_alloTKM","Age_TKM","Diagn-TKM_time","Donor_type","Restade_data","Last_contact_date","Relapse","Status_life","Data_base","Gratwohl","Gratwohl_group","Donor_age","Donor_gender","Consistency","Sourse_transp","ABO_patient","ABO_donor","ABO_consist","CD34","CD34>3","CMV","Condition","ATG","GVHD")
colnames(first_filter)<-name_col_eng

#Запишем то, что получилось в табличку csv####
write.table(x = first_filter, file = "data/first_filtered.csv",sep = ";",row.names = FALSE)

#Дальше будем работать с новой табличкой

head(first_filter)
str(first_filter)

# Факторы в факторы. Не факторы оставляем####
first_filter[-c(5:9,11,13,15:16,21,28:29)] <- lapply( first_filter[-c(5:9,11,13,15:16,21,28:29)], factor) 

#Поиск сомнительного####
str(first_filter)

first_filter$Age_diagn[1:10]#подозрительный дробный возраст. Видимо считали автоматом. ПЕРЕСЧИТАТЬ!!!
# надо бы навесить этикетки на уровни факторов (обсудить с Надей)

#Чувак с полом 2
filter(first_filter, Gender==2)[,2:3]
filter(first_filter, Surname=="esKywqnCqsKhw4E=")[,2:4]#все же пол у него 1

#Cколько у нас пациентов?####
nrow(first_filter)#всего 115
ident<- paste(first_filter$Surname,first_filter$Name)
length(unique(ident))# 104 уникальных пациента (по имени-фамилии)
first_filter[duplicated(ident),]#. Не уникальные. С повторной ТКМ 11 человек
first_filter$ident<-ident

#отберем только тех, у кого ТМК была 1 раз
single_TMK<-filter(first_filter, Num_TKM==1)
length(unique(single_TMK$ident))
nrow(single_TMK)# таких 102 человека
# я так поняла, что разница между 104 уникальных и 102 с первой ТКМ потому, что 2м первую ТКМ делали не в Горбачевке, а вторую в ней

#Считаем возраст на момент диагноза (в годах)####
library(lubridate)

elapsed.time <- first_filter$Born %--% first_filter$Diagn_date
age<-round(as.duration(elapsed.time) / dyears(1),1)

#Считаем промежуток от диагноза до ТКМ в днях####
elapsed.time_1 <- first_filter$Diagn_date %--% first_filter$Date_alloTKM
Diagn_TKM <- round(as.duration(elapsed.time_1) / ddays(1))

#Считаем промежуток от ТКМ до момента обновления базы. 

elapsed.time_2 <- first_filter$Date_alloTKM %--% first_filter$Data_base
TKM_base <- round(as.duration(elapsed.time_2) / ddays(1))

#Можно начинать возиться с выживаемостью))####
library(survival)
library(survminer)

#Соберем это в один дата фрейм для удобства
surv_data<-data_frame(status=first_filter$Status_life, 
                      time_life=TKM_base,
                      time_TKM=Diagn_TKM)
#Уберем наблюдения с NA
surv_data<-surv_data[complete.cases(surv_data),]

#Переведем время до ТКМ в бинарный фактор через медианное значение
surv_data <- surv_data %>% mutate(TKM_group = ifelse(time_TKM >=950, "less", "further"))
surv_data$TKM_group <- factor(surv_data$TKM_group)

#Нам нужно рассчитать общую выживаемость в зависимости от времени до ТКМ
# status==1 принципиальная вещь, которая стоила мне пару часов жизни)) Инвертирует то, что у врачей живой обозначен 0, а мертвый 1

km_fit <- survfit(Surv(time_life, status==1) ~ TKM_group, data=surv_data)
summary(km_fit, times = c(1,30,60,90*(1:10)))
ggsurvplot(km_fit, data = surv_data, pval = TRUE)
