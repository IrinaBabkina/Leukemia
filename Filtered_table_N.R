library(readxl)
library(dplyr)
#Загрузим сырые данные от Жени и Оксаны#####
raw_data<-read_excel(path = "~/Bioinf_project/Autumn_project/Leukemia/data/data_base_IB.xls")

#Отберем те колонки, которые нам интересны
need_collumns<-c(2:17,19,24, 25,51,52,54:64,72,83)
need_collumns_RTAH <- c(2:17,19,24, 25,51,52,83:93)
first_filter<- raw_data[,need_collumns]
filter_for_event_free <- raw_data[, need_collumns_RTAH]
filter_for_event_free$Check <- filter_for_event_free[, 22] != 0
filter_for_event_free$Check1 <- filter_for_event_free[, 26] != 0
filter_for_event_free$Check <- as.numeric(filter_for_event_free$Check)
filter_for_event_free$Check1 <- as.numeric(filter_for_event_free$Check1)
check <- cbind(filter_for_event_free[,27], filter_for_event_free$Check, filter_for_event_free$Check1)
which.na(check[check[,2] != 0, 1])
check <- check[is.na(check[,1]),]
check <- check[check[,2] != 0,]

table_for_check <- raw_data[c(3,7,10,13,35,58,115),]
table_for_check <- table_for_check[is.na(table_for_check[,88]),]
table_for_check1 <- table_for_check[table_for_check[,24] == 0,]

write.csv(table_for_check, file = "~/Downloads/Check_GVHD.csv", row.names = F)
write.csv(table_for_check1, file = "~/Downloads/Check_GVHD_alive.csv", row.names = F)

for (i in range(1:nrow(filter_for_event_free))){
  filter_for_event_free[i,33] <- paste(filter_for_event_free[i,2], 
                                          filter_for_event_free[i,3], sep="")
}
filter_for_event_free$name_fam <- paste(filter_for_event_free[,2], filter_for_event_free[,3], )
?paste
#для хронической РТПХ
chek2 <- cbind(raw_data[,c(90,92)])
chek2 <- chek2[is.na(chek2[,2]),]
chek2 <- chek2[chek2[,1] != 0,]

filter_for_event_free1 <- filter_for_event_free[filter_for_event_free[, 22] != 0,]
cbind(filter_for_event_free[22:26])
for i in filter_for_event_free
for_check <- filter_for_event_free1[is.na(filter_for_event_free1) == T,]

#Переназовем отобранные колонки (иначе так замучаемся.. )####
name_col_eng<-c("Num_TKM","Surname","Name","Gender","Born","Growth",
                "Weight","Diagn_date","Age_diagn","Phaze_do_TKM",
                "Date_alloTKM","Age_TKM","Diagn-TKM_period(days)",
                "Donor_type","Restade_data","Last_contact_date",
                "Relapse","Status_life","Data_base","Gratwohl",
                "Gratwohl_group","Donor_age","Donor_gender",
                "Consistency","Sourse_transp","ABO_patient",
                "ABO_donor","ABO_consist","CD34","CD34>3",
                "CMV","Condition","ATG","GVHD")
colnames(first_filter)<-name_col_eng

name_col_eng<-c("Num_TKM","Surname","Name","Gender","Born","Growth",
                "Weight","Diagn_date","Age_diagn","Phaze_do_TKM",
                "Date_alloTKM","Age_TKM","Diagn-TKM_period(days)",
                "Donor_type","Restade_data","Last_contact_date",
                "Relapse","Status_life","Data_base","Gratwohl",
                "Gratwohl_group","Donor_age","Donor_gender",
                "Consistency","Sourse_transp","ABO_patient",
                "ABO_donor","ABO_consist","CD34","CD34>3",
                "CMV","Condition","ATG","GVHD")
colnames(first_filter)<-name_col_eng


#Дальше будем работать с новой табличкой

head(first_filter)
str(first_filter)

# Факторы в факторы. Не факторы оставляем####
first_filter[-c(5:9,11,13,15:16,21,28:29)] <- 
  lapply(first_filter[-c(5:9,11,13,15:16,21,28:29)], factor) 

#Поиск сомнительного####
str(first_filter)

first_filter$Age_diagn[1:10]#подозрительный дробный возраст. Видимо считали автоматом. ПЕРЕСЧИТАТЬ!!!
# надо бы навесить этикетки на уровни факторов (обсудить с Надей)

#Чувак с полом 2
filter(first_filter, Gender==2)[,2:3]
filter(first_filter, Surname=="esKywqnCqsKhw4E=")[,2:4]#все же пол у него 1

#Считаем возраст на момент диагноза (в годах)####
library(lubridate)

elapsed.time <- first_filter$Born %--% first_filter$Diagn_date
age<-round(as.duration(elapsed.time) / dyears(1),1)
first_filter$Age_diagn<-age

#Считаем промежуток от диагноза до ТКМ в днях####
elapsed.time_1 <- first_filter$Diagn_date %--% first_filter$Date_alloTKM
Diagn_TKM <- round(as.duration(elapsed.time_1) / ddays(1))
first_filter$`Diagn-TKM_period(days)`<-Diagn_TKM

#Считаем промежуток от ТКМ до последнего контакта 

elapsed.time_2 <- first_filter$Date_alloTKM %--% first_filter$Last_contact_date
TKM_base <- round(as.duration(elapsed.time_2) / ddays(1))

#Cколько у нас пациентов?####
nrow(first_filter)#всего 115
ident<- paste(first_filter$Surname,first_filter$Name)
length(unique(ident))# 104 уникальных пациента (по имени-фамилии)
first_filter[duplicated(ident),]#. Не уникальные. С повторной ТКМ 11 человек
first_filter$ident<-ident

#отберем только тех, у кого ТМК была 1 раз
double<-filter(first_filter, Num_TKM==2)$ident #имена людей, у кого было несколько ТКМ

first_filter$Only_one_TKM <- ifelse(first_filter$ident %in% double==T,"no","yes")

#запишем новую колонку в таблицу, которая будет маркировать людей с единственной ТКМ
TKM <- subset(first_filter, first_filter$Only_one_TKM == 'yes')

#Считаем промежуток от ТКМ до последнего контакта 

elapsed.time_2_ <- TKM$Date_alloTKM %--% TKM$Last_contact_date
TKM_base_2 <- round(as.duration(elapsed.time_2_) / ddays(1))
TKM_base_1 <- TKM_base_2 / 30

#Можно начинать возиться с выживаемостью))####
library(survival)
library(survminer)

#Соберем это в один дата фрейм для удобства
surv_data <- data_frame(status = TKM$Status_life, 
                      time_life = TKM_base_1,
                      time_TKM = TKM$`Diagn-TKM_period(days)`)
#Уберем наблюдения с NA
surv_data <- surv_data[complete.cases(surv_data),]

#Нам нужно рассчитать общую выживаемость в зависимости от времени до ТКМ
# status==1 принципиальная вещь, которая стоила мне пару часов жизни)) 
#Инвертирует то, что у врачей живой обозначен 0, а мертвый 1
km_fit <- survfit(Surv(time_life, status==1) ~ 1, data=surv_data)
ggsurvplot(km_fit, data = surv_data, size = 1,  
           linetype = "strata", # change line type by groups
           palette = c("#fd0166"), # custom color palette
           conf.int = TRUE, # Add confidence interval 
           legend.title = "Patients",
           legend = c(0.1, 0.2),
           xlim = c(0, 100),
           xlab = "Время (мес)")$plot + 
  ggtitle("Kaplan-Meier survival curve")+
  theme(legend.text = element_text(size = 14, color = "black"),
        legend.title = element_text(size = 14, color = "black"))


#Нам нужно рассчитать общую выживаемость в зависимости от времени до ТКМ
# status==1 принципиальная вещь, которая стоила мне пару часов жизни)) Инвертирует то, что у врачей живой обозначен 0, а мертвый 1
# График по фактору "время от диагноза до ТКМ" по медиане
km_fit <- survfit(Surv(time_life, status==1) ~ gratwohl, data=surv_data)
summary(km_fit, times = c(1,30,60,90*(1:10)))
ggsurvplot(km_fit, data = surv_data, size = 1,  # change line size
           linetype = "strata", # change line type by groups
           conf.int = TRUE, # Add confidence interval
           pval = TRUE, # Add p-value
           )

km_fit_1 <- survfit(Surv(time_life, status==1) ~ TKM$Gratwohl, data=surv_data)
summary(km_fit_1, times = c(1,30,60,90*(1:10)))
ggsurvplot(km_fit_1, data = surv_data, size = 1,  
           linetype = "strata", # change line type by groups
           palette = c("#fd0166"), # custom color palette
           conf.int = TRUE, # Add confidence interval 
           legend.title = "Patients",
           legend = c(0.1, 0.2),
           xlab = "Time in days")$plot +ggtitle("Kaplan-Meier survival curve")+theme(legend.text = element_text(size = 14, color = "black"),legend.title = element_text(size = 14, color = "black"))

#Запишем то, что получилось в  first_filter в табличку csv####
#write.table(x = first_filter, file = "data/first_filtered.csv",sep = ";",row.names = FALSE)

#если я ничего важного в нее не забыла положить, то дальше будем работать только с ней

