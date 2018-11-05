library(dplyr)
library(ggplot2)
library(ggfortify)
library(survival)
library(readxl)
library(survMisc)
library(ggpubr)
library(magrittr)
library(survminer)

#Загрузим сырые данные от Жени и Оксаны#####
raw_data <- read_excel(path = "~/Bioinf_project/Autumn_project/db_31_10_2018.xls")

#Отберем те колонки, которые нам интересны
need_collumns<-c(2:18, 20:26, 52:65,72:74,76,81, 84, 89, 91:93, 114, 122:125, 130)
ev_free <- raw_data[,need_collumns]

#Переназовем отобранные колонки (иначе так замучаемся.. )####
name_col_eng<-c("Num_TKM","Surname","Name","Gender","Born","Growth", "Weight",
                "Diagn_date","Age_diagn","Phaze_do_TKM", "Number_BC_episodes", 
                "Date_alloTKM","Age_TKM","Diagn-TKM_period(days)", "Donor_type",
                "Restade_data","Last_contact_date", "Relapse","Relapse_type",
                "Date_mol_Relapse", "Date_cyt_Relapse", "Date_hem_Relapse",
                "Status_life","Data_base","Gratwohl", "Gratwohl_group",
                "Percent_blast","Donor_age","Donor_gender", "Consistency",
                "Sourse_transp","ABO_patient", "ABO_donor","ABO_consist","CD34",
                "CD34>3", "CMV","Condition","MAC_RIC","ATG","ATG_type",
                "GVHD_prophylactic","Engraftment","aGVHD", "Date_aGVHD","cGVHD",
                "cGVHD_rate","Date_cGVHD", "MAC_RIC_1","Therapy", "ITK_before_TKM",
                "Start_date", "End_date", "ITK_after_TKM")
colnames(ev_free)<-name_col_eng

str(ev_free)
# Факторы в факторы. Не факторы оставляем####
ev_free[-c(5:9,11:13,15:16,18:20, 22, 25,32, 37, 39, 42)] <- 
  lapply(ev_free[-c(5:9,11:13,15:16,18:20, 22, 25,32, 37, 39, 42)], factor) 


#Cколько у нас пациентов?####
nrow(ev_free)#всего 115
ident <- paste(ev_free$Surname,ev_free$Name)
length(unique(ident))# 104 уникальных пациента (по имени-фамилии)
ev_free[duplicated(ident),]#. Не уникальные. С повторной ТКМ 11 человек
ev_free$ident<-ident

#отберем только тех, у кого ТМК была 1 раз
double<-filter(ev_free, Num_TKM==2)$ident #имена людей, у кого было несколько ТКМ

#запишем новую колонку в таблицу, которая будет маркировать людей с единственной ТКМ
ev_free$Only_one_TKM <- ifelse(ev_free$ident %in% double==T,"no","yes")

#Оставим только уникальных пациентов
ev_free <- subset(ev_free, ev_free$Only_one_TKM == 'yes')

#Считаем возраст на момент диагноза (в годах)####
library(lubridate)

elapsed.time <- ev_free$Born %--% ev_free$Diagn_date
age<-round(as.duration(elapsed.time) / dyears(1),1)
ev_free$Age_diagn<-age

#Считаем промежуток от диагноза до ТКМ в днях####
elapsed.time_1 <- ev_free$Diagn_date %--% ev_free$Date_alloTKM
Diagn_TKM <- round(as.duration(elapsed.time_1) / ddays(1))
ev_free$`Diagn-TKM_period(days)`<-Diagn_TKM

#Считаем промежуток от ТКМ до последнего контакта в мес
elapsed.time_2 <- ev_free$Date_alloTKM %--% ev_free$Last_contact_date
TKM_cont <- round(as.duration(elapsed.time_2) / ddays(1))



####################################
# Бессобытийная выживаемость для событиев: смерть и рецидив


#уберем значения NA из нужных нам колонок
ev_free <- ev_free[complete.cases(ev_free$Relapse),]
ev_free <- ev_free[complete.cases(ev_free$Status_life),]

#Создаем новый дата фрейм с датами разных рецидивов и выбираем оттуда минимиальную дату
# Которую записываем в новый столбец
new_df <- cbind(ev_free$Date_mol_Relapse, ev_free$Date_cyt_Relapse, ev_free$Date_hem_Relapse)
ev_free$Date_Relapse <- apply(new_df, 1, min, na.rm = T)
ev_free$Date_Relapse <- as.POSIXct(ev_free$Date_Relapse,tz = "GMT", origin)

#делаем 2 колонки, в которых совмещаем события и выбираем подходящую дату
for (i in 1:nrow(ev_free)){
  if ((ev_free$Relapse[i] == 1) && (ev_free$Status_life[i] == 1)){
    ev_free$event[i] <- 1
    ev_free$Date_event[i] <- as.Date(ev_free$Date_Relapse[i])
  }else if(ev_free$Status_life[i] == 1 && (ev_free$Relapse[i] == 0)){
    ev_free$event[i] <- 1
    ev_free$Date_event[i] <- as.Date(ev_free$Last_contact_date[i])
  }else if(ev_free$Status_life[i] == 0 && (ev_free$Relapse[i] == 1)){
    ev_free$event[i] <- 1
    ev_free$Date_event[i] <- as.Date(ev_free$Date_Relapse[i])
  }else{
    ev_free$event[i] <- 0
    ev_free$Date_event[i] <- as.Date(ev_free$Data_base[i])
  }
}
#переводим числа в даты. почему-то показывает, что NA у нас нет, но такой один случай есть
# поэтому удаляем его вручную
library(zoo)
ev_free$Date_event <- as.Date(ev_free$Date_event)
ev_free <- ev_free[-54,]

#Считаем промежуток от ТКМ до времени произошедшего события
elapsed.time_event <- ev_free$Date_alloTKM %--% ev_free$Date_event
TKM_event <- round(as.duration(elapsed.time_event) / ddays(1))

#Можно начинать возиться с выживаемостью))####
library(survival)
library(survminer)

#Соберем это в один дата фрейм для удобства
surv_data <- data_frame(status = ev_free$event, 
                        time_base = TKM_event/30.5)
#Ищем, что за последний случай, который дал нам 0% в конйе кривой
max(TKM_event/30.5)
m <- ev_free[68,]

#Уберем наблюдения с NA
surv_data <- surv_data[complete.cases(surv_data),]

#Функция для рассчета координат "хвостика". На вход модель, на выход координаты.
fin_cum<-function(km_fit){
  if (length(km_fit$n)==1) {
    list(round(km_fit$surv[length(km_fit$surv)],3),
         max(km_fit$time))
  } else {
    s_f<-summary(km_fit)
    strata <- names(km_fit$strata)
    sapply(strata, function(x){data.frame(cum = round(min(km_fit$surv[s_f$strata==x]),3),
                                          nr = max(km_fit$time[s_f$strata==x]))})
  }
}

#Нам нужно рассчитать общую выживаемость в зависимости от времени до ТКМ
# status==1 принципиальная вещь, которая стоила мне пару часов жизни)) 
#Инвертирует то, что у врачей живой обозначен 0, а мертвый 1
km_fit <- survfit(Surv(time_base, status==1) ~ 1, data=surv_data)
text<-fin_cum(km_fit)
ggsurvplot(km_fit, data = surv_data, size = 1,  
           linetype = "strata", # change line type by groups
           palette = c("#fd0166"), # custom color palette
           conf.int = TRUE, # Add confidence interval 
           legend.title = "Пациенты",
           legend.labs = "Все",
           break.y.by = 0.1,
           break.x.by = 24,
           xlim = c(0,220),
           legend = c(0.1, 0.2),
           ylab = "Бессобытийная выживаемость",
           xlab = "Время после TKM (мес)")$plot +
  ggtitle("Кривая выживаемости Каплана-Майера")+
  theme(legend.text = element_text(size = 14, color = "black"),
        legend.title = element_text(size = 14, color = "black"))+
  annotate("text", x = text[[2]]-20, y = text[[1]]+0.1, 
           label = paste(text[[1]]*100,"%"))#штуки в двойных квадратных скобках это коэфициенты, которые отвечают за положение "хвостика". Берутся из того листа, который возвращает функция fin_cum 
ggtheme = theme_bw()





####################################
# Бессобытийная выживаемость для событиев: смерть, рецидив, неприживление и хРТПХ 3-4 стадии

# надо посмотреть, в каких формулировках записано неприживление:
as.factor(ev_free$Engraftment)

#так как есть NA  в Engraftment, то заменяем их на 0, чтобы не было затем проблем
ev_free$Engraftment[is.na(ev_free$Engraftment)] <- 0

# 2 вариант: неприживление и 1-е неприживление. И создаем таблицу с рецидивом и РТПХ, чтобы
# выбрать потом минимальную дату
date_event_df <- cbind(ev_free$Date_Relapse, ev_free$GVHD_date)
ev_free$min_date <- apply(date_event_df, 1, min, na.rm = T)
ev_free$min_date <- as.POSIXct(ev_free$min_date,tz = "GMT", origin)

#создаем отдельно столбик с 1, у кого РТПХ считать за событие. ТО есть 3-4 стадии хРТПХ
for (i in 1:nrow(ev_free)){
  if ((ev_free$cGVHD_rate[i] == 3) | (ev_free$cGVHD_rate[i] == 4)){
    ev_free$GVHD[i] <- 1
    ev_free$GVHD_date[i] <- ev_free$Date_cGVHD[i]
  }else{
    ev_free$GVHD[i] <- 0
    ev_free$GVHD_date[i] <- NA
  }
}
ev_free$GVHD_date <- as.POSIXct(ev_free$GVHD_date,tz = "GMT", origin)

str(ev_free)

#делаем 2 колонки, в которых совмещаем события и выбираем подходящую дату
for (i in 1:nrow(ev_free)){
  if ((ev_free$Engraftment[i] == 'неприживление') | (ev_free$Engraftment[i] == '1-е неприживление')){
    ev_free$event[i] <- 1
    ev_free$Date_event_all[i] <- (as.Date(ev_free$Date_alloTKM[i]) + 30)
  }else if(ev_free$GVHD[i] == 1 | (ev_free$Relapse[i] == 1)){
    ev_free$event[i] <- 1
    ev_free$Date_event_all[i] <- as.Date(ev_free$min_date[i])
  }else if(ev_free$Status_life[i] == 1){
    ev_free$event[i] <- 1
    ev_free$Date_event_all[i] <- as.Date(ev_free$Last_contact_date[i])
  }else{
    ev_free$event[i] <- 0
    ev_free$Date_event_all[i] <- as.Date(ev_free$Data_base[i])
  }
}
# не забываем перести в дату
ev_free$Date_event_all <- as.Date(ev_free$Date_event_all)


#Считаем промежуток от ТКМ до времени произошедшего события
elapsed.time_event_all <- ev_free$Date_alloTKM %--% ev_free$Date_event_all
TKM_event_all <- round(as.duration(elapsed.time_event_all) / ddays(1))

# добавим к основному дата фрейму время от ткм до события, чтобы посмотреть, как дела у наших
# пациентов с РТПХ и неприживлением
ev_free$time <- TKM_event_all
mm <- ev_free[ev_free$GVHD == 1 | ev_free$Engraftment == "неприживление" | 
                ev_free$Engraftment == '1-е неприживление', ]

#Можно начинать возиться с выживаемостью))####
library(survival)
library(survminer)

#Соберем это в один дата фрейм для удобства
surv_data_all <- data_frame(status = ev_free$event, 
                        time_event = TKM_event_all/30.5)
#Ищем, что за последний случай, который дал нам 0% в конйе кривой
max(TKM_event/30.5)
m <- ev_free[68,]

#Уберем наблюдения с NA
surv_data_all <- surv_data_all[complete.cases(surv_data_all),]
surv_data_all <- surv_data_all[-54,]

#Функция для рассчета координат "хвостика". На вход модель, на выход координаты.
fin_cum<-function(km_fit){
  if (length(km_fit$n)==1) {
    list(round(km_fit$surv[length(km_fit$surv)],3),
         max(km_fit$time))
  } else {
    s_f<-summary(km_fit)
    strata <- names(km_fit$strata)
    sapply(strata, function(x){data.frame(cum = round(min(km_fit$surv[s_f$strata==x]),3),
                                          nr = max(km_fit$time[s_f$strata==x]))})
  }
}

#Нам нужно рассчитать общую выживаемость в зависимости от времени до ТКМ
# status==1 принципиальная вещь, которая стоила мне пару часов жизни)) 
#Инвертирует то, что у врачей живой обозначен 0, а мертвый 1
km_fit_all <- survfit(Surv(time_event, status==1) ~ 1, data=surv_data_all)
text<-fin_cum(km_fit_2)
ggsurvplot(km_fit_2, data = surv_data_2, size = 1,  
           linetype = "strata", # change line type by groups
           palette = c("#fd0166"), # custom color palette
           conf.int = TRUE, # Add confidence interval 
           legend.title = "Пациенты",
           legend.labs = "Все",
           break.y.by = 0.1,
           break.x.by = 36,
           legend = c(0.1, 0.2),
           surv.median.line = "hv",
           ylab = "Бессобытийная выживаемость",
           xlab = "Время после TKM (мес)")$plot +
  ggtitle("Кривая выживаемости Каплана-Майера")+
  theme(legend.text = element_text(size = 14, color = "black"),
        legend.title = element_text(size = 14, color = "black"))+
  annotate("text", x = text[[2]]-20, y = text[[1]]+0.1, 
           label = paste(text[[1]]*100,"%"))#штуки в двойных квадратных скобках это коэфициенты, которые отвечают за положение "хвостика". Берутся из того листа, который возвращает функция fin_cum 


ggtheme = theme_bw()