library(readr)
library(dplyr)
library(ggplot2)
library(survival)
library(survminer)

third_filter <- read.table("data/third_filtered.csv", header = T, sep = ";")

third_filter[c(5,8,12,16:17,20:22, 24, 46, 49, 52:53, 57, 59, 61)] <- 
  lapply(third_filter[c(5,8,12,16:17,20:22, 24, 46, 49, 52:53, 57, 59, 61)], as.POSIXct) 

library(lubridate)

#Считаем промежуток от диагноза до ТКМ в днях####
elapsed.time_1 <- third_filter$Diagn_date %--% third_filter$Date_alloTKM
Diagn_TKM <- round(as.duration(elapsed.time_1) / ddays(1))
third_filter$`Diagn-TKM_period(days)`<-Diagn_TKM

#Считаем промежуток от ТКМ до события/последнего контакта
logic<-third_filter$Status_life == 'died'
third_filter$Date_death[logic]<-third_filter$Last_contact_date[logic]
third_filter$Date_death[!logic]<-third_filter$Data_base[!logic]

elapsed.time_2 <- third_filter$Date_alloTKM %--% third_filter$Date_death
third_filter$TKM_death <- round(as.duration(elapsed.time_2) / ddays(1))

#Функция для рассчета координат "хвостика". На вход модель, на выход координаты.

fin_cum<-function(km_fit){
  if (length(km_fit$n)==1) {
    list(round(km_fit$surv[length(km_fit$surv)],3),
         max(km_fit$time))
  } else {
    s_f<-summary(km_fit)
    strata <- names(km_fit$strata)
    sapply(strata, function(x){data.frame(cum = round(min(s_f$surv[s_f$strata==x]),3),
                                          nr = max(km_fit$time[s_f$strata==x]))})
  }
}

#Исключаем пациентов с ХФ1
third_filter <- third_filter[third_filter$Phaze_do_TKM != "CP_1",]

############################# ГРАФИКИ ########################################

surv_data <- data_frame(status = third_filter$Status_life, 
                        time_life = third_filter$TKM_death/30.5,
                        time_TKM = third_filter$`Diagn.TKM_period.days.`,
                        consistency = third_filter$Consistency,
                        mac_ric = third_filter$MAC_RIC)

######## Время от диагноза до ТКМ: до 12 мес, после 12 мес.

#Переведем время до ТКМ в бинарный фактор
surv_data <- surv_data %>% 
  mutate(TKM_group = ifelse(third_filter$`Diagn.TKM_period.days.` <= 365, "less", "further"))
surv_data$TKM_group <- factor(surv_data$TKM_group)

km_fit <- survfit(Surv(time_life, status=='died') ~ TKM_group, data=surv_data)
text<-fin_cum(km_fit)
ggsurvplot(km_fit, data = surv_data, size = 1,  
           linetype = "strata",
           pval = TRUE,
           legend.title = "Время от диагноза до ТКМ",
           legend.labs = c("Более 12 мес", "До 12 мес включительно"),
           break.y.by = 0.1,
           break.x.by = 24,
           xlim = c(0,120),
           font.legend=18,
           size=2,
           font.x=20,
           font.y=20,
           font.tickslab=c(16,"plain"),
           legend = c(0.8,0.9),
           ylab = "Общая выживаемость",
           xlab = "Время после TKM (мес)")$plot+
annotate("text", x = text[[4]]+90, y = text[[1]]+0.1, label = paste(text[[1]]*100,"%"))+
  annotate("text", x = text[[2]]-60, y = text[[3]]+0.05, label = paste(text[[3]]*100,"%"))#штуки в двойных квадратных скобках это коэфициенты, которые отвечают за положение "хвостика". Берутся из того листа, который возвращает функция fin_cum 


###################### Режим кондиционирования MAC/RIC.
km_fit_mac_ric <- survfit(Surv(time_life, status=='died') ~ mac_ric, data=surv_data)
text<-fin_cum(km_fit_mac_ric)
ggsurvplot(km_fit_mac_ric, data = surv_data, size = 1,  
           linetype = "strata", # change line type by groups
           #palette = c("#fd0166"), # custom color palette
           #conf.int = TRUE, # Add confidence interval 
           pval = TRUE,
           legend.title = "MAC / RIC",
           legend.labs = c("MAC", "RIC"),
           break.y.by = 0.1,
           break.x.by = 24,
           xlim = c(0,120),
           legend = c(0.8,0.9),
           font.legend=18,
           size=2,
           font.x=20,
           font.y=20,
           font.tickslab=c(16,"plain"),
           ylab = "Общая выживаемость",
           xlab = "Время после TKM (мес)")$plot+annotate("text", x = text[[4]], y = text[[1]]+0.1, label = paste(text[[1]]*100,"%"))+
  annotate("text", x = text[[2]]-30, y = text[[3]]+0.05, label = paste(text[[3]]*100,"%"))#штуки в двойных квадратных скобках это коэфициенты, которые отвечают за положение "хвостика". Берутся из того листа, который возвращает функция fin_cum 


#################### Совместимость - полная/частичная/гапло.
km_fit_consis <- survfit(Surv(time_life, status=='died') ~ consistency, data=surv_data)
text<-fin_cum(km_fit_consis)
ggsurvplot(km_fit_consis, data = surv_data, size = 1,  
           linetype = "strata", # change line type by groups
           #palette = c("#fd0166"), # custom color palette
           #conf.int = TRUE, # Add confidence interval
           pval = TRUE,
           legend.title = "Совместимость",
           legend.labs = c("Полная", "Гапло", "Частичная"),
           break.y.by = 0.1,
           break.x.by = 24,
           xlim = c(0,120),
           legend = c(0.8,0.8),
           font.legend=18,
           size=2,
           font.x=20,
           font.y=20,
           font.tickslab=c(16,"plain"),
           ylab = "Общая выживаемость",
           xlab = "Время после TKM (мес)")$plot+
  annotate("text", x = 120, y =0.414+0.1, label = paste(0.414*100,"%")) +
  annotate("text", x = 45, y =1, label = paste(100,"%"))+
  annotate("text", x = 72, y =0, label = paste(0,"%"))

############################# Создадим дата фрейм для РТПХ и профилактики РТПХ
surv_data_GVHD <- data_frame(status = third_filter$Status_life, 
                             time_life = third_filter$TKM_death/30.5,
                             time_event = third_filter$TKM_event/30.5,
                             event = third_filter$event,
                             aGVHD_rate = third_filter$aGVHD_rate,
                             cGVHD_rate = third_filter$cGVHD_rate,
                             prophylactic = third_filter$GVHD_prophylactic)
str(surv_data_GVHD)
levels(surv_data_GVHD$prophylactic)
surv_data_GVHD$Cy <- ifelse(surv_data_GVHD$prophylactic %in% c("Cy_CsA", "CsA_MTX_Cy","CsA_MMF_Cy", "Tx_Cy", 
                                          "Tx_MMF_Cy", "Tx_MTX_Cy", "Cy", "Су"), "yes", "no")
surv_data_GVHD$aGVHD <- ifelse(surv_data_GVHD$aGVHD_rate == 0, "no", 
                               ifelse(surv_data_GVHD$aGVHD_rate %in% c(1,2), "1_2_stages", "3_4_stages"))

surv_data_GVHD$cGVHD <- ifelse(surv_data_GVHD$cGVHD_rate == 0, "no", 
                               ifelse(surv_data_GVHD$cGVHD_rate %in% c(1,2), "1_2_stages", "3_4_stages"))

############################# острая РТПХ
km_fit_aGVHD <- survfit(Surv(time_life, status=='died') ~ aGVHD, data=surv_data_GVHD)
text<-fin_cum(km_fit_aGVHD)
ggsurvplot(km_fit_aGVHD, data = surv_data_GVHD, size = 1,  
           linetype = "strata", # change line type by groups
           #palette = c("#fd0166"), # custom color palette
           #conf.int = TRUE, # Add confidence interval
           pval = TRUE,
           legend.title = "острая РТПХ",
           legend.labs = c("1-2 стадии", "3-4 стадии", "нет оРТПХ"),
           break.y.by = 0.1,
           break.x.by = 24,
           xlim = c(0,120),
           legend = c(0.8,0.8),
           font.legend=18,
           size=2,
           font.x=20,
           font.y=20,
           font.tickslab=c(16,"plain"),
           ylab = "Общая выживаемость",
           xlab = "Время после TKM (мес)")$plot+annotate("text", x = text[[6]]+30, y = text[[1]], label = paste(text[[5]]*100,"%"))+
  annotate("text", x = text[[2]]-70, y = text[[3]], label = paste(text[[3]]*100,"%"))+ annotate("text", x = text[[4]], y = text[[5]]+0.15, label = paste(text[[1]]*100,"%"))#штуки в двойных квадратных скобках это коэфициенты, которые отвечают за положение "хвостика". Берутся из того листа, который возвращает функция fin_cum 


# Безрецидивная выживаемость
km_fit_aGVHD_ev <- survfit(Surv(time_event, event == 'yes') ~ aGVHD, data=surv_data_GVHD)
text<-fin_cum(km_fit_aGVHD_ev)
ggsurvplot(km_fit_aGVHD_ev, data = surv_data_GVHD, size = 1,  
           linetype = "strata", # change line type by groups
           #palette = c("#fd0166"), # custom color palette
           #conf.int = TRUE, # Add confidence interval
           pval = TRUE,
           legend.title = "острая РТПХ",
           legend.labs = c("1-2 стадии", "3-4 стадии", "нет оРТПХ"),
           break.y.by = 0.1,
           break.x.by = 24,
           xlim = c(0,120),
           legend = c(0.8,0.8),
           font.legend=18,
           size=2,
           font.x=20,
           font.y=20,
           font.tickslab=c(16,"plain"),
           ylab = "Безрецидивная выживаемость",
           xlab = "Время после TKM (мес)")$plot+annotate("text", x = text[[2]]+110, y = text[[5]]+0.1, label = paste(text[[5]]*100,"%"))+
  annotate("text", x = text[[4]]+10, y = text[[3]]+0.05, label = paste(text[[3]]*100,"%"))+ annotate("text", x = text[[6]]-80, y = text[[1]]+0.1, label = paste(text[[1]]*100,"%"))#штуки в двойных квадратных скобках это коэфициенты, которые отвечают за положение "хвостика". Берутся из того листа, который возвращает функция fin_cum 

################################ Хроническая РТПХ
km_fit_сGVHD <- survfit(Surv(time_life, status=='died') ~ cGVHD, data=surv_data_GVHD)
text<-fin_cum(km_fit_сGVHD)
ggsurvplot(km_fit_сGVHD, data = surv_data_GVHD, size = 1,  
           linetype = "strata", # change line type by groups
           #palette = c("#fd0166"), # custom color palette
           #conf.int = TRUE, # Add confidence interval
           pval = TRUE,
           legend.title = "хроническая РТПХ",
           legend.labs = c("1-2 стадии", "3-4 стадии", "нет хРТПХ"),
           break.y.by = 0.1,
           break.x.by = 24,
           xlim = c(0,120),
           legend = c(0.8,0.9),
           font.legend=18,
           size=2,
           font.x=20,
           font.y=20,
           font.tickslab=c(16,"plain"),
           ylab = "Общая выживаемость",
           xlab = "Время после TKM (мес)")$plot+
  annotate("text", x = text[[4]]+70, y = text[[3]]+0.05, label = paste(text[[3]]*100,"%"))+ annotate("text", x = text[[6]]-40, y = text[[1]]-0.05, label = paste(text[[1]]*100,"%"))+annotate("text", x = text[[2]]+5, y = text[[5]], label = paste(text[[5]]*100,"%"))#штуки в двойных квадратных скобках это коэфициенты, которые отвечают за положение "хвостика". Берутся из того листа, который возвращает функция fin_cum 


# Безрецидивная выживаемость
km_fit_cGVHD_ev <- survfit(Surv(time_event, event == 'yes') ~ cGVHD, data=surv_data_GVHD)
text<-fin_cum(km_fit_cGVHD_ev)
ggsurvplot(km_fit_cGVHD_ev, data = surv_data_GVHD, size = 1,  
           linetype = "strata", # change line type by groups
           #palette = c("#fd0166"), # custom color palette
           #conf.int = TRUE, # Add confidence interval
           pval = TRUE,
           legend.title = "хроническая РТПХ",
           legend.labs = c("1-2 стадии", "3-4 стадии", "нет хРТПХ"),
           break.y.by = 0.1,
           break.x.by = 24,
           xlim = c(0,120),
           legend = c(0.8,0.8),
           font.legend=18,
           size=2,
           font.x=20,
           font.y=20,
           font.tickslab=c(16,"plain"),
           ylab = "Безрецидивная выживаемость",
           xlab = "Время после TKM (мес)")$plot+annotate("text", x = text[[2]], y = text[[3]]+0.05, label = paste(text[[3]]*100,"%"))+ annotate("text", x = text[[6]]-40, y = text[[1]]+0.1, label = paste(text[[1]]*100,"%"))+annotate("text", x = text[[4]]+75, y = text[[5]], label = paste(text[[5]]*100,"%"))#штуки в двойных квадратных скобках это коэфициенты, которые отвечают за положение "хвостика". Берутся из того листа, который возвращает функция fin_cum 


####################### Наличие циклофосфана (Cy) в схеме профилактики РТПХ: есть/нет.
km_fit_cy <- survfit(Surv(time_life, status=='died') ~ Cy, data=surv_data_GVHD)
text<-fin_cum(km_fit_cy)
ggsurvplot(km_fit_cy, data = surv_data_GVHD, size = 1,  
           linetype = "strata", # change line type by groups
           #palette = c("#fd0166"), # custom color palette
           #conf.int = TRUE, # Add confidence interval
           pval = TRUE,
           legend.title = "Наличие циклофосфана",
           legend.labs = c("Нет", "Есть"),
           break.y.by = 0.1,
           break.x.by = 24,
           xlim = c(0,120),
           legend = c(0.8,0.9),
           font.legend=18,
           size=2,
           font.x=20,
           font.y=20,
           font.tickslab=c(16,"plain"),
           ylab = "Общая выживаемость",
           xlab = "Время после TKM (мес)")$plot+annotate("text", x = text[[4]]-30, y = text[[1]]+0.1, label = paste(text[[1]]*100,"%"))+
  annotate("text", x = text[[2]], y = text[[3]]+0.05, label = paste(text[[3]]*100,"%"))#штуки в двойных квадратных скобках это коэфициенты, которые отвечают за положение "хвостика". Берутся из того листа, который возвращает функция fin_cum 


km_fit_mac_ric <- survfit(Surv(time_life, status=='died') ~ 1, data=surv_data)
text<-fin_cum(km_fit_mac_ric)
ggsurvplot(km_fit_mac_ric, data = surv_data, size = 1,  
           #palette = c("#fd0166"), # custom color palette
           conf.int = FALSE, 
           pval = TRUE,
           break.y.by = 0.1,
           break.x.by = 24,
           xlim = c(0,120),
           legend = "none",
           font.legend=18,
           size=2,
           font.x=20,
           font.y=20,
           font.tickslab=c(16,"plain"),
           ylab = "Общая выживаемость",
           xlab = "Время после TKM (мес)")$plot+annotate("text", x = text[[2]]-30, y = text[[1]]+0.1, label = paste(text[[1]]*100,"%"))
