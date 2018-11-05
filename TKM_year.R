first_filter<-read.csv("~/Bioinf_project/Autumn_project/Leukemia/data/first_filtered.csv", sep=";")
str(data)

library(lubridate)
library(dplyr)

#Считаем промежуток от ТКМ до момента последнего контакта 
elapsed.time_2 <- first_filter$Date_alloTKM %--% first_filter$Last_contact_date
TKM_cont <- round(as.duration(elapsed.time_2) / ddays(1))

#Можно начинать возиться с выживаемостью))####
library(survival)
library(survminer)

#Соберем это в один дата фрейм для удобства
surv_data<-data_frame(status=first_filter$Status_life, 
                      time_life=TKM_cont/30.5,
                      time_TKM=first_filter$Diagn.TKM_period.days./30.5,
                      phase_do_TKM=first_filter$Phaze_do_TKM,
                      n_TKM=first_filter$Only_one_TKM,
                      date_TKM = first_filter$Date_alloTKM)

#отберем тех, у кого ТКМ была 1 раз
surv_data<-filter(surv_data, n_TKM=="yes")
#Уберем наблюдения с NA
surv_data<-surv_data[complete.cases(surv_data),]
str(surv_data)

surv_data$date_TKM <- as.POSIXct(surv_data$date_TKM)

#Переведем время до ТКМ в бинарный фактор через медианное значение
surv_data <- surv_data %>% mutate(TKM_group = ifelse(as.numeric(date_TKM) <= as.numeric(as.POSIXct('2012-01-01')),
                                                     "less", "further"))
surv_data$TKM_group <- factor(surv_data$TKM_group)

#Нам нужно рассчитать общую выживаемость в зависимости от времени до ТКМ
# status==1 принципиальная вещь, которая стоила мне пару часов жизни)) Инвертирует то, что у врачей живой обозначен 0, а мертвый 1

# График по фактору "время от диагноза до ТКМ" по 12
km_fit <- survfit(Surv(time_life, status==1) ~ TKM_group, data=surv_data, conf.type = "log-log")
summary(km_fit, times = c(1,30,60,90*(1:10)))

ggsurvplot(km_fit, data = surv_data, size = 1,  # change line size
           linetype = "strata", # change line type by groups
           palette = c("#E7B800", "#2E9FDF"), # custom color palette
           conf.int = TRUE, # Add confidence interval
           pval = T,
           xlim = c(0, 120),
           break.y.by = 0.1,
           break.x.by = 12,
           cumcensor = TRUE,
           legend = c(0.8, 0.8),
           #legend = "bottom", 
           legend.title = "Дата проведения ТКМ",
           legend.labs = c("С 2012 года", "До 2012 года"))$plot
