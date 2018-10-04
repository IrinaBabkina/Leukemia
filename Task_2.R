first_filter<-read.csv("data/first_filtered.csv", sep=";")
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
                      n_TKM=first_filter$Only_one_TKM)

#отберем тех, у кого ТКМ была 1 раз
surv_data<-filter(surv_data, n_TKM=="yes")
#Уберем наблюдения с NA
surv_data<-surv_data[complete.cases(surv_data),]

#Переведем время до ТКМ в бинарный фактор через медианное значение
surv_data <- surv_data %>% mutate(TKM_group = ifelse(time_TKM >=12, "less", "further"))
surv_data$TKM_group <- factor(surv_data$TKM_group)

#Нам нужно рассчитать общую выживаемость в зависимости от времени до ТКМ
# status==1 принципиальная вещь, которая стоила мне пару часов жизни)) Инвертирует то, что у врачей живой обозначен 0, а мертвый 1
# График по фактору "время от диагноза до ТКМ" по 12
km_fit <- survfit(Surv(time_life, status==1) ~ TKM_group, data=surv_data)
summary(km_fit, times = c(1,30,60,90*(1:10)))

ggsurvplot(km_fit, data = surv_data, size = 1,  # change line size
           linetype = "strata", # change line type by groups
           palette = c("#E7B800", "#2E9FDF"), # custom color palette
           conf.int = TRUE, # Add confidence interval
           pval = T,
           cumcensor = TRUE,
           legend = "bottom", 
           legend.title = "Период от диагностики до TKM",
           legend.labs = c("Больше 12 месяцев", "Меньше 12 месяцев"))$plot+
  scale_x_continuous("Время после TKM (мес)",breaks = seq(0, 300, 24)) +
  scale_y_continuous("Кумулятивная доля выживших",breaks = seq(0, 1, 0.1))


km_fit_1 <- survfit(Surv(time_life, status==1) ~ 1, data=surv_data)
proz<-round(km_fit_1$surv[length(km_fit_1$surv)],3)

ggsurvplot(km_fit_1, data = surv_data, size = 1,  
           linetype = "strata", # change line type by groups
           palette = c("#fd0166"), # custom color palette
           conf.int = TRUE, # Add confidence interval 
           legend.title = "Пациенты",
           legend = c(0.1, 0.2),
           xlab = "Время после TKM (мес)")$plot +ggtitle("Кривая выживаемости Каплана-Майера")+theme(legend.text = element_text(size = 14, color = "black"),legend.title = element_text(size = 14, color = "black"))+scale_x_continuous(breaks = seq(0, 300, 24)) +
  scale_y_continuous("Кумулятивная доля выживших",breaks = seq(0, 1, 0.1))+
  annotate("text", x = max(surv_data$time_life)+20, y = proz, label = paste(proz*100,"%"))



