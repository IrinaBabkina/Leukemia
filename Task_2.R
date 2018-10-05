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

# График по фактору "время от диагноза до ТКМ" по 12
km_fit <- survfit(Surv(time_life, status==1) ~ TKM_group, data=surv_data)
summary(km_fit, times = c(1,30,60,90*(1:10)))

text<-fin_cum(km_fit)

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
  scale_y_continuous("Кумулятивная доля выживших",breaks = seq(0, 1, 0.1))+
  annotate("text", x = text[[4]]-20, y = text[[1]]+0.1, label = paste(text[[1]]*100,"%"))+
  annotate("text", x = text[[2]], y = text[[3]], label = paste(text[[3]]*100,"%"))#штуки в двойных квадратных скобках это коэфициенты, которые отвечают за положение "хвостика". Берутся из того листа, который возвращает функция fin_cum 

#График для общей выживаемости. 

km_fit_1 <- survfit(Surv(time_life, status==1) ~ 1, data=surv_data)
text<-fin_cum(km_fit_1)

ggsurvplot(km_fit_1, data = surv_data, size = 1,  
           linetype = "strata", # change line type by groups
           palette = c("#fd0166"), # custom color palette
           conf.int = TRUE, # Add confidence interval 
           legend.title = "Пациенты",
           legend = c(0.1, 0.2),
           xlab = "Время после TKM (мес)")$plot +ggtitle("Кривая выживаемости Каплана-Майера")+theme(legend.text = element_text(size = 14, color = "black"),legend.title = element_text(size = 14, color = "black"))+scale_x_continuous(breaks = seq(0, 300, 24)) +
  scale_y_continuous("Кумулятивная доля выживших",breaks = seq(0, 1, 0.1))+
  annotate("text", x = text[[2]]-20, y = text[[1]]+0.1, label = paste(text[[1]]*100,"%"))#штуки в двойных квадратных скобках это коэфициенты, которые отвечают за положение "хвостика". Берутся из того листа, который возвращает функция fin_cum 


