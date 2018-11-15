library(lubridate)
library(dplyr)


data <- read.table("data/third_filtered.csv", header = T, sep = ";")

data[c(5,8,12,16:17,20:22, 24, 46, 49, 52:53, 57, 59, 61)] <- 
  lapply(data[c(5,8,12,16:17,20:22, 24, 46, 49, 52:53, 57, 59, 61)], as.POSIXct) 

#Можно начинать возиться с выживаемостью))####
library(survival)
library(survminer)

#Соберем это в один дата фрейм для удобства
surv_data<-data_frame(status=data$Status_life, 
                      time_life=data$TKM_death/30.5)


#Переведем время ТКМ в фактор
surv_data <- surv_data %>% mutate(TKM_date_factor = ifelse(as.numeric(data$Date_alloTKM) < as.numeric(as.POSIXct('2006-01-01')),
                                                     "2006", ifelse(as.numeric(data$Date_alloTKM) >= as.numeric(as.POSIXct('2012-01-01')),
                                                                    "2012", "2006-2012")))
surv_data$TKM_date_factor <- factor(surv_data$TKM_date_factor)

# График по фактору "время от диагноза до ТКМ" по 12
km_fit <- survfit(Surv(time_life, status=='died') ~ TKM_date_factor, data=surv_data)
summary(km_fit, times = c(1,30,60,90*(1:10)))

ggsurvplot(km_fit, data = surv_data, size = 1,  # change line size
           linetype = "strata", # change line type by groups
           pval.method = FALSE,
           pval = T,
           pval.size=8,
           xlim = c(0, 120),
           break.y.by = 0.1,
           cumcensor = FALSE,
           #ggtheme = theme_minimal(),
           risk.table.y.text.col = TRUE,
           risk.table.y.text = FALSE,
           test.for.trend = FALSE,
           xlab = "Время (мес)",
           ylab = "Кумулятивная доля выживших",
           censor.size=9,
           legend = c(0.8,0.9),
           font.legend=18,
           size=2,
           font.x=20,
           font.y=20,
           font.tickslab=c(16,"plain"),
           break.x.by = 24,
           legend.title = "Дата проведения ТКМ",
           legend.labs = c("до 2006 года", "с 2006 до 2012 года", "с 2012 года"))$plot
