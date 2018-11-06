library(readr)
library(dplyr)
library(ggplot2)
library(survival)
library(survminer)

third_filter <- read.csv("data/third_filtered.csv", header = T, sep = ";")

#Исключаем пациентов с ХФ1
third_filter <- third_filter[third_filter$Phaze_do_TKM != "CP_1",]

############################# ГРАФИКИ ########################################

surv_data <- data_frame(status = third_filter$Status_life, 
                        time_life = third_filter$TKM_death/30.5,
                        time_TKM = third_filter$`Diagn-TKM_period(days)`,
                        consistency = third_filter$Consistency,
                        mac_ric = third_filter$MAC_RIC)

######## Время от диагноза до ТКМ: до 12 мес, после 12 мес.

#Переведем время до ТКМ в бинарный фактор
surv_data <- surv_data %>% 
  mutate(TKM_group = ifelse(third_filter$`Diagn-TKM_period(days)` <= 365, "less", "further"))
surv_data$TKM_group <- factor(surv_data$TKM_group)

km_fit <- survfit(Surv(time_life, status=='died') ~ TKM_group, data=surv_data)
ggsurvplot(km_fit, data = surv_data, size = 1,  
           linetype = "strata", # change line type by groups
           #palette = c("#fd0166"), # custom color palette
           #conf.int = TRUE, # Add confidence interval 
           pval = TRUE,
           legend.title = "Время от диагноза до ТКМ",
           legend.labs = c("> 12 мес", "< 12 мес"),
           break.y.by = 0.1,
           break.x.by = 24,
           xlim = c(0,120),
           legend = c(0.2, 0.1),
           ylab = "Общая выживаемость",
           xlab = "Время после TKM (мес)")$plot+
  theme(legend.text = element_text(size = 14, color = "black"),
        legend.title = element_text(size = 14, color = "black"))

###################### Режим кондиционирования MAC/RIC.
km_fit_mac_ric <- survfit(Surv(time_life, status=='died') ~ mac_ric, data=surv_data)
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
           legend = c(0.1, 0.1),
           ylab = "Общая выживаемость",
           xlab = "Время после TKM (мес)")$plot+
  theme(legend.text = element_text(size = 14, color = "black"),
        legend.title = element_text(size = 14, color = "black"))


#################### Совместимость - полная/частичная/гапло.
km_fit_consis <- survfit(Surv(time_life, status=='died') ~ consistency, data=surv_data)
ggsurvplot(km_fit_consis, data = surv_data, size = 1,  
           linetype = "strata", # change line type by groups
           #palette = c("#fd0166"), # custom color palette
           #conf.int = TRUE, # Add confidence interval
           pval = TRUE,
           legend.title = "Совместимость",
           legend.labs = c("Полная", "Частичная", "Гапло"),
           break.y.by = 0.1,
           break.x.by = 24,
           xlim = c(0,120),
           legend = c(0.2, 0.1),
           ylab = "Общая выживаемость",
           xlab = "Время после TKM (мес)")$plot+
  theme(legend.text = element_text(size = 14, color = "black"),
        legend.title = element_text(size = 14, color = "black"))


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
           legend = c(0.8, 0.8),
           ylab = "Общая выживаемость",
           xlab = "Время после TKM (мес)")$plot+
  theme(legend.text = element_text(size = 14, color = "black"),
        legend.title = element_text(size = 14, color = "black"))

# Бессобытийная выживаемость
km_fit_aGVHD_ev <- survfit(Surv(time_event, event == 'yes') ~ aGVHD, data=surv_data_GVHD)
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
           legend = c(0.8, 0.8),
           ylab = "Безрецидивная выживаемость",
           xlab = "Время после TKM (мес)")$plot+
  theme(legend.text = element_text(size = 14, color = "black"),
        legend.title = element_text(size = 14, color = "black"))

################################ Хроническая РТПХ
km_fit_сGVHD <- survfit(Surv(time_life, status=='died') ~ cGVHD, data=surv_data_GVHD)
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
           legend = c(0.9, 0.2),
           ylab = "Общая выживаемость",
           xlab = "Время после TKM (мес)")$plot+
  theme(legend.text = element_text(size = 14, color = "black"),
        legend.title = element_text(size = 14, color = "black"))

# Бессобытийная выживаемость
km_fit_cGVHD_ev <- survfit(Surv(time_event, event == 'yes') ~ cGVHD, data=surv_data_GVHD)
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
           legend = c(0.8, 0.8),
           ylab = "Безрецидивная выживаемость",
           xlab = "Время после TKM (мес)")$plot+
  theme(legend.text = element_text(size = 14, color = "black"),
        legend.title = element_text(size = 14, color = "black"))

####################### Наличие циклофосфана (Cy) в схеме профилактики РТПХ: есть/нет.
km_fit_cy <- survfit(Surv(time_life, status=='died') ~ Cy, data=surv_data_GVHD)
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
           legend = c(0.8, 0.8),
           ylab = "Общая выживаемость",
           xlab = "Время после TKM (мес)")$plot+
  theme(legend.text = element_text(size = 14, color = "black"),
        legend.title = element_text(size = 14, color = "black"))
