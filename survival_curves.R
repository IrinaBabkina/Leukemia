library(readr)

third_filter <- read.csv("data/third_filtered.csv", header = T, sep = ";")

#Исключаем пациентов с ХФ1
third_filter <- third_filter[third_filter$Phaze_do_TKM != "CP_1",]
str(third_filter)

############################# ГРАФИКИ ########################################

surv_data <- data_frame(status = third_filter$Status_life, 
                        time_life = third_filter$TKM_death/30.5,
                        time_TKM = third_filter$`Diagn-TKM_period(days)`,
                        consistency = third_filter$Consistency,
                        mac_ric = third_filter$MAC_RIC)

####### ОБЩАЯ ВЫЖИВАЕМОСТЬ





######## Время от диагноза до ТКМ: до 12 мес, после 12 мес.

#Переведем время до ТКМ в бинарный фактор
surv_data <- surv_data %>% 
  mutate(TKM_group = ifelse(third_filter$`Diagn-TKM_period(days)` <= 365, "less", "further"))
surv_data$TKM_group <- factor(surv_data$TKM_group)

km_fit <- survfit(Surv(time_life, status=='died') ~ TKM_group, data=surv_data)
ggsurvplot(km_fit, data = surv_data, size = 1,  
           linetype = "strata", # change line type by groups
           #palette = c("#fd0166"), # custom color palette
           conf.int = TRUE, # Add confidence interval 
           legend.title = "Время от диагноза до ТКМ",
           legend.labs = c("> 12 мес", "< 12 мес"),
           break.y.by = 0.1,
           break.x.by = 24,
           xlim = c(0,120),
           legend = c(0.2, 0.1),
           ylab = "Кумулятивная выживаемость",
           xlab = "Время после TKM (мес)")$plot +
  ggtitle("Кривая выживаемости Каплана-Майера")+
  theme(legend.text = element_text(size = 14, color = "black"),
        legend.title = element_text(size = 14, color = "black"))

###################### Режим кондиционирования MAC/RIC.
km_fit_mac_ric <- survfit(Surv(time_life, status=='died') ~ mac_ric, data=surv_data)
ggsurvplot(km_fit_mac_ric, data = surv_data, size = 1,  
           linetype = "strata", # change line type by groups
           #palette = c("#fd0166"), # custom color palette
           conf.int = TRUE, # Add confidence interval 
           legend.title = "MAC / RIC",
           legend.labs = c("MAC", "RIC"),
           break.y.by = 0.1,
           break.x.by = 24,
           xlim = c(0,120),
           legend = c(0.1, 0.1),
           ylab = "Кумулятивная выживаемость",
           xlab = "Время после TKM (мес)")$plot +
  ggtitle("Кривая выживаемости Каплана-Майера")+
  theme(legend.text = element_text(size = 14, color = "black"),
        legend.title = element_text(size = 14, color = "black"))


#################### Совместимость - полная/частичная/гапло.
surv_consis <- third_filter[third_filter$Consistency != "haplo",]
surv_consis$Consistency <- factor(surv_consis$Consistency, levels = c("full", "particular"))
surv_data_cons <- data_frame(status = surv_consis$Status_life, 
                        time_life = surv_consis$TKM_death/30.5,
                        consistency = surv_consis$Consistency)
str(surv_consis)
km_fit_consis <- survfit(Surv(time_life, status=='died') ~ consistency, data=surv_data_cons)
ggsurvplot(km_fit_consis, data = surv_data_cons, size = 1,  
           linetype = "strata", # change line type by groups
           #palette = c("#fd0166"), # custom color palette
           conf.int = TRUE, # Add confidence interval 
           legend.title = "Совместимость",
           legend.labs = c("Полная", "Частичная"),
           break.y.by = 0.1,
           break.x.by = 24,
           xlim = c(0,120),
           legend = c(0.1, 0.1),
           ylab = "Кумулятивная выживаемость",
           xlab = "Время после TKM (мес)")$plot +
  ggtitle("Кривая выживаемости Каплана-Майера")+
  theme(legend.text = element_text(size = 14, color = "black"),
        legend.title = element_text(size = 14, color = "black"))


#############################3

3. Наличие острой РТПХ: нет, стадия 1-2, стадия 3-4. Один график общей выживаемости и один график - безрецидивной (событиями считается смерть и рецидив).

4. Наличие хронической РТПХ: нет, стадия 1-2, стадия 3-4. Один график общей выживаемости и один график - безрецидивной (событиями считается смерть и рецидив).

5. Наличие циклофосфана (Cy) в схеме профилактики РТПХ: есть/нет.
