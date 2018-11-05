library(readxl)
library(dplyr)

#Загрузим сырые данные от Жени и Оксаны#####
raw_data<-read_excel(path = "data/db_31_10_2018.xls")

#Отберем те колонки, которые нам интересны
need_collumns<-c(2:18, 20:26, 52:65,72:74,76,81, 84, 89, 91:93, 122:125, 130)
third_filter<- raw_data[,need_collumns]

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
                "cGVHD_rate","Date_cGVHD","Therapy", "ITK_before_TKM",
                "Start_date", "End_date", "ITK_after_TKM")
colnames(third_filter)<-name_col_eng

#Дальше будем работать с новой табличкой
head(third_filter)
str(third_filter$ATG)

# Факторы в факторы. Не факторы оставляем####
third_filter[-c(5:9, 12:14, 16:17, 20:22, 24, 27:28, 35, 43, 45, 48, 51:52)] <- 
  lapply( third_filter[-c(5:9, 12:14, 16:17, 20:22, 24, 27:28, 35, 43, 45, 48, 51:52)], factor) 

#прелюдия, которая делает табличку человекочитаемой) Но ее нужно каждый раз делать
levels(third_filter$Gender)<-c("male","female")
levels(third_filter$Phaze_do_TKM) <- c("CP_1", "CP_2_3_4", "AP", "BC")
levels(third_filter$Donor_type)<- c("UR","R")
levels(third_filter$Relapse)<-c("no","yes")
levels(third_filter$Relapse_type) <- c("no_relapse","mol", "cyt", "hem")
levels(third_filter$Status_life)<-c("life", "died")
levels(third_filter$Donor_gender)<-c("male", "female")
levels(third_filter$Consistency)<-c("full", "particular","haplo")
levels(third_filter$Sourse_transp)<-c("periferia", "BM")
levels(third_filter$ABO_consist) <- c("identical","big", "small", "mixed")
levels(third_filter$'CD34>3')<-c(">3","<3")
levels(third_filter$CMV)<-c("not inf","d-r-","d+r-", "d+r-(G)","d-r+","d+r+", "d-r+(G)","d-r+(G)")
levels(third_filter$MAC_RIC) <- c("MAC", "RIC")
levels(third_filter$ATG)<-c("no","yes")
levels(third_filter$ATG_type) <- c("no","ATGAM", "Timo")
levels(third_filter$aGVHD) <- c("no", "yes")
levels(third_filter$cGVHD) <- c("no", "yes")
levels(third_filter$Therapy) <- c("no_ITK", "first_line", "second", "third")
levels(third_filter$ITK_before_TKM) <- c("no_ITK","ima", "daza","nilo", "bosy", "pona")
levels(third_filter$ITK_after_TKM) <- c("no", "yes")

#Поиск сомнительного####
str(third_filter)

third_filter$Age_diagn[1:10]#подозрительный дробный возраст. Видимо считали автоматом. ПЕРЕСЧИТАТЬ!!!
# надо бы навесить этикетки на уровни факторов (обсудить с Надей)

#Cколько у нас пациентов?####
nrow(third_filter)#всего 115

##########################################################################################

##################### ТОЛЬКО УНИКАЛЬНЫЕ
nrow(third_filter)#всего 115
ident<- paste(third_filter$Surname,third_filter$Name)
length(unique(ident))# 104 уникальных пациента (по имени-фамилии)
third_filter[duplicated(ident),]#. Не уникальные. С повторной ТКМ 11 человек
third_filter$ident<-ident

#отберем только тех, у кого ТМК была 1 раз
double<-filter(third_filter, Num_TKM==2)$ident #имена людей, у кого было несколько ТКМ

#запишем новую колонку в таблицу, которая будет маркировать людей с единственной ТКМ
third_filter$Only_one_TKM <- ifelse(third_filter$ident %in% double==T,"no","yes")

#Оставим только уникальных пациентов
third_filter <- subset(third_filter, third_filter$Only_one_TKM == 'yes')


#########################################################################################

##################### УНИКАЛЬНЫЕ С ОДНОЙ И ПОВТОРНОЙ ТКМ
# найдем повторяющиеся имена. Это те, кому делали ТКМ 2 раза в Гб 
# (кому только вторую, то у них нет 1 в базе и они уникальные тоже)
nrow(third_filter)#всего 115
ident<- paste(third_filter$Surname,third_filter$Name)
length(unique(ident))# 104 уникальных пациента (по имени-фамилии)
third_filter[duplicated(ident),]#. Не уникальные. С повторной ТКМ 11 человек
third_filter$ident<-ident
two_TKM<-third_filter[duplicated(third_filter$ident),]$ident

#сделаем переменную сортировщик
third_filter$Only_one_TKM <- ifelse(third_filter$ident %in% two_TKM,"no","yes")

#отберем по ней и получим 93 наблюдения (91 истинно единственная ТКМ и 2 с первой не в Гб)
sel<-third_filter[third_filter$Only_one_TKM == "yes",]

#разбираемся с повторными ТКМ####
two<-third_filter[third_filter$Only_one_TKM=="no",]#те, у кого была повторная
first<-two[two$Num_TKM =="1",]#только первая
second<-two[two$Num_TKM =="2",]#только вторая
all.equal(sort(first$ident),sort(second$ident))#да, это одни и те же люди. Никого не потеряли
tab<-cbind(as.character(first$ident), as.character(first$Status_life), 
           as.character(second$ident), as.character(second$Status_life))# всякие нестыковки в заполнении таблицы
names_life<-second[second$Status_life=="life","ident"]# имена живых после второй ТКМ
names_died<-second[second$Status_life=="died","ident"]#имена умерших после 2 ТКМ
f_for_s_life <- first[first$ident %in% names_life$ident,]#информация про первую у живых
f_for_s_died<-first[first$ident %in% names_died$ident,]#информация про первую у мертвых
f_for_s_life$Last_contact_date<-second[second$ident%in%names_life$ident,"Last_contact_date"]#меняем дату последнего контакта на дату второй ТКМ
f_for_s_died$Last_contact_date<-second[second$ident%in%names_died$ident,"Last_contact_date"]#меняем дату последнего контакта на дату смерти от второй ТКМ

third_filter <- rbind(sel,f_for_s_life, f_for_s_died)

###########################################################################################


#Считаем возраст на момент диагноза (в годах)####
library(lubridate)

elapsed.time <- third_filter$Born %--% third_filter$Diagn_date
age<-round(as.duration(elapsed.time) / dyears(1),1)
third_filter$Age_diagn<-age

#Считаем промежуток от диагноза до ТКМ в днях####
elapsed.time_1 <- third_filter$Diagn_date %--% third_filter$Date_alloTKM
Diagn_TKM <- round(as.duration(elapsed.time_1) / ddays(1))
third_filter$`Diagn-TKM_period(days)`<-Diagn_TKM

#Считаем промежуток от ТКМ до момента обновления базы. 
elapsed.time_2 <- third_filter$Date_alloTKM %--% third_filter$Data_base
TKM_base <- round(as.duration(elapsed.time_2) / ddays(1))

#Запишем то, что получилось в  third_filter в табличку csv####
write.table(x = third_filter, file = "data/third_filtered.csv",sep = ";",row.names = FALSE)
