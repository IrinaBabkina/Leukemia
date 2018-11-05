library(dplyr)
library(ggplot2)
data<-read.csv("data/second_filtered.csv", sep=" ")
nrow(data)
#уникальные пациенты 104. Столько должно получиться строк в результирующей таблице. 
length(unique(data$ident))

# разбираемся с единств ТКМ####
#сделаем табличку из тех, у кого единственная ткм (включая двух не из Гб)
two_TKM<-data[duplicated(data$ident),]$ident# найдем повторяющиеся имена. Это те, кому делали ТКМ 2 раза в Гб (кому только вторую, то у них нет 1 в базе и они уникальные тоже)
data$Only_one_TKM <- ifelse(data$ident %in% two_TKM,"no","yes")#сделаем переменную сортировщик
sel<-data[data$Only_one_TKM == "yes",]#отберем по ней и получим 93 наблюдения (91 истинно единственная ТКМ и 2 с первой не в Гб)

#разбираемся с повторными ТКМ####
#
two<-data[data$Only_one_TKM=="no",]#те, у кого была повторная
first<-two[two$Num_TKM =="1",]#только первая
second<-two[two$Num_TKM =="2",]#только вторая
all.equal(sort(first$ident),sort(second$ident))#да, это одни и те же люди. Никого не потеряли
tab<-cbind(as.character(first$ident), as.character(first$Status_life), as.character(second$ident), as.character(second$Status_life))# всякие нестыковки в заполнении таблицы

names_life<-second[second$Status_life=="life","ident"]# имена живых после второй ТКМ
names_died<-second[second$Status_life=="died","ident"]#имена умерших после 2 ТКМ
f_for_s_life<-first[first$ident %in% names_life,]#информация про первую у живых
f_for_s_died<-first[first$ident %in% names_died,]#информация про первую у мертвых
f_for_s_life$Last_contact_date<-second[second$ident%in%names_life,"Last_contact_date"]#меняем дату последнего контакта на дату второй ТКМ
f_for_s_died$Last_contact_date<-second[second$ident%in%names_died,"Last_contact_date"]#меняем дату последнего контакта на дату смерти от второй ТКМ

result_base<-rbind(sel,f_for_s_life, f_for_s_died)
write.csv(result_base, file = "data/Plus_two.csv")
