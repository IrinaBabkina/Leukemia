library(ggplot2)
library(lubridate)
library(dplyr)
library(stringr)
library(tidyr)

#Прочитаем данные и преобразуем факторы
data<-read.table("data/third_filtered.csv", sep  = ";", header = T)
data[c(5,8,12,16:17,20:22, 24, 46, 49, 52:53, 57, 59, 61)] <- 
  lapply(data[c(5,8,12,16:17,20:22, 24, 46, 49, 52:53, 57, 59, 61)], as.POSIXct)
data[c(11, 25,26,45,48)]<-lapply(data[c(11, 25,26,45,48)], as.factor)
data$year<-factor(year(data$Date_alloTKM))
data$Cy<-factor(str_detect(data$GVHD_prophylactic, "Cy"))
levels(data$Cy)<-c("Cy-","Cy+")
new_data

# соберем статистику по процентам
b_data<-data %>%
  group_by(year,Phaze_do_TKM)%>%
  summarize(n=n())%>%
  mutate(prop = round(n/sum(n), 3)*100)%>%
  subset(select=c("Phaze_do_TKM","year","prop"))%>%  
  spread(Phaze_do_TKM, prop) 

a_data<-data%>%
  group_by(year,MAC_RIC)%>%
  summarize(n=n())%>%
  mutate(prop=round(n/sum(n), 3)*100)%>%
  subset(select=c("MAC_RIC","year","prop"))%>%   #drop the frequency value
  spread(MAC_RIC, prop) 

c_data<-data%>%
  group_by(year,ITK_before_TKM)%>%
  summarize(n=n())%>%
  mutate(prop=round(n/sum(n), 3)*100)%>%
  subset(select=c("ITK_before_TKM","year","prop"))%>%   #drop the frequency value
  spread(ITK_before_TKM, prop) 

d_data<-data%>%
  group_by(year,Consistency)%>%
  summarize(n=n())%>%
  mutate(prop=round(n/sum(n), 3)*100)%>%
  subset(select=c("Consistency","year","prop"))%>%   #drop the frequency value
  spread(Consistency, prop) 

e_data<-data%>%
  group_by(year,ITK_after_TKM)%>%
  summarize(n=n())%>%
  mutate(prop=round(n/sum(n), 3)*100)%>%
  subset(select=c("ITK_after_TKM","year","prop"))%>%   #drop the frequency value
  spread(ITK_after_TKM, prop) 

f_data<-data%>%
  group_by(year,Cy)%>%
  summarize(n=n())%>%
  mutate(prop=round(n/sum(n), 3)*100)%>%
  subset(select=c("Cy","year","prop"))%>%   #drop the frequency value
  spread(Cy, prop) 

#склеим единый датафрейм. Он и есть наша таблица. 
all<-cbind(b_data, a_data, c_data[-8], d_data[-5],e_data[-4],f_data)
all<-all[-c(6,9,16,20,23)]
colnames(all)

#рисовать из сопряженной таблицы нельзя. Ее нужно развернуть
grapf<-all %>% gather(key = Fact, value = prot, -year)
grapf$Fact<-factor(grapf$Fact, levels = colnames(all))

# чтобы нули и NA не мешали
grapf <- grapf %>%
  mutate(prot = replace(prot,is.na(prot),0),
         text = replace(prot,prot == 0,""))

#Нужно сделать факторы, отвечающие за цвет блоков путем объединения градаций
library(rockchalk)
a<-combineLevels(grapf$Fact, c("Cy-", "Cy+"), newLabel = "Циклофосфан")
a<-combineLevels(a, c("particular", "haplo","full"), newLabel = "Совместимость")
a<-combineLevels(a, c("MAC", "RIC"), newLabel = "MAC_RIC")
a<-combineLevels(a, c("AP", "BC","CP_1","CP_2_3_4"), newLabel = "Фаза")
a<-combineLevels(a, c("no", "yes"), newLabel = "ИТК после")
a<-combineLevels(a, c("bosy", "daza","ima","nilo", "no_ITK","pona"), newLabel = "ИТК до")
grapf$color<-a


#График!!!!
ggplot(grapf, aes(year, Fact))+geom_tile(aes(alpha = prot, fill = color ))+theme_classic()+geom_text(aes(label=text), color="black", size=3)+guides(alpha = guide_legend(title = "Процент"), fill = guide_legend(title = "Фактор"))+labs(x= "Год проведения ТКМ", y="Уровни факторов")

#Если добавить терапию

detach("package:rockchalk", unload=TRUE)
g_data<-data%>%
  group_by(year,Therapy)%>%
  summarize(n=n())%>%
  mutate(prop=round(n/sum(n), 3)*100)%>%
  subset(select=c("Therapy","year","prop"))%>%   #drop the frequency value
  spread(Therapy, prop) 

all_t<-cbind(all, g_data)
all_t<-all_t[-21]
grapf_t<-all_t %>% gather(key = Fact, value = prot, -year)
grapf_t$Fact<-factor(grapf_t$Fact, levels = colnames(all_t))

grapf_t <- grapf_t %>%
  mutate(prot = replace(prot,is.na(prot),0),
         text = replace(prot,prot == 0,""))

library(rockchalk)
a<-combineLevels(grapf_t$Fact, c("Cy-", "Cy+"), newLabel = "Циклофосфан")
a<-combineLevels(a, c("particular", "haplo","full"), newLabel = "Совместимость")
a<-combineLevels(a, c("MAC", "RIC"), newLabel = "MAC_RIC")
a<-combineLevels(a, c("AP", "BC","CP_1","CP_2_3_4"), newLabel = "Фаза")
a<-combineLevels(a, c("no", "yes"), newLabel = "ИТК после")
a<-combineLevels(a, c("bosy", "daza","ima","nilo", "no_ITK","pona"), newLabel = "ИТК до")
a<-combineLevels(a, c("first_line", "no_ITK1","second","third"), newLabel = "Терапия")
grapf_t$color<-a

ggplot(grapf_t, aes(year, Fact))+geom_tile(aes(alpha = prot, fill = color ))+theme_classic()+geom_text(aes(label=text), color="black", size=3)+guides(alpha = guide_legend(title = "Процент"), fill = guide_legend(title = "Фактор"))+labs(x= "Год проведения ТКМ", y="Уровни факторов")
