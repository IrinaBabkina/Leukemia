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

#PCA
all[is.na(all)] <- 0
all_t<-all
all_t<-all_t[,-1]
all_PCA<-t(apply(all_t, 1,as.numeric))
library(vegan)
all_PCA
PCA<-rda(X=all_PCA, scale = T)
e_v<-eigenvals(PCA)
factor_scores<-scores(PCA, display = "species", 
                      choices = c(1, 2, 3), scaling = 0)
screeplot(PCA, type = "lines", bstick = TRUE)

biplot(PCA, display = "species", scaling = 2,choices = c(2,3))
biplot(PCA, display = "species", scaling = 2,choices = c(1,2))
biplot(PCA, display = "species", scaling = 2,choices = c(1,3))

df_scores <- data.frame(all_PCA,
                        scores(PCA, display = "sites", choices = c(1, 2, 3,4), scaling = 1), all$year)
df_scores$year<-as.numeric(df_scores$all.year)

library("ggrepel")
point_col<-factor(c(rep(1,7), rep(2,6),rep(3,6)))

ggplot(df_scores, aes(x = PC1, y = PC2, colour = point_col)) + 
  geom_point(size = 3, alpha = 0.8) + ggtitle("Ординация в пространстве главных компонент")+geom_text_repel(label = df_scores$all.year, color = "black",nudge_y = 0.05) + theme_bw()+scale_color_discrete(name = "Период")

library(plotly)
plot_ly(df_scores, x = ~PC1, y = ~PC2, z = ~PC3, color = ~year, marker =list(size = 5))


d <- dist(all_PCA) # euclidean distances between the rows
fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
fit # view results

# plot solution 
x <- fit$points[,1]
y <- fit$points[,2]
mds<-data.frame(x=x, y=y, year = all$year)
mds$year<-as.character(mds$year)
mds$year<-as.numeric(mds$year)

mds$point<-point_col
ggplot(mds, aes(x,y, color = point))+geom_text_repel(label = all$year, nudge_y = 7, color = "black")+geom_point(size = 3, alpha = 0.8)+ggtitle("Ординация в пространстве MDS") + scale_color_discrete(name = "Период")+theme_bw()

patient<-data[,c("Phaze_do_TKM", "MAC_RIC", "Consistency","ITK_after_TKM", "ITK_before_TKM","Gender", "year" , "ident")]
patient<-na.omit(patient)
library(PCAmixdata)
pat_PCA<-PCAmix(X.quali = patient[, -c(7:8)], ndim = 3, graph = T)
str(pat_PCA)
pat_scores<-as.data.frame(pat_PCA$scores)
colnames(pat_scores)<-c("PC1", "PC2", "PC3")
ggplot(pat_scores, aes(PC1, PC2))+geom_point(aes(color = patient$year), size =3)
pat_col<-ifelse(patient$year %in% c("1995", "2001","2002", "2003", "2004", "2005"), 1, ifelse(patient$year %in% c("2006", "2007", "2008", "2009", "2010", "2011", "2012"), 2, 3))
pat_col<-factor(pat_col)
ggplot(pat_scores, aes(PC2, PC3))+geom_point(aes(color = pat_col), size =3, alpha = 0.7)+geom_text(label = rownames(patient), nudge_y = 0.1)

library(dummies)
d_d<-dummy.data.frame(patient[, -c(7:8)])
d_dist<-dist(d_d)
fit_d <- cmdscale(d_dist,eig=TRUE, k=2) 

# plot solution 
x <- fit_d$points[,1]
y <- fit_d$points[,2]
mds<-data.frame(x=x, y=y)
ggplot(mds, aes(x,y, color = pat_col))+geom_point(size = 3, alpha = 0.8)+ggtitle("Ординация в пространстве MDS")


pat_PCA<-PCAmix(X.quanti = d_d, ndim = 3, graph = T)
str(pat_PCA)
pat_scores<-as.data.frame(pat_PCA$scores)
colnames(pat_scores)<-c("PC1", "PC2", "PC3")
ggplot(pat_scores, aes(PC1, PC2))+geom_point(aes(color = patient$year), size =3)
pat_col<-ifelse(patient$year %in% c("1995", "2001","2002", "2003", "2004", "2005"), 1, ifelse(patient$year %in% c("2006", "2007", "2008", "2009", "2010", "2011", "2012"), 2, 3))
pat_col<-factor(pat_col)
ggplot(pat_scores, aes(PC1, PC2))+geom_point(aes(color = pat_col), size =3, alpha = 0.7)
pat_PCA$coef

vegan_pca<-rda(X=d_d, scale = T)
e_v_p<-eigenvals(vegan_pca)
(e_v_p/19)*100
