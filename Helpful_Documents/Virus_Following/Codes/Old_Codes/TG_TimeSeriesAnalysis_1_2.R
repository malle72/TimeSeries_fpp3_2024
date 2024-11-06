#load libraries and functions ====================
rm(list = ls())
library(easypackages)
libraries("fpp2","readxl","gridExtra","zoo","GGally", "urca","foreign","tidyverse","ggrepel")
#===================== Read data, keep latest, format dates =======================
df1=read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
df2=df1%>%filter(state=="Louisiana")%>%select(date,cases,deaths)%>%mutate()
colnames(df2)=c("Date","Cases","Deaths")
write.csv(df2,"../Data/Processed/Cleandata1.csv")
dir.create(paste0("../Results/",df2$Date[nrow(df2)]),showWarnings = F)
df2$Date=as.Date(df2$Date)
#=====================
p1=ggplot(df2, aes(x=Date, y=Cases)) +
  geom_point(size=0.5)+
  scale_x_date(breaks = "1 month",date_labels="%b")+
  geom_line(color="#00AFBB") + xlab("") + ylab("Daily Cases")+ 
  #geom_text(aes(label=Cases),size=2,hjust = 1, nudge_x = 0.05)+
  ggtitle("Timeseries of Cases")+
  theme_classic()
  #+theme(axis.text.x = element_text(angle = 90))
ggsave(paste0("../Results/",df2$Date[nrow(df2)],"/Cases.jpg"),width=10,heigh=10,p1) 
p1=ggplot(df2, aes(x=Date, y=Deaths)) +
  geom_point(size=0.5)+
  scale_x_date(breaks = "1 month",date_labels="%b")+
  geom_line(color="#00AFBB") + xlab("") + ylab("Daily Deaths")+ 
  #geom_text(aes(label=Cases),size=2,hjust = 1, nudge_x = 0.05)+
  ggtitle("Timeseries of Deaths")+
  theme_classic()
#+theme(axis.text.x = element_text(angle = 90))
ggsave(paste0("../Results/",df2$Date[nrow(df2)],"/Deaths.jpg"),width=10,heigh=10,p1) 
#===================== Creating moving averages and adding them to the dataset ============
start=df2$Date[1]
yr=as.numeric(strsplit(as.character(start),"-")[[1]][1])
mt=as.numeric(strsplit(as.character(start),"-")[[1]][2])
da=as.numeric(strsplit(as.character(start),"-")[[1]][3])
Casests <- ts(df2$Cases, start=c(yr,mt,da),frequency=365.25)
Casets1=c(Casests[1],diff(Casests,1))
Casests_ma=ma(Casets1,7)
df2$Cases_ma=as.vector(Casests_ma)
Deathsts <- ts(df2$Deaths, start=c(yr,mt,da),frequency=365.25)
Deathsts1=c(Deathsts[1],diff(Deathsts,1))
Deathsts_ma=ma(Deathsts1,7)
df2$Deaths_ma=as.vector(Deathsts_ma)
write.csv(df2,"../Data/Processed/Cleandata2.csv")
#==================== Creating simple graphs with important dates ========================
geom.text.size = 4
theme.size = (14/5) * geom.text.size
p2=ggplot(df2, aes(x=Date, y=Cases_ma)) +
  geom_point(size=0.5)+
  geom_vline(xintercept=as.numeric(df2$Date[22]), linetype=4,colour="red")+
  geom_text(x=as.numeric(df2$Date[23]), y=1000, label="Lockdown",colour="red",size=geom.text.size)+
  geom_vline(xintercept=as.numeric(df2$Date[96]), linetype=4,colour="green")+
  geom_text(x=as.numeric(df2$Date[97]), y=1000, label="Phase 2 Reopening",colour="green",size=geom.text.size)+
  geom_vline(xintercept=as.numeric(df2$Date[133]), linetype=4,colour="red")+
  geom_text(x=as.numeric(df2$Date[174]), y=1000, label="Schools Open",colour="green",size=geom.text.size)+
  geom_vline(xintercept=as.numeric(df2$Date[173]), linetype=4,colour="green")+
  geom_text(x=as.numeric(df2$Date[134]), y=1000, label="Mask Mandate",colour="red",size=geom.text.size)+
  #geom_text(aes(label=Cases_ma),size=2,hjust = 1, nudge_x = 0.05)+
  #scale_x_date(breaks=df4$Date ,labels=format(df4$Date,format="%m-%d"))+
  scale_x_date(breaks = "1 month",date_labels="%b")+
  geom_line(color="lightblue") + xlab("") + ylab("Daily Cases")+
  ggtitle("MA(7) Smoothed Timeseries of Covid19 Cases In Louisiana")+
  theme_classic()+
  theme(axis.text = element_text(size = theme.size, colour="black")) 
ggsave(paste0("../Results/",df2$Date[nrow(df2)],"/Cases_Smoothed.jpg"),width=10,heigh=10,p2) 











