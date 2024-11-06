#load libraries and functions ====================
rm(list = ls())
library(easypackages)
libraries("fpp2","readxl","gridExtra","zoo","GGally", "urca","foreign","tidyverse","ggrepel","xts","dygraph")
source("./Functions/All_Functions.R") # adds the function 
#===================== Read data, keep latest, format DATEs =======================
dirnow=getwd()
df1=read.csv("../Data/Raw/us-counties.csv",header = T)
df2=df1%>%filter(state=="Louisiana")%>%mutate(across(where(is_character),as_factor))%>%rename_all(. %>% toupper() %>% gsub(" ", "_", .))%>%select(DATE,COUNTY,CASES,DEATHS)
df3=df2 %>% group_by(STATE,DATE) %>% summarise(CASES_TOTAL = sum(CASES,na.rm = T))
df3$DATE=as.Date(df3$DATE)
df3=df3%>%mutate(MA7_DAILY=rollmean(CASES_TOTAL-lag(CASES_TOTAL,1),7,fill=NA))

write.csv(df3,"../Data/Processed/Cleandata1.csv")
dir.create(paste0("../Results/",df3$DATE[nrow(df3)]),showWarnings = F)
df4=xts(df3,order.by = df3$DATE)

#=====================
p1=ggplot(df3, aes(x=DATE, y=MA7_DAILY)) +
  geom_point(size=0.5)+
  scale_x_DATE(breaks = "1 month",DATE_labels="%b")+
  #scale_x_DATE(breaks=df3$DATE,labels=format(df3$DATE,format="%d"))+
  geom_line(color="#00AFBB") + xlab("") + ylab("Daily Cases")+ 
  #geom_text(aes(label=CASES_TOTAL),size=2,hjust = 1, nudge_x = 0.05)+
  ggtitle("Timeseries of Cases")+
  theme_classic()
  #+theme(axis.text.x = element_text(angle = 90))
ggsave(paste0("../Results/",df3$DATE[nrow(df3)],"/Cases.jpg"),width=10,heigh=10,p1) 
#===================== Creating moving averages and adding them to the dataset ============
Casests_ma <- TSConvert(df3,"MA7_DAILY",365.25)

#==================== Creating simple graphs with important DATEs ========================
geom.text.size = 4
theme.size = (14/5) * geom.text.size
p2=ggplot(df3, aes(x=DATE, y=MA7_DAILY)) +
  geom_point(size=0.5)+
  geom_vline(xintercept=as.numeric(df3$DATE[22]), linetype=4,colour="red")+
  geom_text(x=as.numeric(df3$DATE[22]), y=1000, label="Lockdown",colour="red",size=geom.text.size)+
  geom_vline(xintercept=as.numeric(df3$DATE[96]), linetype=4,colour="green")+
  geom_text(x=as.numeric(df3$DATE[96]), y=1000, label="Phase 2 Reopening",colour="green",size=geom.text.size)+
  geom_vline(xintercept=as.numeric(df3$DATE[133]), linetype=4,colour="red")+
  geom_text(x=as.numeric(df3$DATE[173]), y=1000, label="Schools Open",colour="green",size=geom.text.size)+
  geom_vline(xintercept=as.numeric(df3$DATE[173]), linetype=4,colour="green")+
  geom_text(x=as.numeric(df3$DATE[133]), y=1000, label="Mask Mandate",colour="red",size=geom.text.size)+
  #geom_text(aes(label=Cases_ma),size=2,hjust = 1, nudge_x = 0.05)+
  #scale_x_DATE(breaks=df4$DATE ,labels=format(df4$DATE,format="%m-%d"))+
  scale_x_date(breaks = "1 month",date_labels="%b")+
  geom_line(color="lightblue") + xlab("") + ylab("Daily Cases")+
  ggtitle("MA(7) Smoothed Timeseries of Covid19 Cases In Louisiana")+
  theme_classic()+
  theme(axis.text = element_text(size = theme.size, colour="black")) 
ggsave(paste0("../Results/",df3$DATE[nrow(df3)],"/Cases_Smoothed.jpg"),width=10,heigh=10,p2) 











