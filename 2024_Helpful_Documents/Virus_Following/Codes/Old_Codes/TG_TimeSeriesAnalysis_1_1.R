#load libraries and functions ====================
rm(list = ls())
library(easypackages)
source("./Functions/LIFO.R") # adds the function 
libraries("fpp2","readxl","gridExtra","zoo","GGally", "urca","foreign","tidyverse","ggrepel")
#===================== Read data, keep latest, format dates =======================
dirnow=getwd()
df1=LIFO()
df1$Date=0
if (length(df1$datetime)==0)
{df1$datetime=df1$Lab.Collection.Date}
for (i in 1:nrow(df1)){
  df1$Date[i]=strsplit(as.character(df1$datetime[i])," ")[[1]][1]
}

#df1=df1[order(as.Date(df1$DateTime, format="%m/%d/%Y")),]
setwd(dirnow)
df2=df1[,c(2,5,8)]
colnames(df2)=c("Parish","Cases","Date")
df3=df2 %>% group_by(Date) %>% summarise(Cases_Total = sum(Cases,na.rm = T))
#df3$Cases_Orleans=subset(df2,df2$Parish=="Orleans")$Cases 
df3$Date=as.Date(df3$Date, format="%m/%d/%Y")
df3=df3[order(as.Date(df3$Date, format="%m/%d/%Y")),]
write.csv(df3,"../Data/Processed/Cleandata1.csv")
dir.create(paste0("../Results/",df3$Date[nrow(df3)]),showWarnings = F)
#=====================
#df4=gather(df3, Area, Cases, Cases_Total:Cases_Orleans, factor_key=TRUE)
#levels(df4$Area)=c("Total","Orleans")
#df4$Date=as.Date(df4$Date, format="%m/%d/%Y")
#df4=df4[order(as.Date(df4$Date, format="%m/%d/%Y")),]
#write.csv(df4,"../Data/Processed/Cleandata2.csv")
#=====================
p1=ggplot(df3, aes(x=Date, y=Cases_Total)) +
  geom_point(size=0.5)+
  scale_x_date(breaks = "1 month",date_labels="%b")+
  #scale_x_date(breaks=df3$Date,labels=format(df3$Date,format="%d"))+
  geom_line(color="#00AFBB") + xlab("") + ylab("Daily Cases")+ 
  geom_text(aes(label=Cases_Total),size=2,hjust = 1, nudge_x = 0.05)+
  ggtitle("Timeseries of Cases")+
  theme_classic()
  #+theme(axis.text.x = element_text(angle = 90))
ggsave(paste0("../Results/",df3$Date[nrow(df3)],"/Cases.jpg"),width=10,heigh=10,p1) 
#===================== Creating moving averages and adding them to the dataset ============
start=df3$Date[1]
yr=as.numeric(strsplit(as.character(start),"-")[[1]][1])
mt=as.numeric(strsplit(as.character(start),"-")[[1]][2])
da=as.numeric(strsplit(as.character(start),"-")[[1]][3])
Casests <- ts(df3$Cases_Total, start=c(yr,mt,da),frequency=365.25)
Casests_ma=ma(Casests,7)
df3$Cases_ma=as.vector(Casests_ma)
write.csv(df3,"../Data/Processed/Cleandata2.csv")
#==================== Creating simple graphs with important dates ========================
geom.text.size = 4
theme.size = (14/5) * geom.text.size
p2=ggplot(df3, aes(x=Date, y=Cases_ma)) +
  geom_point(size=0.5)+
  geom_vline(xintercept=as.numeric(df3$Date[22]), linetype=4,colour="red")+
  geom_text(x=as.numeric(df3$Date[22]), y=1000, label="Lockdown",colour="red",size=geom.text.size)+
  geom_vline(xintercept=as.numeric(df3$Date[96]), linetype=4,colour="green")+
  geom_text(x=as.numeric(df3$Date[96]), y=1000, label="Phase 2 Reopening",colour="green",size=geom.text.size)+
  geom_vline(xintercept=as.numeric(df3$Date[133]), linetype=4,colour="red")+
  geom_text(x=as.numeric(df3$Date[173]), y=1000, label="Schools Open",colour="green",size=geom.text.size)+
  geom_vline(xintercept=as.numeric(df3$Date[173]), linetype=4,colour="green")+
  geom_text(x=as.numeric(df3$Date[133]), y=1000, label="Mask Mandate",colour="red",size=geom.text.size)+
  #geom_text(aes(label=Cases_ma),size=2,hjust = 1, nudge_x = 0.05)+
  #scale_x_date(breaks=df4$Date ,labels=format(df4$Date,format="%m-%d"))+
  scale_x_date(breaks = "1 month",date_labels="%b")+
  geom_line(color="lightblue") + xlab("") + ylab("Daily Cases")+
  ggtitle("MA(7) Smoothed Timeseries of Covid19 Cases In Louisiana")+
  theme_classic()+
  theme(axis.text = element_text(size = theme.size, colour="black")) 
ggsave(paste0("../Results/",df3$Date[nrow(df3)],"/Cases_Smoothed.jpg"),width=10,heigh=10,p2) 











