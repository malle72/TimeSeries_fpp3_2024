#============= Load libraries====================
rm(list = ls())
library(easypackages)
libraries("fpp2","zoo","tidyverse","lubridate","GGally","gridExtra","ggrepel","foreign", "urca","psych","hydroGOF")
source("./Functions/TSConvert.R") # adds the function 
#===================== Read data, keep latest, format dates =======================
df=read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
df1=df%>%filter(state=="Louisiana")%>%select(date,cases,deaths)
colnames(df1)=c("Date","Cases","Deaths")
write.csv(df1,"../Data/Processed/Cleandata1.csv")
df1$Date=as.Date(df1$Date)
dir.create(paste0("../Results/",df1$Date[nrow(df1)]),showWarnings = F)

#===================== Simple Graph of Cumulative Cases ===========================
p1=ggplot(df1, aes(x=Date, y=Cases)) +
  geom_point(size=0.5)+
  scale_x_date(breaks = "1 month",date_labels="%b")+
  geom_line(color="#00AFBB") + xlab("") + ylab("Daily Cases")+ 
  ggtitle("Timeseries of Cumulative Cases")+
  theme_classic()
ggsave(paste0("../Results/",df1$Date[nrow(df1)],"/Cases.jpg"),width=10,heigh=10,p1) 
p1
#=================== Simple Graph of Cumulative Deaths ===========================
p1=ggplot(df1, aes(x=Date, y=Deaths)) +
  geom_point(size=0.5)+
  scale_x_date(breaks = "1 month",date_labels="%b")+
  geom_line(color="#00AFBB") + xlab("") + ylab("Daily Deaths")+ 
  ggtitle("Timeseries of Cumulative Deaths")+
  theme_classic()
ggsave(paste0("../Results/",df1$Date[nrow(df1)],"/Deaths.jpg"),width=10,heigh=10,p1) 
p1
#===================== Creating moving averages and adding them to the dataset ============
StartDay=df1$Date[1]
yr_mt_da=as.numeric(strsplit(as.character(StartDay),"-")[[1]])
#============ Creating the Daily Cases and their moving Average
Casests <- ts(df1$Cases, start=yr_mt_da,frequency=365.25)
Casets1=c(Casests[1],diff(Casests,1))
Casests_ma=ma(Casets1,7)
df1$DailyCases=as.vector(Casets1)
df1$Cases_ma=as.vector(Casests_ma)
#============ Creating the daily deaths and their moving average
Deathsts <- ts(df1$Deaths, start=yr_mt_da,frequency=365.25)
Deathsts1=c(Deathsts[1],diff(Deathsts,1))
Deathsts_ma=ma(Deathsts1,7)
df1$DailyDeaths=as.vector(Deathsts1)
df1$Deaths_ma=as.vector(Deathsts_ma)

#============ Summary Statistics and Saving the data =====================================
SumStats=describe(df1,na.rm=TRUE,trim=0.1,quant=c(.25,.75))%>%select(-vars)
write.csv(SumStats,paste0("../Results/",df1$Date[nrow(df1)],"/Summary_Statistics.csv"))
write.csv(df1,"../Data/Processed/Cleandata.csv")

#============= Train test split Setting parameters ================
d=0.05
h=10
k=round(nrow(df1)*d)
l=nrow(df1)-k
train=df1[1:l,]
test=df1[(l+1):nrow(df1),]
freq=365.25
#============= Analysis For Cases ===============================================================

#============= Creating Time series ============

Allcases_train=TSConvert(train,"Cases_ma",freq)
Allcases_test=TSConvert(test,"Cases_ma",freq)
AllCases=c(Allcases_train,Allcases_test)

#================ NN train and prediction =========================
fit1=nnetar(Allcases_train,p=7,Size=10,repeats=50,lambda = "auto")
for1=forecast(fit1,k)
autoplot(for1)
predictions1=for1$mean
autoplot(predictions1)

fit2=nnetar(Allcases_train)
for2=forecast(fit2,k)
autoplot(for2)
predictions2=for2$mean
autoplot(predictions2)
#================ ARIMA and prediction =========================
fit3=auto.arima(Allcases_train) 
for3=forecast(fit3,h=k)
autoplot(for3)
predictions3=for3$mean
autoplot(predictions3)

fit4=arima(Allcases_train,order=c(4,0,1))
for4=forecast(fit4,k)
autoplot(for4)
predictions4=for4$mean
autoplot(predictions4)
#================ Comparing Predictions vs Truth ===================
results=data.frame(Date=test$Date,Test=test$Cases_ma,PredNN=as.vector(predictions1),
                   PredAutoNN=as.vector(predictions2),PredAutoARIMA=as.vector(predictions3),
                   PredARIMA=as.vector(predictions4))

results$Ensamble1=rowMeans(results[,c(3,5)])
results$Ensamble2=rowMeans(results[,3:6])

#================ Plotting Prediction vs Truth ====================
p1=ggplot() +
  geom_line(data=df1, aes(x = Date, y = Cases_ma, colour = "Actual Values"))+
  geom_line(data = results, aes(x = Date, y = Test, colour = "Actual Values")) +
  geom_line(data = results, aes(x = Date, y = PredNN,   colour = "Predictions NN"))  +
  geom_line(data = results, aes(x = Date, y = PredARIMA,   colour = "Predictions ARIMA"))  +
  geom_line(data = results, aes(x = Date, y = PredAutoARIMA,   colour = "Predictions Auto ARIMA"))  +
  geom_line(data = results, aes(x = Date, y = PredAutoNN,   colour = "Predictions Auto Neural Network"))  +
  geom_line(data = results, aes(x = Date, y = Ensamble1,   colour = "Predictions Ensamble1"))  +
  geom_line(data = results, aes(x = Date, y = Ensamble2,   colour = "Predictions Ensamble2"))  +
  ylab('Cases')+
  scale_x_date(breaks = "1 month",date_labels="%b")+
  #scale_x_date(breaks=df1$Date ,labels=format(df1$Date,format="%m-%d"))+
  ggtitle(paste0("Comparison of Predicted vs True Number of Smoothed (7) Cases for ",round(nrow(df1)*d)," days"))+
  theme(axis.text.x = element_text(angle=45, hjust = 1))
ggsave(paste0("../Results/",df1$Date[nrow(df1)],"/Predictions_vs_Test_days_",k,"_Cases.jpg"),p1,width=10,heigh=8)
p1
#=============== Create Root Mean Square Errors ==================
RMSE=matrix(0,nrow=1,ncol = ncol(results)-2) 
RMSE=as.data.frame(RMSE)
colnames(RMSE)=colnames(results[,3:ncol(results)])

for (i in 1:ncol(RMSE)){
  RMSE[1,i]=rmse(results[,2],results[,i+2],na.rm=TRUE)
}

write.csv(RMSE,paste0("../Results/",df1$Date[nrow(df1)],"/Smoothed_7_RMSE_days_",k,"_Cases.csv"))

#=============== Graphing the results with important dates ==============
geom.text.size = 4
theme.size = (14/5) * geom.text.size
Lck=as.Date("2020-03-22")
Ph2=Lck+74
Msk=Lck+111
ScOpen=Lck+151
Ph3=Lck+177
MPh2=Lck+248
Vac=Lck+288
Ph3N=Lck+346
p1=ggplot(df1, aes(x=Date, y=Cases_ma)) +
  #geom_rect(xmin = ScOpen, xmax = ScOpen+5, ymin = -500, ymax = max(df1$Cases_Total)+500,alpha = .05,fill="lightgreen")+
  geom_point(size=0.5)+
  geom_line(data=df1, aes(x = Date, y = Cases_ma, colour = "Actual Values"))+
  geom_line(data=results, aes(x = Date, y = PredNN, colour = "NN Predictions"))+
  geom_text(x=Lck, y=1800, label="Lockdown",colour="red",size=geom.text.size)+
  geom_vline(xintercept=Lck, linetype=4,colour="red")+
  geom_text(x=Ph2, y=1000, label="Phase 2 Reopening",colour="green",size=geom.text.size)+
  geom_vline(xintercept=Ph2, linetype=4,colour="green")+
  geom_text(x=Msk, y=1500, label="Mask Mandate",colour="red",size=geom.text.size)+
  geom_vline(xintercept=Msk, linetype=4,colour="red")+
  geom_text(x=ScOpen, y=1300, label="Schools Open",colour="green",size=geom.text.size)+
  geom_vline(xintercept=ScOpen, linetype=4,colour="green")+
  #geom_vline(xintercept=ScOpen+5, linetype=4,colour="green")+
  geom_text(x=Ph3, y=300, label="Phase 3 Reopening",colour="green",size=geom.text.size)+
  geom_vline(xintercept=Ph3, linetype=4,colour="green")+ 
  geom_text(x=MPh2, y=1100, label="Modified Phase 2",colour="green",size=geom.text.size)+
  geom_vline(xintercept=MPh2, linetype=4,colour="green")+
  geom_text(x=Vac, y=800, label="Vaccinations Started",colour="red",size=geom.text.size)+
  geom_vline(xintercept=Vac, linetype=4,colour="red")+
  geom_text(x=Ph3N, y=1100, label="Phase 3",colour="green",size=geom.text.size)+
  geom_vline(xintercept=Ph3N, linetype=4,colour="green")+
  scale_x_date(breaks = "1 month",date_labels="%b")+
  ylab("Daily Cases")+
  ggtitle(paste0("MA(7) Smoothed Timeseries of Covid19 Cases in Louisiana with ",h," days prediction"))+
  theme_bw()+
  theme(axis.text = element_text(size = theme.size, colour="black"))+
  theme(legend.title = element_blank())
ggsave(paste0("../Results/",df1$Date[nrow(df1)],"/Smoothed_With_",h,"_days_prediction_Cases.jpg"),width=12,heigh=8,p1) 
ggsave(paste0("../Results/Latest_Smoothed_With_",h,"_days_prediction_Cases.jpg"),width=12,heigh=8,p1) 











#============= Analysis For Deaths ===============================================================

#============= Creating Time series ============
freq=365.25
Alldeaths_train=TSConvert(train,"Deaths_ma",freq)
Alldeaths_test=TSConvert(test,"Deaths_ma",freq)
Alldeaths=c(Alldeaths_train,Alldeaths_test)

#================ NN train and prediction =========================
fit1=nnetar(Alldeaths_train,p=7,Size=10,repeats=50,lambda = "auto")
for1=forecast(fit1,k)
autoplot(for1)
predictions1=for1$mean
autoplot(predictions1)

fit2=nnetar(Alldeaths_train)
for2=forecast(fit2,k)
autoplot(for2)
predictions2=for2$mean
autoplot(predictions2)
#================ ARIMA and prediction =========================
fit3=auto.arima(Alldeaths_train) 
for3=forecast(fit3,h=k)
autoplot(for3)
predictions3=for3$mean
autoplot(predictions3)

fit4=arima(Alldeaths_train,order=c(4,0,1))
for4=forecast(fit4,k)
autoplot(for4)
predictions4=for4$mean
autoplot(predictions4)
#================ Comparing Predictions vs Truth ===================
results=data.frame(Date=test$Date,Test=test$Deaths_ma,PredNN=as.vector(predictions1),
                   PredAutoNN=as.vector(predictions2),PredAutoARIMA=as.vector(predictions3),
                   PredARIMA=as.vector(predictions4))

results$Ensamble1=rowMeans(results[,c(3,5)])
results$Ensamble2=rowMeans(results[,3:6])

#================ Plotting Prediction vs Truth ====================
p1=ggplot() +
  geom_line(data=df1, aes(x = Date, y = Deaths_ma, colour = "Actual Values"))+
  geom_line(data = results, aes(x = Date, y = Test, colour = "Actual Values")) +
  geom_line(data = results, aes(x = Date, y = PredNN,   colour = "Predictions NN"))  +
  geom_line(data = results, aes(x = Date, y = PredARIMA,   colour = "Predictions ARIMA"))  +
  geom_line(data = results, aes(x = Date, y = PredAutoARIMA,   colour = "Predictions Auto ARIMA"))  +
  geom_line(data = results, aes(x = Date, y = PredAutoNN,   colour = "Predictions Auto Neural Network"))  +
  geom_line(data = results, aes(x = Date, y = Ensamble1,   colour = "Predictions Ensamble1"))  +
  geom_line(data = results, aes(x = Date, y = Ensamble2,   colour = "Predictions Ensamble2"))  +
  ylab('Deaths')+
  scale_x_date(breaks = "1 month",date_labels="%b")+
  #scale_x_date(breaks=df1$Date ,labels=format(df1$Date,format="%m-%d"))+
  ggtitle(paste0("Comparison of Predicted vs True Number of Smoothed (7) Deaths for ",round(nrow(df1)*d)," days"))+
  theme(axis.text.x = element_text(angle=45, hjust = 1))
ggsave(paste0("../Results/",df1$Date[nrow(df1)],"/Predictions_vs_Test_days_",k,"_Deaths.jpg"),p1,width=10,heigh=8)
p1
#=============== Create Root Mean Square Errors ==================
RMSE=matrix(0,nrow=1,ncol = ncol(results)-2) 
RMSE=as.data.frame(RMSE)
colnames(RMSE)=colnames(results[,3:ncol(results)])

for (i in 1:ncol(RMSE)){
  RMSE[1,i]=rmse(results[,2],results[,i+2],na.rm=TRUE)
}

write.csv(RMSE,paste0("../Results/",df1$Date[nrow(df1)],"/Smoothed_7_RMSE_days_",k,"_Deaths.csv"))

res=(sort(RMSE[1,])[c(1,2)])
colnames(res)


#=============== Graphing the results with important dates ==============
geom.text.size = 4
theme.size = (14/5) * geom.text.size
Lck=as.Date("2020-03-22")
Ph2=Lck+74
Msk=Lck+111
ScOpen=Lck+151
Ph3=Lck+177
MPh2=Lck+248
Vac=Lck+288
Ph3N=Lck+346
p1=ggplot(df1, aes(x=Date, y=Deaths_ma)) +
  #geom_rect(xmin = ScOpen, xmax = ScOpen+5, ymin = -500, ymax = max(df1$Cases_Total)+500,alpha = .05,fill="lightgreen")+
  geom_point(size=0.5)+
  geom_line(data=df1, aes(x = Date, y = Deaths_ma, colour = "Actual Values"))+
  geom_line(data=results, aes(x = Date, y = PredAutoARIMA, colour = "AutoArima Predictions"))+
  geom_text(x=Lck, y=18, label="Lockdown",colour="red",size=geom.text.size)+
  geom_vline(xintercept=Lck, linetype=4,colour="red")+
  geom_text(x=Ph2, y=10, label="Phase 2 Reopening",colour="green",size=geom.text.size)+
  geom_vline(xintercept=Ph2, linetype=4,colour="green")+
  geom_text(x=Msk, y=15, label="Mask Mandate",colour="red",size=geom.text.size)+
  geom_vline(xintercept=Msk, linetype=4,colour="red")+
  geom_text(x=ScOpen, y=13, label="Schools Open",colour="green",size=geom.text.size)+
  geom_vline(xintercept=ScOpen, linetype=4,colour="green")+
  #geom_vline(xintercept=ScOpen+5, linetype=4,colour="green")+
  geom_text(x=Ph3, y=3, label="Phase 3 Reopening",colour="green",size=geom.text.size)+
  geom_vline(xintercept=Ph3, linetype=4,colour="green")+ 
  geom_text(x=MPh2, y=11, label="Modified Phase 2",colour="green",size=geom.text.size)+
  geom_vline(xintercept=MPh2, linetype=4,colour="green")+
  geom_text(x=Vac, y=8, label="Vaccinations Started",colour="red",size=geom.text.size)+
  geom_vline(xintercept=Vac, linetype=4,colour="red")+
  geom_text(x=Ph3N, y=11, label="Phase 3",colour="green",size=geom.text.size)+
  geom_vline(xintercept=Ph3N, linetype=4,colour="green")+
  scale_x_date(breaks = "1 month",date_labels="%b")+
  ylab("Daily Deaths")+
  ggtitle(paste0("MA(7) Smoothed Timeseries of Covid19 Deaths in Louisiana with ",h," days prediction"))+
  theme_bw()+
  theme(axis.text = element_text(size = theme.size, colour="black"))+
  theme(legend.title = element_blank())
ggsave(paste0("../Results/",df1$Date[nrow(df1)],"/Smoothed_With_",h,"_days_prediction_Deaths.jpg"),width=12,heigh=8,p1) 
ggsave(paste0("../Results/Latest_Smoothed_With_",h,"_days_prediction_Deaths.jpg"),width=12,heigh=8,p1) 

  


