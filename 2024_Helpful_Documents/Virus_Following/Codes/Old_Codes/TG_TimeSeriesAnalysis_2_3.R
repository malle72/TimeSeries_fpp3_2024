#============= Load libraries, functions and data ====================
# Make sure to run the TG_TimeSeriesAnalysis_1_1.R first
rm(list = ls())
library(easypackages)
libraries("fpp2","zoo","tidyverse","lubridate","GGally")
source("./Functions/TSConvert.R") # adds the function 
df1=read.csv("../Data/Processed/Cleandata2.csv",row.names=1)
df1$Date=as.Date(df1$Date)
#============= Train test split ================
d=0.1
h=15
k=round(nrow(df1)*d)
l=nrow(df1)-k
train=df1[1:l,]
test=df1[(l+1):nrow(df1),]
#============= Creating Time series ============
freq=365.25
Allcases_train=TSConvert(train,"Cases_ma",freq)
Allcases_test=TSConvert(test,"Cases_ma",freq)
AllCases=c(Allcases_train,Allcases_test)
#AllCases_ma=ma(AllCases,order=7)
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
ggsave(paste0("../Results/",df1$Date[nrow(df1)],"/Predictions_vs_Test_days_",k,".jpg"),p1,width=10,heigh=8)
p1
#=============== Create Root Mean Square Errors ==================
RMSE=matrix(0,nrow=1,ncol = 6) 
RMSE=as.data.frame(RMSE)
colnames(RMSE)=colnames(results[,3:8])
for (i in 1:6){
  temp=results[,c(2,i+2)]
  temp$Diff=temp[,1]-temp[,2]
  temp1=temp$Diff[!is.na(temp$Diff)]
  RMSE[1,i]=sqrt(mean(temp1^2))
}
write.csv(RMSE,paste0("../Results/",df1$Date[nrow(df1)],"/Smoothed_7_RMSE_days_",k,".csv"))

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
ggsave(paste0("../Results/",df1$Date[nrow(df1)],"/Smoothed_With_",h,"_days_prediction.jpg"),width=12,heigh=8,p1) 



  

