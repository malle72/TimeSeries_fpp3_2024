TSConvert<-function(df,Var_Name,freq){
  StartTrYear=as.numeric(strsplit(as.character(df$Date[1]),"-")[[1]][1])
  StartTrMonth=as.numeric(strsplit(as.character(df$Date[1]),"-")[[1]][2])
  StartTrDay=as.numeric(strsplit(as.character(df$Date[1]),"-")[[1]][3])  
  tmseries=ts(df[,Var_Name],start=c(StartTrYear,StartTrMonth,StartTrDay),frequency=freq)
 return(tmseries)
}