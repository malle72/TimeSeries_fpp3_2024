# x will be the old time series 
# y will be the percentage (d)

AdjTS=function(x,y){
  myts_vec=as.vector(x)
  cut_off=quantile(myts_vec,c(y,1-y))
  myts_old=x
  #==== Removing high residuals through choosing percentage to get the numeric values as outlier cutpoints============================
  UB=cut_off[2]
  LB=cut_off[1]
  x[x<LB]=LB
  x[x>UB]=UB
  myts_diff=myts_old-x
  ls=list(x,myts_diff)
  k1=as.vector(myts_diff)
  k1=abs(k1)
  k2=log(k1+1)
  k3=subset(k2,k2!=0)
  dfdif=data.frame(Differences=k3)
  g3=ggplot(dfdif, aes(x=Differences)) + 
    geom_histogram(aes(y=..density..),bins=12)+
    geom_density(alpha=.2, fill="#FF6666")+
    theme( axis.line = element_line(colour = "darkblue", size = 1, linetype = "solid"))+
    ggtitle(paste0("Histogram of Log Diff (no 0),d=",y*100))
    theme_light()
    ggsave(paste0("../Results/Residual_Clean/Graphs/Hist_Diff_d=",y*100,".pdf"),g3)
  g3
  return(ls)  #returns the augmented data setk1=as.vector(myts_diff)
  
} 