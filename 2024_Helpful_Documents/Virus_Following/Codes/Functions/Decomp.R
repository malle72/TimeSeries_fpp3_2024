Decomp=function(x,y,z){
y=100*y  
#========================STL decomposition============================
stl1=mstl(x)
autoplot(stl1)+
  ggtitle("STL decomposition-smooth_trainspl-MeanMethod")
ggsave(file=paste0("../Results/Residual_Clean/Graphs/Decom_Plots/STL_d_",y,"_k_",z,".pdf"), h=6, w=6, units="in",dpi=300)

#=======================classical multiplicative decomposition===================================== 

x %>% decompose(type="multiplicative") %>%
  autoplot()+xlab("Year")+
  ggtitle("Classical multipicative decomposition of lumber taxes")
ggsave(file=paste0("../Results/Residual_Clean/Graphs/Decom_Plots/Multi_d_",y,"_k_",z,".pdf"), h=6, w=6, units="in",dpi=300)

#====================x11 decomposition========= 
x %>% seas(x11="") %>%
  autoplot()+xlab("Year")+
  ggtitle("X11 decomposition of lumber taxes")
ggsave(file=paste0("../Results/Residual_Clean/Graphs/Decom_Plots/X11_d_",y,"_k_",z,".pdf"), h=6, w=6, units="in",dpi=300)

#=======================classical additive decomposition=====================================
x %>% decompose(type="additive") %>%
  autoplot()+xlab("Year")+
  ggtitle("Classical additive decomposition of lumber taxes")
ggsave(file=paste0("../Results/Residual_Clean/Graphs/Decom_Plots/Add_d_",y,"_k_",z,".pdf"), h=6, w=6, units="in",dpi=300)

}