Preproc=function(x,y,z){
myts_ma=ma(x,y)
Decomp(myts_ma,y,z)
ls=list(myts_adj,myts_diff,myts_ma)
return(ls)
}


# Define a function as "Preproc", this function will call the other functions we created before: myts_adj,myts_diff, myts_ma, Decomp.