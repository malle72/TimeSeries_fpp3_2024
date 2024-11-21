LIFO<-function()
{
setwd("../Data/Raw/")
temp1 = list.files(pattern="*.csv") #Creates an ordered vector by scraping and storing the FullNames all the .csv files in this folder
temp2 = c()
temp3 = c()
for (i in 1:length(temp1)){temp2[i]=strsplit(temp1[i],"[.]")[[1]][1]
}
for (i in 1:length(temp1)){temp3[i]=as.numeric(strsplit(temp2[i],"_")[[1]][2])*50+as.numeric(strsplit(temp2[i],"_")[[1]][3])+as.numeric(strsplit(temp2[i],"_")[[1]][4])*1000
}
Allfiles = lapply(temp1, read.csv)
df1=Allfiles[[which.max(temp3)]]
return(df1)
}