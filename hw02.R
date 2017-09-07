# Homework two
library(xlsx)

soi.table = read.xlsx2("soi1876-2017.xlsx",1,as.data.frame = TRUE, stringsAsFactors=FALSE)

sum(!is.na(soi.table))

soi = c()
for(i in 1:142){
  soi = c(soi,unlist(soi.table[i,]))
}
soi = soi[!soi == "NaN"]
soi = as.numeric(soi)

soi.trans = matrix(NA,nrow = 170,ncol = 10,)
soi.seq = seq(1,1701,by = 170)
for(i in 1:9){
  soi.trans[,i] = soi[soi.seq[i]:(soi.seq[i+1]-1)]
}

soi.sum = matrix(NA,nrow = 10,ncol = 4)
colnames(soi.sum) = c("center","scale","lower","upper")
n = 170
for(i in 1:10){
  center = sum(soi.trans[2:170,i]*soi.trans[1:169,i]) / sum(soi.trans[1:169,i]^2)
  scale = (sum(soi.trans[2:170,i]^2 * soi.trans[1:169,i]^2) - (sum(soi.trans[2:170,i]*soi.trans[1:169,i])^2)/
             (sum(soi.trans[1:169,i]^2)^2)) / (168)
  lower = qt(.05,n-2)*scale + center
}
