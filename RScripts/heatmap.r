rm(list=ls())
setwd("/Volumes/GoogleDrive/My Drive/MACMARC GD/CONSULTANCY/CORONA/2020_UA_Corona_golf2_data_no clean")

#library(foreign)
#cordat=as.data.frame(read.spss("2020_UA_Corona_golf2_data_no clean.sav"))
library(haven)
library(brglm2)
cd=read_sav("Data/secondSurvey/2020_UA_Corona_golf2_data_no clean.sav") 
colnames(cd)


cd$Q29_1[is.na(cd$Q29_1)]=0
cd$Q29_2[is.na(cd$Q29_2)]=0
cd$Q29_3[is.na(cd$Q29_3)]=0
cd$Q29_4[is.na(cd$Q29_4)]=0
cd$Q29_5[is.na(cd$Q29_5)]=0
cd$Q29_6[is.na(cd$Q29_6)]=0
cd$Q29_7[is.na(cd$Q29_7)]=0
cd$Q29_8[is.na(cd$Q29_8)]=0
cd$Q29_9[is.na(cd$Q29_9)]=0
cd$Q29_10[is.na(cd$Q29_10)]=0
cd$Q29_11[is.na(cd$Q29_11)]=0
cd$Q29_12[is.na(cd$Q29_12)]=0
cd$Q29_13[is.na(cd$Q29_13)]=0
cd$Q36[is.na(cd$Q36)]=0
table(cd$Q36)

mat=matrix(NA,nrow=6,ncol=13)
row.names(mat)=c("COVID-19 bevestigd door labo (Q36=4)","COVID-19 niet bevestigd door labo (Q36=1)","COVID-19 of verkoudheid of griep, niet bevestigd door labo (Q36=2)","griep bevestigd door labo (Q36=3)","geen enkele van toepassing (Q36=5)","missing Q36")
colnames(mat)=c("Q19_1","Q19_2","Q19_5","Q19_4","Q19_7","Q19_9","Q19_10","Q19_11","Q19_12","Q19_13","Q19_3","Q19_6","Q19_8")
sel=(cd$Q36==4)
cdsel=cd[sel,]
mat[1,]=c(mean(cdsel$Q29_1),mean(cdsel$Q29_2),mean(cdsel$Q29_5),mean(cdsel$Q29_4),mean(cdsel$Q29_7),mean(cdsel$Q29_9),mean(cdsel$Q29_10),mean(cdsel$Q29_11),mean(cdsel$Q29_12),mean(cdsel$Q29_13),mean(cdsel$Q29_3),mean(cdsel$Q29_6),mean(cdsel$Q29_8))
sel=(cd$Q36==1)
cdsel=cd[sel,]
mat[2,]=c(mean(cdsel$Q29_1),mean(cdsel$Q29_2),mean(cdsel$Q29_5),mean(cdsel$Q29_4),mean(cdsel$Q29_7),mean(cdsel$Q29_9),mean(cdsel$Q29_10),mean(cdsel$Q29_11),mean(cdsel$Q29_12),mean(cdsel$Q29_13),mean(cdsel$Q29_3),mean(cdsel$Q29_6),mean(cdsel$Q29_8))
sel=(cd$Q36==2)
cdsel=cd[sel,]
mat[3,]=c(mean(cdsel$Q29_1),mean(cdsel$Q29_2),mean(cdsel$Q29_5),mean(cdsel$Q29_4),mean(cdsel$Q29_7),mean(cdsel$Q29_9),mean(cdsel$Q29_10),mean(cdsel$Q29_11),mean(cdsel$Q29_12),mean(cdsel$Q29_13),mean(cdsel$Q29_3),mean(cdsel$Q29_6),mean(cdsel$Q29_8))
sel=(cd$Q36==3)
cdsel=cd[sel,]
mat[4,]=c(mean(cdsel$Q29_1),mean(cdsel$Q29_2),mean(cdsel$Q29_5),mean(cdsel$Q29_4),mean(cdsel$Q29_7),mean(cdsel$Q29_9),mean(cdsel$Q29_10),mean(cdsel$Q29_11),mean(cdsel$Q29_12),mean(cdsel$Q29_13),mean(cdsel$Q29_3),mean(cdsel$Q29_6),mean(cdsel$Q29_8))
sel=(cd$Q36==5)
cdsel=cd[sel,]
mat[5,]=c(mean(cdsel$Q29_1),mean(cdsel$Q29_2),mean(cdsel$Q29_5),mean(cdsel$Q29_4),mean(cdsel$Q29_7),mean(cdsel$Q29_9),mean(cdsel$Q29_10),mean(cdsel$Q29_11),mean(cdsel$Q29_12),mean(cdsel$Q29_13),mean(cdsel$Q29_3),mean(cdsel$Q29_6),mean(cdsel$Q29_8))
sel=(cd$Q36==0)
cdsel=cd[sel,]
mat[6,]=c(mean(cdsel$Q29_1),mean(cdsel$Q29_2),mean(cdsel$Q29_5),mean(cdsel$Q29_4),mean(cdsel$Q29_7),mean(cdsel$Q29_9),mean(cdsel$Q29_10),mean(cdsel$Q29_11),mean(cdsel$Q29_12),mean(cdsel$Q29_13),mean(cdsel$Q29_3),mean(cdsel$Q29_6),mean(cdsel$Q29_8))
round(mat,3)

install.packages("RColorBrewer")
library("RColorBrewer")
heatmap(round(1-mat,3),scale="none",col = brewer.pal(11,"RdYlGn"), Colv=NA)


