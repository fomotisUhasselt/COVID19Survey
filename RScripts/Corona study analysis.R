rm(list=ls())

setwd("C:/Users/lucp2188/Documents/COVID19")

# Libraries
library(tidyverse)
library(haven)
library(INLA)
library(MASS)
library(zoo)
library(car)

########################################
######          ROUND 1          #######
########################################

# Data
data<-read_sav("data/2020_UA_Corona_study_wave 17-03-2020_first clean.sav")

# Data management (can be improved)

vragenlijst<-lapply(data, function (x) attr(x,"label"))

# I made these variables on the first day, just to investigate them one by one. 
# I did not find the time to write a code that automatically cleans them etc.

age<-as.numeric(data$Q2)
sex<-as.factor(ifelse(data$Q3==3,NA,data$Q3))
sex<-as.factor(ifelse(sex==2,"female","male"))
postcode<-data$Q4
diploma<-as.factor(data$Q27)
fam71<-as.numeric(data$Q5_1)
fam1870<-as.numeric(data$Q5_2)
fam1217<-as.numeric(data$Q5_3)
fam11<-as.numeric(data$Q5_4)
fam_child<-as.numeric(data$Q5_child)
fam_child_c<-as.factor(data$Q5_child_group)
contacts<-as.numeric(data$Q6)
date_stop<-data$Q7
date_stop_num<-as.numeric(data$Q7_date)
date_stop_num<-ifelse(date_stop_num>18338 | date_stop_num<18293,NA,date_stop_num)
date_stop_num_c<-date_stop_num-18293 #aantal dagen sinds 1/2/2020
employment<-as.factor(data$Q8)
telework<-as.factor(data$Q9)
num_telework<-as.numeric(data$Q10)
employment_type<-as.factor(data$Q11)
telework_once_last_week<-as.factor(ifelse(data$Q12==12,NA,data$Q12))
num_telework_last_week<-as.numeric(data$Q13)
telework_today<-as.factor(ifelse(data$Q14==17,NA,data$Q14))
no_telework_reason<-as.factor(data$Q15)
childcare<-as.factor(data$Q16)
flu_symp<-as.factor(ifelse(data$Q17==3,NA,data$Q17))
sindswanneergriep<-data$Q18
sindswanneerteruggezond<-data$Q19
hebjegriep<-as.factor(data$Q26)
fam_symp<-as.factor(ifelse(data$Q20==3,NA,data$Q20))
fam_symp<-as.factor(ifelse(fam_symp==2,"no","yes"))
adapt_fam<- as.numeric(data$Q22_1)
adapt_work<- as.numeric(data$Q22_2)
adapt_public<- as.numeric(data$Q22_3)
risk_group<- as.factor(data$Q21)
anxious<-as.factor(data$Q23)
afraid<-as.factor(data$Q24)
age_cat<-as.character(data$age_class)
age_cat<-sapply(1:length(age_cat), function(x) ifelse(age_cat[x]=="Uncategorized" & age[x]<10,"0-12 jaar",age_cat[x]))
age_cat<-as.factor(sapply(1:length(age_cat), function(x) ifelse(age_cat[x]=="Uncategorized" & age[x]>79,"+80 jaar",age_cat[x])))
province<-as.factor(data$province)
nis<-as.numeric(data$niscode)
community<-as.factor(data$community)
gezinmetkinderen<-as.numeric(data$`filter_$`)
fam71_cat<-as.factor(data$Q5_1_group)
fam1870_cat<-as.factor(data$Q5_2_group)
fam1217_cat<-as.factor(data$Q5_3_group)
fam11_cat<-as.factor(data$Q5_4_group)
fam_num<-as.numeric(data$Q5_sum)
fam_num_cat<-as.factor(data$Q5_sum_group)
single_hh<-as.factor(ifelse(data$Single_HH==1,1,0))

# gezond1703 indicates whether you were healthy on 17/03
# gezond1603 indicates whether you were healthy on 16/03

gezond1703<-ifelse(is.na(hebjegriep),1,hebjegriep)
gezond1703<-ifelse(gezond1703==2,0,1)
gezond1603<-sapply( 1:length(sindswanneergriep), function (x) ifelse(sindswanneergriep[x]=="17/03/2020",1,gezond1703[x]))
gezond1603<-sapply( 1:length(sindswanneerteruggezond), 
                    function (x) ifelse(sindswanneerteruggezond[x] %in% c("16/03/2020","17/03/2020"),0,gezond1603[x]))

# flu indicates whether persons had flu-like symptoms, were recovered from these symptom, or did not have any symptoms yet, on 16/03
flu<-as.factor(ifelse(data$Q17==3,NA,data$Q17))
flu<-sapply( 1:length(flu), function (x) ifelse(flu[x]==1,"Yes",flu[x]))
flu<-sapply( 1:length(flu), function (x) ifelse(flu[x]==2,"No",flu[x]))
flu<-sapply( 1:length(flu), function (x) ifelse(sindswanneergriep[x]=="17/03/2020","No",flu[x]))
flu<-sapply( 1:length(flu), function (x) ifelse(!(sindswanneerteruggezond[x] %in% c("","16/03/2020","17/03/2020")),"Recovered",flu[x]))

# This is a long way to clean a variable to obtain information about the day of onset of flu-like symptoms
sindswanneergriep2<-ifelse(grepl("/20", sindswanneergriep)==T,sindswanneergriep,NA)
sindswanneergriep2<-gsub("/20", "/2020", sindswanneergriep2)
sindswanneergriep2<-gsub("/202020", "/2020", sindswanneergriep2)
sindswanneergriep2<-ifelse(grepl("2019", sindswanneergriep2)==T,NA,sindswanneergriep2)
sindswanneergriep2<-ifelse(grepl("2015", sindswanneergriep2)==T,NA,sindswanneergriep2)
sindswanneergriep2<-gsub("/1/", "/01/", sindswanneergriep2)
sindswanneergriep2<-gsub("/2/", "/02/", sindswanneergriep2)
sindswanneergriep2<-gsub("/3/", "/03/", sindswanneergriep2)
sindswanneergriep2<-gsub("20202", "2020", sindswanneergriep2)
sindswanneergriep2<-gsub("20209", NA, sindswanneergriep2)
sindswanneergriep2<-gsub(" ", "", sindswanneergriep2)
sindswanneergriep2<-ifelse(substring(sindswanneergriep2,1,2)=="1/",gsub("1/0", "01/0", sindswanneergriep2),sindswanneergriep2)
sindswanneergriep2<-ifelse(substring(sindswanneergriep2,1,2)=="2/",gsub("2/0", "02/0", sindswanneergriep2),sindswanneergriep2)
sindswanneergriep2<-ifelse(substring(sindswanneergriep2,1,2)=="3/",gsub("3/0", "03/0", sindswanneergriep2),sindswanneergriep2)
sindswanneergriep2<-ifelse(substring(sindswanneergriep2,1,2)=="4/",gsub("4/0", "04/0", sindswanneergriep2),sindswanneergriep2)
sindswanneergriep2<-ifelse(substring(sindswanneergriep2,1,2)=="5/",gsub("5/0", "05/0", sindswanneergriep2),sindswanneergriep2)
sindswanneergriep2<-ifelse(substring(sindswanneergriep2,1,2)=="6/",gsub("6/0", "06/0", sindswanneergriep2),sindswanneergriep2)
sindswanneergriep2<-ifelse(substring(sindswanneergriep2,1,2)=="7/",gsub("7/0", "07/0", sindswanneergriep2),sindswanneergriep2)
sindswanneergriep2<-ifelse(substring(sindswanneergriep2,1,2)=="8/",gsub("8/0", "08/0", sindswanneergriep2),sindswanneergriep2)
sindswanneergriep2<-ifelse(substring(sindswanneergriep2,1,2)=="9/",gsub("9/0", "09/0", sindswanneergriep2),sindswanneergriep2)
sindswanneergriep2<-gsub("[^0-9/]", "", sindswanneergriep2)
sindswanneergriep2<-ifelse(nchar(sindswanneergriep2)!=10,NA,sindswanneergriep2)
days_since_onset_flu<-as.numeric(as.Date("16/03/2020",format = "%d/%m/%Y")-as.numeric(as.Date(sindswanneergriep2,format = "%d/%m/%Y")))+1
days_since_onset_flu<-sapply( 1:length(days_since_onset_flu), function (x) ifelse(flu[x]=="Yes",days_since_onset_flu[x],NA))
days_since_onset_flu<-ifelse(days_since_onset_flu>7,NA,days_since_onset_flu)
flu_onset<-as.numeric(as.Date(sindswanneergriep2,format = "%d/%m/%Y"))
flu_onset<-ifelse(flu_onset>as.numeric(as.Date("09/03/2020",format = "%d/%m/%Y")),flu_onset,NA)
flu_onset<-flu_onset-as.numeric(as.Date("10/03/2020",format = "%d/%m/%Y"))

# I know that it looks like I've just started reading my first "coding in R" book, 
# but due to limited time, I did not have the time to do it very efficiently.

data2<-data.frame(postcode,
                  nis,
                  community,
                  date_stop,
                  date_stop_num,
                  date_stop_num_c,
                  sindswanneergriep,
                  sindswanneerteruggezond,
                  contacts,
                  gezond1603,
                  age,
                  sex,
                  diploma,
                  fam71,
                  fam1870,
                  fam1217,
                  fam11,
                  fam_child,
                  fam_child_c,
                  employment,
                  telework,
                  num_telework,
                  employment_type,
                  days_since_onset_flu,
                  telework_once_last_week,
                  num_telework_last_week,
                  telework_today,
                  no_telework_reason,
                  childcare,
                  flu_symp,
                  flu,
                  flu_onset,
                  hebjegriep,
                  fam_symp,
                  adapt_fam,
                  adapt_work,
                  adapt_public,
                  risk_group,
                  anxious,
                  afraid,
                  age_cat,
                  province,
                  gezinmetkinderen,
                  fam71_cat,
                  fam1870_cat,
                  fam1217_cat,
                  fam11_cat,
                  fam_num,
                  fam_num_cat,
                  single_hh)

### Press figure

date15<-date_stop_num_c[date_stop_num_c>=15]
age15<-age_cat[date_stop_num_c>=15]

tiff("Fig_age1.tiff", units="in", width=14*0.8, height=10*0.8, res=300)
plot(density(date15[age15%in%c("0-12 jaar","13-17 jaar")], adjust = 7, na.rm=T,from=15,to=45),main=" ",
     ylab=" " , xlab="Datum",cex.lab=1.5, xlim = c(15,45), lwd = 2, col = "Blue", xaxt="n", zero.line = FALSE, axes=FALSE)
axis(1, at=c(15 , 45), labels=c("",""), lwd.ticks=0)
title(main = ("Wanneer gaven Belgen voor het laatst een hand of zoen aan iemand anders dan een huisgenoot?"), cex.main = 1.3)
#axis(2, at=c(0 , 0.15), labels=c("",""), lwd.ticks=0)
axis(1, at=sort((c(seq(15 , 40, by=5),38,44))+0.5),cex.axis=1.3, label = c("17/2","22/2","27/2","3/3","8/3","11/3","13/3","17/3"))
lines(density(date15[age15%in%c("+80 jaar","66-80 jaar")], adjust = 7, na.rm=T,from=15,to=45), lwd = 2, col = "Red")
lines(density(date15[age15%in%c("18-45 jaar","46-65 jaar")], adjust = 7, na.rm=T,from=15,to=45), lwd = 2, col = "Green")
legend(16, 0.11, legend=c("0-17 jaar", "18-65 jaar", "+65 jaar","maatregelen"),
       col=c("blue", "green","red","black"), lty=c(1,1,1,2), cex=1.5, lwd = 2)
abline(v=(as.numeric(as.Date("11/03/2020",format = "%d/%m/%Y"))-as.numeric(as.Date("01/02/2020",format = "%d/%m/%Y"))-0.5), col="black",lty=2, lwd = 2 )
abline(v=(as.numeric(as.Date("13/03/2020",format = "%d/%m/%Y"))-as.numeric(as.Date("01/02/2020",format = "%d/%m/%Y"))-0.5), col="black",lty=2, lwd = 2)
abline(v=(as.numeric(as.Date("17/03/2020",format = "%d/%m/%Y"))-as.numeric(as.Date("01/02/2020",format = "%d/%m/%Y"))-0.5), col="black",lty=2, lwd = 2)
text(37.6+0.5,0.115,"1",cex=1.5)
text(39.6+0.5,0.115,"2",cex=1.5)
text(43.6+0.5,0.115,"3",cex=1.5)
dev.off()


### Statistical analysis
# Often model assumptions are not met and analysis approach is rather exploratory. This should be improved.
# These models were often based on ad hoc questions by members of the team.

# Contacts

analysis1 <- lapply(10:dim(data2)[2], function(x) summary(glm.nb(data2$contacts~ data2[,x]) )$coefficients)
names(analysis1)<-names(data2[10:length(names(data2))])

analysis2<-glm.nb(contacts~ anxious + gezond1603*sex + age_cat + fam_symp, data=data2 )
summary(analysis2)

analysis3<-lm(date_stop_num_c~ anxious + flu*sex + age_cat + fam_symp, data=data2 )
summary(analysis3)

data2b<-data2[data2$contacts<1000,]

analysis2<-glm.nb(contacts~ anxious*days_since_onset_flu + anxious
                  + days_since_onset_flu*sex + days_since_onset_flu + age_cat +
                    days_since_onset_flu*fam_symp + fam_symp, data=data2b )
summary(analysis2)

# Days until stopping giving hands and/or kisses

data3<-data2[data2$date_stop_num>18330,]
data3$date_stop_num_c<-data3$date_stop_num-18331

analysis4<-lm(log(date_stop_num_c)~ anxious + flu*sex + age_cat + fam_symp, data=data3 )
summary(analysis4)

analysis4<-lm(date_stop_num_c~  sex + flu_onset*fam_symp, data=data3 )
summary(analysis4)

# The perception of how well colleagues adapt to social distancing measures

data4<-data2[data2$employment_type%in%c("2","3","4","5"),]
data4$employment_type<-as.character(data4$employment_type)
data4$employment_type<-as.factor(data4$employment_type)
data4$leidinggevend<-ifelse(data4$employment_type%in%c("4","5"),"Yes","No")

analysis5<-lm(adapt_work~sex*leidinggevend, data=data4 )
summary(analysis5)


########################################
######          ROUND 2          #######
########################################

# Data
data_n<-read_sav("data/2020_UA_Corona_golf2_data_no clean.sav")

# Data management (can be improved)

vragenlijst2<-lapply(data_n, function (x) attr(x,"label"))

data_n$Q36<-ifelse(is.na(data_n$Q36),0,data_n$Q36)
#data_n<-data_n[data_n$Q36!=3,] # eventueel mensen waarvan we weten dat ze geen covid hebben uit analyse halen (hangt af van welke vraag wordt gesteld)
data_n<-data_n[data_n$Q4%in%c(1,2),]
data_n$male<-ifelse(data_n$Q4==1,1,0)# male=1, female=0
data_n$covid<-ifelse(data_n$Covid_symptoms>0,1,0)

#Number of contacts outside household
data<-data_n # note that this comes from separate analyses for each round. Hence the "data" will overwrite the data from round 1.
data$nr.contacts=data$hh.contacts1=data$hh.contacts2=data$hh.contacts3=data$hh.contacts4=rep(NA,length(data$Q9))
data$nr.contacts[data$Q9==1]=0
data$nr.contacts[data$Q9==22]=21
data$nr.contacts[data$Q62==1]=0
data$nr.contacts[data$Q62==22]=21

for(i in 2:21){
  data$nr.contacts[data$Q9==paste(i)]=i-1;
  data$nr.contacts[data$Q62==paste(i)]=i-1}

# Number of contacts in household

data$hh.contacts1[data$Q8_1_1==0]=0
data$hh.contacts1[data$Q8_1_1==11]=11

for(i in 1:10){
  data$hh.contacts1[data$Q8_1_1==paste(i)]=i}

data$hh.contacts2[data$Q8_1_2==0]=0
data$hh.contacts2[data$Q8_1_2==11]=11

for(i in 1:10){
  data$hh.contacts2[data$Q8_1_2==paste(i)]=i}

data$hh.contacts3[data$Q8_1_3==0]=0
data$hh.contacts3[data$Q8_1_3==11]=11

for(i in 1:10){
  data$hh.contacts3[data$Q8_1_3==paste(i)]=i}

data$hh.contacts4[data$Q8_1_4==0]=0
data$hh.contacts4[data$Q8_1_4==11]=11

for(i in 1:10){
  data$hh.contacts4[data$Q8_1_4==paste(i)]=i}

data$hh.contacts=data$hh.contacts1+data$hh.contacts2+data$hh.contacts3+data$hh.contacts4

data$hh.contacts.cens=0
data$hh.contacts.cens=((data$hh.contacts1==11)+(data$hh.contacts2==11)+(data$hh.contacts3==11)+(data$hh.contacts4==11))>0

data$hh.contacts[data$Q7==1]=0
data$hh.contacts.cens[data$Q7==1]=0

tmp=((data$nr.contacts==21)+(data$hh.contacts.cens!=0))>0

data$contacts<-data$nr.contacts+data$hh.contacts

data$hh.contacts.high.1<-ifelse(is.na(ifelse(data$Q8_1_1==11,1,0)),0,ifelse(data$Q8_1_1==11,1,0))
data$hh.contacts.high.2<-ifelse(is.na(ifelse(data$Q8_1_2==11,1,0)),0,ifelse(data$Q8_1_2==11,1,0))
data$hh.contacts.high.3<-ifelse(is.na(ifelse(data$Q8_1_3==11,1,0)),0,ifelse(data$Q8_1_3==11,1,0))
data$hh.contacts.high.4<-ifelse(is.na(ifelse(data$Q8_1_4==11,1,0)),0,ifelse(data$Q8_1_4==11,1,0))
data$hh.contacts.high.sum<-ifelse(data$hh.contacts.high.1+data$hh.contacts.high.2+data$hh.contacts.high.3+data$hh.contacts.high.4>0,1,0)

hist(data$nr.contacts+data$hh.contacts,breaks=seq(0,62),right=F,main="number of contacts (censored values included)")
hist((data$nr.contacts+data$hh.contacts)[!tmp],breaks=seq(0,62),right=F,main="number of contacts (censored values included)")

data$male<-ifelse(data$Q4==1,1,0)# male=1, female=0
data$male<-ifelse(data$Q4==3,NA,data$male)

data$age<-data_n$Q3
data$age_cat<-ifelse(data$age<13,"0-12 jaar",
                     ifelse(data$age>12 & data$age<18,"13-17 jaar",
                            ifelse(data$age>17 & data$age<46,"18-45 jaar",
                                   ifelse(data$age>45 & data$age<66,"46-65 jaar",
                                          ifelse(data$age>65 & data$age<81,"66-80 jaar","+80 jaar")))))
data$age_cat<-ifelse(is.na(data$age),NA, data$age_cat)

symp<-data.frame(data$Q37_1_1_1,data$Q37_1_1_2,data$Q37_1_1_3,data$Q37_1_1_4,
                 data$Q37_1_2_1,data$Q37_1_2_2,data$Q37_1_2_3,data$Q37_1_2_4,
                 data$Q37_1_3_1,data$Q37_1_3_2,data$Q37_1_3_3,data$Q37_1_3_4,
                 data$Q37_1_4_1,data$Q37_1_4_2,data$Q37_1_4_3,data$Q37_1_4_4,
                 data$Q37_1_5_1,data$Q37_1_5_2,data$Q37_1_5_3,data$Q37_1_5_4,
                 data$Q37_1_6_1,data$Q37_1_6_2,data$Q37_1_6_3,data$Q37_1_6_4,
                 data$Q37_1_7_1,data$Q37_1_7_2,data$Q37_1_7_3,data$Q37_1_7_4,
                 data$Q37_1_8_1,data$Q37_1_8_2,data$Q37_1_8_3,data$Q37_1_8_4)

symp<-ifelse(is.na(symp),0,1)
symp$fam_symp<-rowSums(symp)
symp$fam_symp2<-ifelse(as.numeric(symp$fam_symp)>0,1,0)
data$fam_symp<-as.factor(ifelse(symp$fam_symp2==0,"no","yes"))

risk<-data.frame(data$Q40_1,data$Q40_2,data$Q40_3,data$Q40_4,
                 data$Q40_5,data$Q40_6,data$Q40_7)
risk<-ifelse(is.na(risk),0,1)
risk$risk_sum<-rowSums(risk)
risk$risk2<-ifelse(as.numeric(risk$risk_sum)>0,1,0)
data$risk<-as.factor(ifelse(risk$risk2==0,"no","yes"))

data$Q42<-data$Q42
data$Q43<-data$Q43
data$mentaal<-data$Q42+data$Q43+data$Q44+data$Q45+data$Q46+data$Q47+data$Q48+data$Q49+data$Q50+data$Q51+data$Q52+data$Q53+data$Q54
ment.quant<-quantile(data$mentaal[!is.na(data$mentaal)])
as.numeric(ment.quant[2])
data$mentaal_cat<-ifelse(data$mentaal<as.numeric(ment.quant[2]),"1",
                         ifelse(data$mentaal>=as.numeric(ment.quant[2]) & data$mentaal<as.numeric(ment.quant[3]),"2",
                                ifelse(data$mentaal>=as.numeric(ment.quant[3]) & data$mentaal<as.numeric(ment.quant[4]),"3","4")))
data$mentaal_cat<-as.factor(ifelse(is.na(data$mentaal),NA, data$mentaal_cat))
data$anxious<-as.factor(data$Q55)
data$afraid<-as.factor(data$Q56)

buiten<-data.frame(data$Q58_1,data$Q58_2)
buiten<-ifelse(is.na(buiten),0,1)
buiten$buiten_sum<-rowSums(buiten)
buiten$buiten2<-ifelse(as.numeric(buiten$buiten_sum)>0,1,0)
data$buiten<-as.factor(ifelse(buiten$buiten2==0,"no","yes"))
data$buiten_actief<-data$buiten
data$buiten_praten<-ifelse(is.na(data$Q58_5),0,1)
data$buiten_praten<-as.factor(ifelse(data$buiten_praten==0,"no","yes"))

sindswanneergriep<-as.Date(data$Q30,format = "%d/%m/%Y")
sindswanneerteruggezond<-as.Date(data$Q32,format = "%d/%m/%Y")
sindswanneergriep<-as.Date(ifelse(as.numeric(sindswanneergriep)>as.numeric(as.Date("01/03/2020",format = "%d/%m/%Y")),sindswanneergriep,NA))
sindswanneergriep<-as.Date(ifelse(as.numeric(sindswanneergriep)<as.numeric(as.Date("24/03/2020",format = "%d/%m/%Y")),sindswanneergriep,NA))
sindswanneerteruggezond<-as.Date(ifelse(as.numeric(sindswanneerteruggezond)>as.numeric(as.Date("01/03/2020",format = "%d/%m/%Y")),sindswanneerteruggezond,NA))
sindswanneerteruggezond<-as.Date(ifelse(as.numeric(sindswanneerteruggezond)<as.numeric(as.Date("24/03/2020",format = "%d/%m/%Y")),sindswanneerteruggezond,NA))
ziekop24maart<-ifelse(data$Q31==1,1,0)
ziekop23maart<-ifelse(!(is.na(sindswanneergriep)) & ziekop24maart==1,1,0)
genezenop23maart<-ifelse(!(is.na(sindswanneergriep)) &!(is.na(sindswanneerteruggezond))==1,1,0)
statusop23maart<-ifelse(ziekop23maart==1,"ziek",ifelse(genezenop23maart==1,"genezen","gezond"))
days_since_onset_flu<-ifelse(ziekop23maart==1,as.numeric(as.Date("23/03/2020",format = "%d/%m/%Y"))-as.numeric(sindswanneergriep),NA)

#### Statistical analysis

# contacts

analysis2a<-glm.nb(contacts~ age_cat, data=data )
summary(analysis2a)

analysis2b<-glm.nb(contacts~ male, data=data )
summary(analysis2b)

analysis2c<-glm.nb(contacts~ fam_symp, data=data )
summary(analysis2c)

analysis2d<-glm.nb(contacts~ risk, data=data )
summary(analysis2d)

analysis2e<-glm.nb(contacts~ mentaal_cat, data=data )
summary(analysis2e)

analysis2f<-glm.nb(contacts~ afraid, data=data )
summary(analysis2f)

analysis2g<-glm.nb(contacts~ anxious, data=data )
summary(analysis2g)

analysis2h<-glm.nb(contacts~ symp_status, data=data )
summary(analysis2h)

analysis2i<-glm.nb(contacts~ days_since_onset_symp, data=data )
summary(analysis2i)

analysis2j<-glm.nb(contacts~ buiten, data=data )
summary(analysis2j)

analysis2k<-glm.nb(contacts~ buiten_praten + buiten_actief + symp_status + mentaal_cat + risk + fam_symp + male + age_cat, data=data )

analysis2m<-glm.nb(contacts~ buiten_praten*age_cat + male*age_cat + 
                     buiten_praten*male + symp_status*male + mentaal_cat*male + risk*male + fam_symp*male +
                     buiten_praten*fam_symp  + symp_status*fam_symp + mentaal_cat*fam_symp + risk*fam_symp +
                     buiten_actief*risk + symp_status*risk +
                     buiten_praten*mentaal_cat + symp_status*mentaal_cat +
                     buiten_actief*buiten_praten, data=data )

summary(analysis2m)

analysis2m2<-glm.nb(contacts~ buiten_praten*age_cat + male*age_cat + 
                      buiten_praten*male + mentaal_cat*male + risk*male + fam_symp*male +
                      buiten_praten*fam_symp  + symp_status*fam_symp + mentaal_cat*fam_symp + risk*fam_symp +
                      buiten_actief*risk + symp_status*risk +
                      buiten_praten*mentaal_cat + buiten_actief*buiten_praten, data=data )

summary(analysis2m2)

(est <- cbind(Estimate = coef(analysis2m2), confint(analysis2m2))) # ll profiling takes long; use confint.default() for wald if needed
exp(est)


analysis2k<-glm.nb(contacts~  male*age_cat + mentaal_cat*male + risk*male + fam_symp*male  
                   + buiten*fam_symp + mentaal_cat*fam_symp
                   + symp_status*risk, data=data )
summary(analysis2k)


analysis2l<-glm.nb(nr.contacts~age_cat+ mentaal_cat*male + fam_symp*male  
                   + buiten + fam_symp
                   + symp_status*risk, data=data )

# not a fan of many interactions, but just an attempt to find trends...

analysis2l<-glm.nb(nr.contacts~ buiten_praten*age_cat + buiten_actief*age_cat + symp_status*age_cat + mentaal_cat*age_cat + risk*age_cat + fam_symp*age_cat + male*age_cat +
                     buiten_praten*male + buiten_actief*male + symp_status*male + mentaal_cat*male + risk*male + fam_symp*male +
                     buiten_praten*fam_symp + buiten_actief*fam_symp + symp_status*fam_symp + mentaal_cat*fam_symp + risk*fam_symp +
                     buiten_praten*risk + buiten_actief*risk + symp_status*risk + mentaal_cat*risk +
                     buiten_praten*mentaal_cat + buiten_actief*mentaal_cat + symp_status*mentaal_cat +
                     buiten_praten*symp_status + buiten_actief*symp_status + buiten_praten*buiten_actief, data=data )

analysis2l<-glm.nb(nr.contacts~ buiten_praten*age_cat +
                     mentaal_cat*male + risk*male + fam_symp*male +
                     symp_status*fam_symp + risk*fam_symp +
                     buiten_actief*risk  + 
                     buiten_praten*mentaal_cat +
                     buiten_praten*buiten_actief, data=data )

analysis2l<-glm.nb(nr.contacts~ buiten_actief, data=data )

(est2 <- cbind(Estimate = coef(analysis2l), confint(analysis2l)))
exp(est2)

table(data$hh.contacts.high.sum,data$age_cat)
table(data$hh.contacts.high.sum,data$male)
table(data$hh.contacts.high.sum,data$buiten)

# research into dynamics when people live in large households (question from Niel)

data$ziek<-ifelse(data$symp_status=="ziek",1,0)
analysis3a<-glm(ziek ~ hh.contacts.high.sum +  buiten_praten + buiten_actief + mentaal_cat + risk + fam_symp + male + age_cat, family=binomial, data=data )
summary(analysis3a)

# analysis mental health, based on the so called GHQ-12 score (=WB_1)
# note that before this question was asked, I defined my own mental health variable (mentaal_cat)
# It is better to use WB_1 instead of mentaal_cat (as well as a covariate) in the future

mentaal<-data.frame(data$Q42,data$Q43,data$Q44,data$Q45,data$Q46,data$Q47,data$Q48,data$Q49,data$Q50,data$Q51,data$Q52,data$Q53)
mentaal2<-data.frame(data$Q42,data$Q43,data$Q44,data$Q45,data$Q46,data$Q47,data$Q48,data$Q49,data$Q50,data$Q51,data$Q52,data$Q53)

for(i in 1:dim(mentaal)[2]){
  mentaal[,i]<-sapply(1:length(as.numeric(mentaal[,i])), function(x) ifelse(mentaal[x,i]%in%c(1,2),0,ifelse(mentaal[x,i]%in%c(3,4),1,NA)))}
data$WB_1<-rowSums(mentaal)
data$WB_2<-ifelse(data$WB_1>2,1,0)
data$WB_3<-ifelse(data$WB_1>4,1,0)

data$arbeid<-factor(ifelse(data$Q11==1,"geen",ifelse(data$Q11==2,"deeltijds","voltijds")),levels=c("geen","deeltijds","voltijds"))
data$diploma<-haven::as_factor(data$Q6)

model1<-lm(WB_1~ age_cat + male + covid + fam_symp + hh.contacts + arbeid + diploma ,data=data )
summary(model1)

par(mfrow=c(2,2))
plot(model1) # horrible :-), but I did not have time to improve the model"
Anova(model1,type=3)
dev.off()

model2<-glm(WB_2 ~ age_cat + male + covid + fam_symp + hh.contacts*diploma + arbeid, family=binomial, data=data )
summary(model2)

(est4 <- cbind(Estimate = coef(model2), confint(model2)))
exp(est4)

model3<-glm(WB_3 ~ age_cat + male + covid + fam_symp + hh.contacts + diploma + arbeid, family=binomial, data=data )
summary(model3)

(est5 <- cbind(Estimate = coef(model3), confint(model3)))
exp(est5)

# analysis relational problems

relatie<-data.frame(data$Q57_1,data$Q57_2,data$Q57_3,data$Q57_4,data$Q57_5)
relatie2<-data.frame(data$Q57_1,data$Q57_2,data$Q57_3,data$Q57_4,data$Q57_5)

for(i in 1:dim(relatie)[2]){
  relatie[,i]<-sapply(1:length(as.numeric(relatie[,i])), function(x) ifelse(relatie[x,i]%in%c(1,2),0,ifelse(relatie[x,i]%in%c(3,4),1,NA)))}

relatie3<-relatie

for(i in 1:dim(relatie3)[2]){
  relatie3[,i]<-sapply(1:length(as.numeric(relatie3[,i])), function(x) ifelse(is.na(relatie3[x,i]),0,relatie3[x,i]))}

data$rel.prob<-ifelse(rowSums(relatie3)>0,1,ifelse(rowSums(relatie3)==0,0,NA))
data$rel.prob_70_plus<-relatie3[,1]
data$rel.prob_18_70<-relatie3[,2]
data$rel.prob_12_18<-relatie3[,3]
data$rel.prob_0_12<-relatie3[,4]
data$rel.prob_partner_fam<-relatie3[,5]

data$single<-ifelse(data$hh.contacts==0,1,0) # after I did the analyses, it was mentioned that "single" might be better than "hh.contacts", but that might depend on the analysis

model4<-glm(rel.prob ~ age_cat + male + covid + fam_symp + hh.contacts*diploma + arbeid, family=binomial, data=data )
summary(model4)

(est6 <- cbind(Estimate = coef(model4), confint.default(model4)))
exp(est6)

model5<-glm(rel.prob_70_plus ~ age_cat + male + covid + fam_symp + hh.contacts*diploma + arbeid, family=binomial, data=data )
summary(model5)

(est7 <- cbind(Estimate = coef(model5), confint.default(model5)))
exp(est7)

model6<-glm(rel.prob_18_70 ~ age_cat + male + covid + fam_symp + hh.contacts*diploma + arbeid, family=binomial, data=data )
summary(model6)

(est8 <- cbind(Estimate = coef(model6), confint.default(model6)))
exp(est8)

model7<-glm(rel.prob_12_18 ~ age_cat + male + covid + fam_symp + hh.contacts*diploma + arbeid, family=binomial, data=data )
summary(model7)

(est9 <- cbind(Estimate = coef(model7), confint.default(model7)))
exp(est9)

model8<-glm(rel.prob_0_12 ~ age_cat + male + fam_symp + hh.contacts*diploma + arbeid, family=binomial, data=data )
summary(model8)

(est10 <- cbind(Estimate = coef(model8), confint.default(model8)))
exp(est10)

model9<-glm(rel.prob_partner_fam ~ age_cat + male + covid + fam_symp + hh.contacts + diploma + arbeid, family=binomial, data=data )
summary(model9)

(est11 <- cbind(Estimate = coef(model9), confint.default(model9)))
exp(est11)

# I ran into memory allocation problems, so I saved a part of the dataset in order to combine it with that from week 3
data_round2<-data[,224:264]
write.table(data_round2, file = "data_round2.txt", row.names = F)

########################################
######          ROUND 3          #######
########################################

# Data
data_n3<-read_sav("data/2020_UA_Corona_golf3_data_no clean.sav")
vragenlijst3<-lapply(data_n3, function (x) attr(x,"label"))

data_n3$nr.contacts=data_n3$hh.contacts1=data_n3$hh.contacts2=data_n3$hh.contacts3=data_n3$hh.contacts4=rep(NA,length(data_n3$Q9))
data_n3$nr.contacts[data_n3$Q9==1]=0
data_n3$nr.contacts[data_n3$Q9==22]=21
data_n3$nr.contacts[data_n3$Q62==1]=0
data_n3$nr.contacts[data_n3$Q62==22]=21

for(i in 2:21){
  data_n3$nr.contacts[data_n3$Q9==paste(i)]=i-1;
  data_n3$nr.contacts[data_n3$Q62==paste(i)]=i-1}

# Number of contacts in household

data_n3$hh.contacts1[data_n3$Q64==0]=0
data_n3$hh.contacts1[data_n3$Q64==11]=11

for(i in 1:10){
  data_n3$hh.contacts1[data_n3$Q64==paste(i)]=i}

data_n3$hh.contacts2[data_n3$Q65==0]=0
data_n3$hh.contacts2[data_n3$Q65==11]=11

for(i in 1:10){
  data_n3$hh.contacts2[data_n3$Q65==paste(i)]=i}

data_n3$hh.contacts3[data_n3$Q66==0]=0
data_n3$hh.contacts3[data_n3$Q66==11]=11

for(i in 1:10){
  data_n3$hh.contacts3[data_n3$Q66==paste(i)]=i}

data_n3$hh.contacts4[data_n3$Q67==0]=0
data_n3$hh.contacts4[data_n3$Q67==11]=11

for(i in 1:10){
  data_n3$hh.contacts4[data_n3$Q67==paste(i)]=i}

data_n3$hh.contacts=data_n3$hh.contacts1+data_n3$hh.contacts2+data_n3$hh.contacts3+data_n3$hh.contacts4

data_n3$hh.contacts.cens=0
data_n3$hh.contacts.cens=((data_n3$hh.contacts1==11)+(data_n3$hh.contacts2==11)+(data_n3$hh.contacts3==11)+(data_n3$hh.contacts4==11))>0

data_n3$hh.contacts[data_n3$Q7==1]=0
data_n3$hh.contacts.cens[data_n3$Q7==1]=0

tmp=((data_n3$nr.contacts==21)+(data_n3$hh.contacts.cens!=0))>0

data_n3$contacts<-data_n3$nr.contacts+data_n3$hh.contacts

data_n3$hh.contacts.high.1<-ifelse(is.na(ifelse(data_n3$Q64==11,1,0)),0,ifelse(data_n3$Q64==11,1,0))
data_n3$hh.contacts.high.2<-ifelse(is.na(ifelse(data_n3$Q65==11,1,0)),0,ifelse(data_n3$Q65==11,1,0))
data_n3$hh.contacts.high.3<-ifelse(is.na(ifelse(data_n3$Q66==11,1,0)),0,ifelse(data_n3$Q66==11,1,0))
data_n3$hh.contacts.high.4<-ifelse(is.na(ifelse(data_n3$Q67==11,1,0)),0,ifelse(data_n3$Q67==11,1,0))

data_n3$hh.contacts.high.sum<-ifelse(data_n3$hh.contacts.high.1+data_n3$hh.contacts.high.2+data_n3$hh.contacts.high.3+data_n3$hh.contacts.high.4>0,1,0)

hist(data_n3$nr.contacts+data_n3$hh.contacts,breaks=seq(0,100),right=F,main="number of contacts (censored values included)")
hist((data_n3$nr.contacts+data_n3$hh.contacts)[!tmp],breaks=seq(0,62),right=F,main="number of contacts (censored values included)")

data_n3$covid<-ifelse(data_n3$COVID_symptoms>0,1,0)
data_n3$male<-ifelse(data_n3$Q4==1,1,0)# male=1, female=0
data_n3$male<-ifelse(data_n3$Q4==3,NA,data_n3$male)

data_n3$age<-data_n3$Q3
data_n3$age_cat<-ifelse(data_n3$age<13,"0-12 jaar",
                        ifelse(data_n3$age>12 & data_n3$age<18,"13-17 jaar",
                               ifelse(data_n3$age>17 & data_n3$age<46,"18-45 jaar",
                                      ifelse(data_n3$age>45 & data_n3$age<66,"46-65 jaar",
                                             ifelse(data_n3$age>65 & data_n3$age<81,"66-80 jaar","+80 jaar")))))
data_n3$age_cat<-ifelse(is.na(data_n3$age),NA, data_n3$age_cat)

symp3<-data.frame(data_n3$Q75_1,data_n3$Q75_2,data_n3$Q75_3,data_n3$Q75_4,
                  data_n3$Q79_1,data_n3$Q79_2,data_n3$Q79_3,data_n3$Q79_4,
                  data_n3$Q82_1,data_n3$Q82_2,data_n3$Q82_3,data_n3$Q82_4,
                  data_n3$Q85_1,data_n3$Q85_2,data_n3$Q85_3,data_n3$Q85_4)

symp3<-ifelse(is.na(symp3),0,1)
symp3$fam_symp<-rowSums(symp3)
symp3$fam_symp2<-ifelse(as.numeric(symp3$fam_symp)>0,1,0)
data_n3$fam_symp<-as.factor(ifelse(symp3$fam_symp2==0,"no","yes"))

data_n3$arbeid<-factor(ifelse(data_n3$Q11==1,"geen",ifelse(data_n3$Q11==2,"deeltijds","voltijds")),levels=c("geen","deeltijds","voltijds"))
data_n3$diploma<-haven::as_factor(data_n3$Q6)
levels(data_n3$diploma)[1] <- c("Lager onderwijs")

data_n3$single<-ifelse(data_n3$hh.contacts==0,1,0)

mentaal3<-data.frame(data_n3$Q42,data_n3$Q43,data_n3$Q44,data_n3$Q45,data_n3$Q46,data_n3$Q47,data_n3$Q48,data_n3$Q49,data_n3$Q50,data_n3$Q51,data_n3$Q52,data_n3$Q53)
mentaal23<-data.frame(data_n3$Q42,data_n3$Q43,data_n3$Q44,data_n3$Q45,data_n3$Q46,data_n3$Q47,data_n3$Q48,data_n3$Q49,data_n3$Q50,data_n3$Q51,data_n3$Q52,data_n3$Q53)

for(i in 1:dim(mentaal3)[2]){
  mentaal3[,i]<-sapply(1:length(as.numeric(mentaal3[,i])), function(x) ifelse(mentaal3[x,i]%in%c(1,2),0,ifelse(mentaal3[x,i]%in%c(3,4),1,NA)))}
data_n3$WB_1<-rowSums(mentaal3)
data_n3$WB_2<-ifelse(data_n3$WB_1>2,1,0)
data_n3$WB_3<-ifelse(data_n3$WB_1>4,1,0)

relatie<-data.frame(data_n3$Q57_1,data_n3$Q57_2,data_n3$Q57_3,data_n3$Q57_4,data_n3$Q57_5)
relatie2<-data.frame(data_n3$Q57_1,data_n3$Q57_2,data_n3$Q57_3,data_n3$Q57_4,data_n3$Q57_5)

for(i in 1:dim(relatie)[2]){
  relatie[,i]<-sapply(1:length(as.numeric(relatie[,i])), function(x) ifelse(relatie[x,i]%in%c(1,2),0,ifelse(relatie[x,i]%in%c(3,4),1,NA)))}

relatie3<-relatie

for(i in 1:dim(relatie3)[2]){
  relatie3[,i]<-sapply(1:length(as.numeric(relatie3[,i])), function(x) ifelse(is.na(relatie3[x,i]),0,relatie3[x,i]))}

data_n3$rel.prob<-ifelse(rowSums(relatie3)>0,1,ifelse(rowSums(relatie3)==0,0,NA))
data_n3$rel.prob_70_plus<-relatie3[,1]
data_n3$rel.prob_18_70<-relatie3[,2]
data_n3$rel.prob_12_18<-relatie3[,3]
data_n3$rel.prob_0_12<-relatie3[,4]
data_n3$rel.prob_partner_fam<-relatie3[,5]

data_n3$geenhand<-ifelse(data_n3$Q10==1,1,0)

data_n3$age_cat2<-ifelse(data_n3$age<13,"0-12 jaar",
                         ifelse(data_n3$age>12 & data_n3$age<18,"13-17 jaar",
                                ifelse(data_n3$age>17 & data_n3$age<26,"18-25 jaar",
                                       ifelse(data_n3$age>25 & data_n3$age<66,"26-65 jaar","+65 jaar"))))
data_n3$age_cat2<-ifelse(is.na(data_n3$age),NA, data_n3$age_cat2)

# This is where I read in that data_round2 again, which i made because of the memory allocation issues.
# The aim is to compare mental health etc. between rounds.

data_round2<-read.table("data_round2.txt",header=T)
data_round2$round<-0
data_n3$round<-1

myvars <- c("age_cat","male","covid","fam_symp","single","arbeid","diploma","WB_1","WB_2","WB_3","round","rel.prob","rel.prob_70_plus",
            "rel.prob_18_70","rel.prob_12_18","rel.prob_0_12","rel.prob_partner_fam","hh.contacts")

data_com1<-data_round2[myvars]
data_com2<-data_n3[myvars]
data_com<-rbind(data_com1,data_com2)

table(data_com$round,data_com$age_cat)
table(data_com$round,data_com$male)
table(data_com$round,data_com$covid)
table(data_com$round,data_com$fam_symp)
table(data_com$round,data_com$arbeid)
table(data_com$round,data_com$diploma)
table(data_com$round,data_com$single)

### Statistical Analysis

# mental health analysis

model1<-lm(WB_1~ age_cat + male + covid + fam_symp + single + arbeid + diploma + round ,data=data_com )
summary(model1)

model1<-lm(WB_1~ age_cat + male + (covid + fam_symp +  single  + arbeid + diploma)*round ,data=data_com )
summary(model1)

model2<-glm(WB_2 ~ age_cat + male + covid + fam_symp +  single + arbeid + diploma + round, family=binomial, data=data_com )
summary(model2)

(est4 <- cbind(Estimate = coef(model2), confint.default(model2)))
exp(est4)

model2<-glm(WB_2 ~ age_cat + male + covid + (fam_symp +  single + arbeid + diploma)*round, family=binomial, data=data_com )
summary(model2)

(est4 <- cbind(Estimate = coef(model2), confint.default(model2)))
exp(est4)

model3<-glm(WB_3 ~ age_cat + male + covid + fam_symp +  single + arbeid + diploma + round, family=binomial, data=data_com)
summary(model3)

(est5 <- cbind(Estimate = coef(model3), confint.default(model3)))
exp(est5)

model3<-glm(WB_3 ~ age_cat + male + (covid + fam_symp +  single + arbeid + diploma)*round, family=binomial, data=data_com)
summary(model3)

(est5 <- cbind(Estimate = coef(model3), confint.default(model3)))
exp(est5)

# relational problems analysis

model4<-glm(rel.prob ~ age_cat + male + covid + fam_symp +  hh.contacts + arbeid + diploma + round, family=binomial, data=data_com )
summary(model4)

(est6 <- cbind(Estimate = coef(model4), confint.default(model4)))
exp(est6)

model4<-glm(rel.prob ~ covid + arbeid +age_cat +  ( male +fam_symp +  hh.contacts  + diploma)*round, family=binomial, data=data_com )
summary(model4)

(est6 <- cbind(Estimate = coef(model4), confint.default(model4)))
exp(est6)

model4<-glm(rel.prob_18_70 ~ age_cat + male + covid + fam_symp + arbeid + hh.contacts + diploma + round, family=binomial, data=data_com )
summary(model4)

(est6 <- cbind(Estimate = coef(model4), confint.default(model4)))
exp(est6)

model4<-glm(rel.prob_18_70 ~ diploma+male + covid + age_cat + (fam_symp + arbeid + hh.contacts )*round, family=binomial, data=data_com )
summary(model4)

(est6 <- cbind(Estimate = coef(model4), confint.default(model4)))
exp(est6)

model4<-glm(rel.prob_partner_fam ~ age_cat + male + covid + fam_symp + arbeid + hh.contacts + diploma + round, family=binomial, data=data_com )
summary(model4)

(est6 <- cbind(Estimate = coef(model4), confint.default(model4)))
exp(est6)

model4<-glm(rel.prob_partner_fam ~ covid + arbeid + age_cat +  diploma+fam_symp + (male + hh.contacts)* round, family=binomial, data=data_com )
summary(model4)

(est6 <- cbind(Estimate = coef(model4), confint.default(model4)))
exp(est6)

# Important (obvio) thing that was just mentioned to me yesterday evening: we should delete all surveys that took longer than 2 hours
data_n3b<-data_n3[data_n3$Duration__in_seconds_<7200,]

data_n3b$age_cat2<-factor(data_n3b$age_cat2,levels=c("18-25 jaar","0-12 jaar","13-17 jaar","26-65 jaar","+65 jaar"))

# Analysis for the news on MNM

model_mnm<-glm(geenhand~age_cat2 + male + arbeid + diploma,data=data_n3b,family=binomial)
summary(model_mnm)

(est6 <- cbind(Estimate = coef(model_mnm), confint.default(model_mnm)))
exp(est6)


##### Combined figure for press

## Code to make new age categories for the density plots. Done this morning, so can be smoother :-)

# data round 1

data$age<-as.numeric(data$Q2)
data$age_cat<-age_cat
data$age_cat3<-ifelse(data$age<18,"0-17 jaar",
                      ifelse(data$age>17 & data$age<36,"18-35 jaar",
                             ifelse(data$age>35 & data$age<46,"36-45 jaar",
                                    ifelse(data$age>45 & data$age<66,"46-65 jaar",
                                           ifelse(data$age>65 & data$age<81,"66-80 jaar","+80 jaar")))))
data$age_cat3<-ifelse(is.na(data$age),NA, data$age_cat3)

data$age_cat4<-ifelse(data$age<18,"0-17 jaar",
                      ifelse(data$age>17 & data$age<26,"18-25 jaar",
                             ifelse(data$age>25 & data$age<46,"26-45 jaar",
                                    ifelse(data$age>45 & data$age<66,"46-65 jaar",
                                           ifelse(data$age>65 & data$age<81,"66-80 jaar","+80 jaar")))))
data$age_cat4<-ifelse(is.na(data$age),NA, data$age_cat4)

data_a<-data.frame(data$age_cat,date_stop_num_c)
names(data_a)<-c("age_cat","date_stop_num_c")

data_a2<-data.frame(data$age_cat3,date_stop_num_c)
names(data_a2)<-c("age_cat","date_stop_num_c")

data_a3<-data.frame(data$age_cat4,date_stop_num_c)
names(data_a3)<-c("age_cat","date_stop_num_c")

# data round 2

data_n$age_cat3<-ifelse(data_n$age<18,"0-17 jaar",
                        ifelse(data_n$age>17 & data_n$age<36,"18-35 jaar",
                               ifelse(data_n$age>35 & data_n$age<46,"36-45 jaar",
                                      ifelse(data_n$age>45 & data_n$age<66,"46-65 jaar",
                                             ifelse(data_n$age>65 & data_n$age<81,"66-80 jaar","+80 jaar")))))
data_n$age_cat3<-ifelse(is.na(data_n$age),NA, data_n$age_cat3)

data_n$age_cat4<-ifelse(data_n$age<18,"0-17 jaar",
                        ifelse(data_n$age>17 & data_n$age<26,"18-25 jaar",
                               ifelse(data_n$age>25 & data_n$age<46,"26-45 jaar",
                                      ifelse(data_n$age>45 & data_n$age<66,"46-65 jaar",
                                             ifelse(data_n$age>65 & data_n$age<81,"66-80 jaar","+80 jaar")))))
data_n$age_cat4<-ifelse(is.na(data_n$age),NA, data_n$age_cat4)

data_n2_new<-data.frame(as.numeric(data_n$Q2),data_n$age_cat,as.numeric(data_n$Q10),as.numeric(data_n$Q63))
names(data_n2_new)<-c("new","age_cat","stop1","stop2")
data_n2_new$stop1<-as.numeric(sapply(1:length(data_n2_new$new), 
                                     function(x) ifelse(is.na(data_n2_new$stop1[x]),data_n2_new$stop2[x],data_n2_new$stop1[x])))
data_n2_new<-data_n2_new[data_n2_new$new==2,]
data_n2_new<-data_n2_new[data_n2_new$stop1>1,]
date_stop_num3<-as.numeric(data_n2_new$stop1)+as.numeric(as.Date("16/03/2020",format = "%d/%m/%Y"))
data_n2_new$date_stop_num_c3<-date_stop_num3-18293 #aantal dagen sinds 1/2/2020
data_n2_new$date_stop_num<-data_n2_new$date_stop_num_c3


data_b_n2_new<-data.frame(as.numeric(data_n$Q2),data_n$age_cat3,as.numeric(data_n$Q10),as.numeric(data_n$Q63))
names(data_b_n2_new)<-c("new","age_cat","stop1","stop2")
data_b_n2_new$stop1<-as.numeric(sapply(1:length(data_b_n2_new$new), 
                                       function(x) ifelse(is.na(data_b_n2_new$stop1[x]),data_b_n2_new$stop2[x],data_b_n2_new$stop1[x])))
data_b_n2_new<-data_b_n2_new[data_b_n2_new$new==2,]
data_b_n2_new<-data_b_n2_new[data_b_n2_new$stop1>1,]
date_stop_num3<-as.numeric(data_b_n2_new$stop1)+as.numeric(as.Date("16/03/2020",format = "%d/%m/%Y"))
data_b_n2_new$date_stop_num_c3<-date_stop_num3-18293 #aantal dagen sinds 1/2/2020
data_b_n2_new$date_stop_num<-data_b_n2_new$date_stop_num_c3


data_c_n2_new<-data.frame(as.numeric(data_n$Q2),data_n$age_cat4,as.numeric(data_n$Q10),as.numeric(data_n$Q63))
names(data_c_n2_new)<-c("new","age_cat","stop1","stop2")
data_c_n2_new$stop1<-as.numeric(sapply(1:length(data_c_n2_new$new), 
                                       function(x) ifelse(is.na(data_c_n2_new$stop1[x]),data_c_n2_new$stop2[x],data_c_n2_new$stop1[x])))
data_c_n2_new<-data_c_n2_new[data_c_n2_new$new==2,]
data_c_n2_new<-data_c_n2_new[data_c_n2_new$stop1>1,]
date_stop_num3<-as.numeric(data_c_n2_new$stop1)+as.numeric(as.Date("16/03/2020",format = "%d/%m/%Y"))
data_c_n2_new$date_stop_num_c3<-date_stop_num3-18293 #aantal dagen sinds 1/2/2020
data_c_n2_new$date_stop_num<-data_c_n2_new$date_stop_num_c3

data_b<-data_n2_new[,c(2,5)]
names(data_b)<-c("age_cat","date_stop_num_c")

data_b2<-data_b_n2_new[,c(2,5)]
names(data_b2)<-c("age_cat","date_stop_num_c")

data_b3<-data_c_n2_new[,c(2,5)]
names(data_b3)<-c("age_cat","date_stop_num_c")


# data round 3

data_n3_new<-data.frame(as.numeric(data_n3$Q2_3),data_n3$age_cat,as.numeric(data_n3$Q10),as.numeric(data_n3$Q63))
names(data_n3_new)<-c("new","age_cat","stop1","stop2")
data_n3_new$stop1<-as.numeric(sapply(1:length(data_n3_new$new), 
                                     function(x) ifelse(is.na(data_n3_new$stop1[x]),data_n3_new$stop2[x],data_n3_new$stop1[x])))
data_n3_new<-data_n3_new[!is.na(data_n3_new$new),]
data_n3_new<-data_n3_new[data_n3_new$stop1>1,]
date_stop_num3<-as.numeric(data_n3_new$stop1)+as.numeric(as.Date("23/03/2020",format = "%d/%m/%Y"))
data_n3_new$date_stop_num_c3<-date_stop_num3-18293 #aantal dagen sinds 1/2/2020

data_n3$age_cat3<-ifelse(data_n3$age<18,"0-17 jaar",
                         ifelse(data_n3$age>17 & data_n3$age<36,"18-35 jaar",
                                ifelse(data_n3$age>35 & data_n3$age<46,"36-45 jaar",
                                       ifelse(data_n3$age>45 & data_n3$age<66,"46-65 jaar",
                                              ifelse(data_n3$age>65 & data_n3$age<81,"66-80 jaar","+80 jaar")))))
data_n3$age_cat3<-ifelse(is.na(data_n3$age),NA, data_n3$age_cat3)

data_n3$age_cat4<-ifelse(data_n3$age<18,"0-17 jaar",
                         ifelse(data_n3$age>17 & data_n3$age<26,"18-25 jaar",
                                ifelse(data_n3$age>25 & data_n3$age<46,"26-45 jaar",
                                       ifelse(data_n3$age>45 & data_n3$age<66,"46-65 jaar",
                                              ifelse(data_n3$age>65 & data_n3$age<81,"66-80 jaar","+80 jaar")))))
data_n3$age_cat4<-ifelse(is.na(data_n3$age),NA, data_n3$age_cat4)

data_b_n3_new<-data.frame(as.numeric(data_n3$Q2_3),data_n3$age_cat3,as.numeric(data_n3$Q10),as.numeric(data_n3$Q63))
names(data_b_n3_new)<-c("new","age_cat","stop1","stop2")
data_b_n3_new$stop1<-as.numeric(sapply(1:length(data_b_n3_new$new), 
                                       function(x) ifelse(is.na(data_b_n3_new$stop1[x]),data_b_n3_new$stop2[x],data_b_n3_new$stop1[x])))
data_b_n3_new<-data_b_n3_new[!is.na(data_b_n3_new$new),]
data_b_n3_new<-data_b_n3_new[data_b_n3_new$stop1>1,]
date_stop_num3<-as.numeric(data_b_n3_new$stop1)+as.numeric(as.Date("23/03/2020",format = "%d/%m/%Y"))
data_b_n3_new$date_stop_num_c3<-date_stop_num3-18293 #aantal dagen sinds 1/2/2020

data_c_n3_new<-data.frame(as.numeric(data_n3$Q2_3),data_n3$age_cat4,as.numeric(data_n3$Q10),as.numeric(data_n3$Q63))
names(data_c_n3_new)<-c("new","age_cat","stop1","stop2")
data_c_n3_new$stop1<-as.numeric(sapply(1:length(data_c_n3_new$new), 
                                       function(x) ifelse(is.na(data_c_n3_new$stop1[x]),data_c_n3_new$stop2[x],data_c_n3_new$stop1[x])))
data_c_n3_new<-data_c_n3_new[!is.na(data_c_n3_new$new),]
data_c_n3_new<-data_c_n3_new[data_c_n3_new$stop1>1,]
date_stop_num3<-as.numeric(data_c_n3_new$stop1)+as.numeric(as.Date("23/03/2020",format = "%d/%m/%Y"))
data_c_n3_new$date_stop_num_c3<-date_stop_num3-18293 #aantal dagen sinds 1/2/2020

data_c<-data_n3_new[,c(2,5)]
names(data_c)<-c("age_cat","date_stop_num_c")

data_c2<-data_b_n3_new[,c(2,5)]
names(data_c2)<-c("age_cat","date_stop_num_c")

data_c3<-data_c_n3_new[,c(2,5)]
names(data_c3)<-c("age_cat","date_stop_num_c")

# combining data sets for the graphs

data_hist<-rbind(data_a,data_b,data_c)
data_hist2<-rbind(data_a2,data_b2,data_c2)
data_hist3<-rbind(data_a3,data_b3,data_c3)

# the plots

# Taking a subset for the graphs (1st option for grouping of ages)

date15<-data_hist$date_stop_num_c[data_hist$date_stop_num_c>=15]
age15<-data_hist$age_cat[data_hist$date_stop_num_c>=15]

tiff("Fig_age_R3a.tiff", units="in", width=14*0.8, height=10*0.8, res=300)
plot(density(date15[age15%in%c("0-12 jaar","13-17 jaar")], adjust = 7, na.rm=T,from=15,to=59),main=" ",
     ylab=" " , xlab="Datum",cex.lab=1.5, xlim = c(15,60), lwd = 2, col = "Blue", xaxt="n", zero.line = FALSE, axes=FALSE)
axis(1, at=c(15 , 59), labels=c("",""), lwd.ticks=0)
title(main = ("Wanneer gaven Belgen voor het laatst een hand of zoen aan iemand anders dan een huisgenoot?"), cex.main = 1.3)
#axis(2, at=c(0 , 0.15), labels=c("",""), lwd.ticks=0)
axis(1, at=sort((c(seq(15 , 55, by=5),58))+0.5),cex.axis=1.3, label = c("17/2","22/2","27/2","3/3","8/3","13/3","18/3","23/3","28/3","31/3"))
lines(density(date15[age15%in%c("+80 jaar","66-80 jaar")], adjust = 7, na.rm=T,from=15,to=59), lwd = 2, col = "Red")
lines(density(date15[age15%in%c("18-45 jaar","46-65 jaar")], adjust = 7, na.rm=T,from=15,to=59), lwd = 2, col = "Green")
legend(16, 0.088, legend=c("0-17 jaar", "18-65 jaar", "+65 jaar","maatregelen"),
       col=c("blue", "green","red","black"), lty=c(1,1,1,2), cex=1.5, lwd = 2)
abline(v=(as.numeric(as.Date("11/03/2020",format = "%d/%m/%Y"))-as.numeric(as.Date("01/02/2020",format = "%d/%m/%Y"))-0.5), col="black",lty=2, lwd = 2 )
abline(v=(as.numeric(as.Date("13/03/2020",format = "%d/%m/%Y"))-as.numeric(as.Date("01/02/2020",format = "%d/%m/%Y"))-0.5), col="black",lty=2, lwd = 2)
abline(v=(as.numeric(as.Date("17/03/2020",format = "%d/%m/%Y"))-as.numeric(as.Date("01/02/2020",format = "%d/%m/%Y"))-0.5), col="black",lty=2, lwd = 2)
text(37.6+0.5,0.088,"1",cex=1.5)
text(39.6+0.5,0.088,"2",cex=1.5)
text(43.6+0.5,0.088,"3",cex=1.5)
dev.off()

tiff("Fig_age_R3b.tiff", units="in", width=14*0.8, height=10*0.8, res=300)
plot(density(date15[age15%in%c("0-12 jaar","13-17 jaar")], adjust = 7, na.rm=T,from=15,to=59),main=" ",
     ylab=" " , xlab="Datum",cex.lab=1.5, xlim = c(15,60), lwd = 2, col = "Blue", xaxt="n", zero.line = FALSE, axes=FALSE)
axis(1, at=c(15 , 59), labels=c("",""), lwd.ticks=0)
title(main = ("Wanneer gaven Belgen voor het laatst een hand of zoen aan iemand anders dan een huisgenoot?"), cex.main = 1.3)
#axis(2, at=c(0 , 0.15), labels=c("",""), lwd.ticks=0)
axis(1, at=sort((c(seq(15 , 55, by=5),58))+0.5),cex.axis=1.3, label = c("17/2","22/2","27/2","3/3","8/3","13/3","18/3","23/3","28/3","31/3"))
lines(density(date15[age15%in%c("+80 jaar","66-80 jaar")], adjust = 7, na.rm=T,from=15,to=59), lwd = 2, col = "Red")
lines(density(date15[age15%in%c("18-45 jaar","46-65 jaar")], adjust = 7, na.rm=T,from=15,to=59), lwd = 2, col = "Green")
legend(16, 0.088, legend=c("0-17 jaar", "18-65 jaar", "+65 jaar"),
       col=c("blue", "green","red"), lty=c(1,1,1), cex=1.5, lwd = 2)
dev.off()

# Taking a subset for the graphs (2nd option for grouping of ages)

date15<-data_hist2$date_stop_num_c[data_hist2$date_stop_num_c>=15]
age15<-data_hist2$age_cat[data_hist2$date_stop_num_c>=15]

tiff("Fig_age_R3a2.tiff", units="in", width=14*0.8, height=10*0.8, res=300)
plot(density(date15[age15%in%c("0-17 jaar")], adjust = 7, na.rm=T,from=15,to=59),main=" ",
     ylab=" " , xlab="Datum",cex.lab=1.5, xlim = c(15,60), lwd = 2, col = "Blue", xaxt="n", zero.line = FALSE, axes=FALSE)
axis(1, at=c(15 , 59), labels=c("",""), lwd.ticks=0)
title(main = ("Wanneer gaven Belgen voor het laatst een hand of zoen aan iemand anders dan een huisgenoot?"), cex.main = 1.3)
#axis(2, at=c(0 , 0.15), labels=c("",""), lwd.ticks=0)
axis(1, at=sort((c(seq(15 , 55, by=5),58))+0.5),cex.axis=1.3, label = c("17/2","22/2","27/2","3/3","8/3","13/3","18/3","23/3","28/3","31/3"))
lines(density(date15[age15%in%c("18-35 jaar")], adjust = 7, na.rm=T,from=15,to=59), lwd = 2, col = "orange")
lines(density(date15[age15%in%c("+80 jaar","66-80 jaar")], adjust = 7, na.rm=T,from=15,to=59), lwd = 2, col = "Red")
lines(density(date15[age15%in%c("36-45 jaar","46-65 jaar")], adjust = 7, na.rm=T,from=15,to=59), lwd = 2, col = "Green")
legend(16, 0.088, legend=c("0-17 jaar","18-35 jaar", "36-65 jaar", "+65 jaar","maatregelen"),
       col=c("blue", "orange","green","red","black"), lty=c(1,1,1,1,2), cex=1.5, lwd = 2)
abline(v=(as.numeric(as.Date("11/03/2020",format = "%d/%m/%Y"))-as.numeric(as.Date("01/02/2020",format = "%d/%m/%Y"))-0.5), col="black",lty=2, lwd = 2 )
abline(v=(as.numeric(as.Date("13/03/2020",format = "%d/%m/%Y"))-as.numeric(as.Date("01/02/2020",format = "%d/%m/%Y"))-0.5), col="black",lty=2, lwd = 2)
abline(v=(as.numeric(as.Date("17/03/2020",format = "%d/%m/%Y"))-as.numeric(as.Date("01/02/2020",format = "%d/%m/%Y"))-0.5), col="black",lty=2, lwd = 2)
text(37.6+0.5,0.089,"1",cex=1.5)
text(39.6+0.5,0.089,"2",cex=1.5)
text(43.6+0.5,0.089,"3",cex=1.5)
dev.off()


tiff("Fig_age_R3b2.tiff", units="in", width=14*0.8, height=10*0.8, res=300)
plot(density(date15[age15%in%c("0-17 jaar")], adjust = 7, na.rm=T,from=15,to=59),main=" ",
     ylab=" " , xlab="Datum",cex.lab=1.5, xlim = c(15,60), lwd = 2, col = "Blue", xaxt="n", zero.line = FALSE, axes=FALSE)
axis(1, at=c(15 , 59), labels=c("",""), lwd.ticks=0)
title(main = ("Wanneer gaven Belgen voor het laatst een hand of zoen aan iemand anders dan een huisgenoot?"), cex.main = 1.3)
#axis(2, at=c(0 , 0.15), labels=c("",""), lwd.ticks=0)
axis(1, at=sort((c(seq(15 , 55, by=5),58))+0.5),cex.axis=1.3, label = c("17/2","22/2","27/2","3/3","8/3","13/3","18/3","23/3","28/3","31/3"))
lines(density(date15[age15%in%c("18-35 jaar")], adjust = 7, na.rm=T,from=15,to=59), lwd = 2, col = "orange")
lines(density(date15[age15%in%c("+80 jaar","66-80 jaar")], adjust = 7, na.rm=T,from=15,to=59), lwd = 2, col = "Red")
lines(density(date15[age15%in%c("36-45 jaar","46-65 jaar")], adjust = 7, na.rm=T,from=15,to=59), lwd = 2, col = "Green")
legend(16, 0.088, legend=c("0-17 jaar","18-35 jaar", "36-65 jaar", "+65 jaar"),
       col=c("blue", "orange","green","red"), lty=c(1,1,1,1), cex=1.5, lwd = 2)
dev.off()

# Taking a subset for the graphs (3rd option for grouping of ages)

date15<-data_hist3$date_stop_num_c[data_hist3$date_stop_num_c>=15]
age15<-data_hist3$age_cat[data_hist3$date_stop_num_c>=15]

tiff("Fig_age_R3a3.tiff", units="in", width=14*0.8, height=10*0.8, res=300)
plot(density(date15[age15%in%c("0-17 jaar")], adjust = 7, na.rm=T,from=15,to=59),main=" ",
     ylab=" " , xlab="Datum",cex.lab=1.5, xlim = c(15,60), lwd = 2, col = "Blue", xaxt="n", zero.line = FALSE, axes=FALSE,ylim=c(0,0.09))
axis(1, at=c(15 , 59), labels=c("",""), lwd.ticks=0)
title(main = ("Wanneer gaven Belgen voor het laatst een hand of zoen aan iemand anders dan een huisgenoot?"), cex.main = 1.3)
#axis(2, at=c(0 , 0.15), labels=c("",""), lwd.ticks=0)
axis(1, at=sort((c(seq(15 , 55, by=5),58))+0.5),cex.axis=1.3, label = c("17/2","22/2","27/2","3/3","8/3","13/3","18/3","23/3","28/3","31/3"))
lines(density(date15[age15%in%c("18-25 jaar")], adjust = 7, na.rm=T,from=15,to=59), lwd = 2, col = "orange")
lines(density(date15[age15%in%c("+80 jaar","66-80 jaar")], adjust = 7, na.rm=T,from=15,to=59), lwd = 2, col = "Red")
lines(density(date15[age15%in%c("26-45 jaar","46-65 jaar")], adjust = 7, na.rm=T,from=15,to=59), lwd = 2, col = "Green")
legend(16, 0.091, legend=c("0-17 jaar","18-25 jaar", "26-65 jaar", "+65 jaar","maatregelen"),
       col=c("blue", "orange","green","red","black"), lty=c(1,1,1,1,2), cex=1.5, lwd = 2)
abline(v=(as.numeric(as.Date("11/03/2020",format = "%d/%m/%Y"))-as.numeric(as.Date("01/02/2020",format = "%d/%m/%Y"))-0.5), col="black",lty=2, lwd = 2 )
abline(v=(as.numeric(as.Date("13/03/2020",format = "%d/%m/%Y"))-as.numeric(as.Date("01/02/2020",format = "%d/%m/%Y"))-0.5), col="black",lty=2, lwd = 2)
abline(v=(as.numeric(as.Date("17/03/2020",format = "%d/%m/%Y"))-as.numeric(as.Date("01/02/2020",format = "%d/%m/%Y"))-0.5), col="black",lty=2, lwd = 2)
text(37.6+0.5,0.0915,"1",cex=1.5)
text(39.6+0.5,0.0915,"2",cex=1.5)
text(43.6+0.5,0.0915,"3",cex=1.5)
dev.off()


tiff("Fig_age_R3b3.tiff", units="in", width=14*0.8, height=10*0.8, res=300)
plot(density(date15[age15%in%c("0-17 jaar")], adjust = 7, na.rm=T,from=15,to=59),main=" ",
     ylab=" " , xlab="Datum",cex.lab=1.5, xlim = c(15,60), lwd = 2, col = "Blue", xaxt="n", zero.line = FALSE, axes=FALSE,ylim=c(0,0.09))
axis(1, at=c(15 , 59), labels=c("",""), lwd.ticks=0)
title(main = ("Wanneer gaven Belgen voor het laatst een hand of zoen aan iemand anders dan een huisgenoot?"), cex.main = 1.3)
#axis(2, at=c(0 , 0.15), labels=c("",""), lwd.ticks=0)
axis(1, at=sort((c(seq(15 , 55, by=5),58))+0.5),cex.axis=1.3, label = c("17/2","22/2","27/2","3/3","8/3","13/3","18/3","23/3","28/3","31/3"))
lines(density(date15[age15%in%c("18-25 jaar")], adjust = 7, na.rm=T,from=15,to=59), lwd = 2, col = "orange")
lines(density(date15[age15%in%c("+80 jaar","66-80 jaar")], adjust = 7, na.rm=T,from=15,to=59), lwd = 2, col = "Red")
lines(density(date15[age15%in%c("26-45 jaar","46-65 jaar")], adjust = 7, na.rm=T,from=15,to=59), lwd = 2, col = "Green")
legend(16, 0.091, legend=c("0-17 jaar","18-25 jaar", "26-65 jaar", "+65 jaar"),
       col=c("blue", "orange","green","red"), lty=c(1,1,1,1), cex=1.5, lwd = 2)
dev.off()

