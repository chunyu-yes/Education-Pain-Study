setwd("G:/My Drive/Pain research/Data analysis")

library(psych)
library(car)
library(tidyverse)
library(tableone)
library(kableExtra)
library(nnet)
library(survey)
library(mice)

# read in the data
pain <- read.csv("pain_charls.csv")
pain <- pain[, -1]
colnames(pain)

#-------------------------------------------------
#define sample size
nrow(pain)#20967 obs
table(pain$IWSTAT)#N=20967 participated in CHARLS 2015 survey

#restrict to people with non-missing weight
pain <- pain[which(!is.na(pain$WEIGHT)), ]
20967-nrow(pain)#drop 812 obs

#restrict to people aged 50+
pain <- pain[which(pain$age>=50), ]
20155-nrow(pain)#drop 3902 obs

#drop people without information on pain
pain <- pain[which(!is.na(pain$pain_status)), ]
16253-nrow(pain)#drop 999 obs

#drop people without information on education and rural/urban
pain <- pain[which(!is.na(pain$education) &!is.na(pain$sex)), ]
15254-nrow(pain)#drop 4 obs

#------------------------------------
#recode measures

#***********************
#pain status
table(pain$pain_status)
pain$pain_status <- ifelse(pain$pain_status==2, 0, 1)
prop.table(table(pain$pain_status))#~31% had pain

#pain medication
table(pain$pain_med_1)
table(pain$pain_med_2)
#only people taking medication as taking medicaiton: this how other studies asked participants
pain$pain_med <- 0
pain$pain_med[pain$pain_med_1 == 1 | pain$pain_med_2 == 2] <- 1
pain$pain_med[pain$pain_status == 0] <- 0
table(pain$pain_med)
table(pain$pain_status, pain$pain_med)

#***********************
#individual-level demographic
#age
summary(pain$age)

#create new measure: age group 
#1=age50-64, 2=age65-74, 3=age75-84, 4=age85+
pain$agegroup <- ifelse(pain$age<65, 1,
                        ifelse(pain$age>=65 & pain$age<75, 2,
                               ifelse(pain$age>=75 & pain$age<85, 3, 4)))
table(pain$agegroup)

#sex
table(pain$sex)
#1.man, 2.woman

#mstat
table(pain$mstat)
#1.married, 3.partnered, 4.separated, 5.divorced, 7.widowed, 8.never married
#recode into 1=married/partnered, 0=other
pain$mstat <- ifelse(pain$mstat==1|pain$mstat==3, 1, 0)
table(pain$mstat)

#harmonized education
table(pain$education)
#1.Less than lower secondary, 2.upper secondary & vocational training, 3.tertiary

#labor force status
table(pain$lbrf)
#1.Agri employed, 2.Agri self-employed, 3.Non-agri employed, 4.Non-agri self-employed
#5.Non-agri family business, 6.Unemployed, 7.Retired, 8.Never work

#create a new measure if currently farmer, 1=yes, 0=no
pain$if_farmer <- ifelse(pain$lbrf==1|pain$lbrf==2, 1, 0)
table(pain$if_farmer)

#recode 1=employed, 2=retired, 3=other
pain$lbrf <- ifelse(pain$lbrf==1|pain$lbrf==2|pain$lbrf==3|
                    pain$lbrf==4|pain$lbrf==5, 1, 
                    ifelse(pain$lbrf==7, 2, 3))
table(pain$lbrf)

#***********************
#household-level measures
#residence
table(pain$liverural)

#make them 5 quintiles, from 1-5, richer
summary(pain$hhincome)
library(gtools)
pain$hhincome <- quantcut(pain$hhincome, q = 5, na.rm = TRUE)
table(pain$hhincome)
pain$hhincome <- ifelse(pain$hhincome=="[-2.63e+05,780]", 1, 
                        ifelse(pain$hhincome=="(780,2.38e+03]", 2,
                               ifelse(pain$hhincome=="(2.38e+03,9.53e+03]", 3,
                                      ifelse(pain$hhincome=="(9.53e+03,3.68e+04]", 4, 5))))
table(pain$hhincome)

summary(pain$hhwealth)
pain$hhwealth <- quantcut(pain$hhwealth, q = 5, na.rm = TRUE)
table(pain$hhwealth)
pain$hhwealth <- ifelse(pain$hhwealth=="[-3.51e+06,7.4e+03]", 1,
                        ifelse(pain$hhwealth=="(7.4e+03,4.05e+04]", 2,
                               ifelse(pain$hhwealth=="(4.05e+04,1.17e+05]", 3,
                                      ifelse(pain$hhwealth=="(1.17e+05,2.93e+05]", 4, 5))))
table(pain$hhwealth)

#hhsize
table(pain$hhsize)

#hukou
table(pain$hukou)
#1.Agricultual hukou, 2.Non-agricultural hukou, 3.Unified residence hukou, 4.Do not have hukou
#combine 1 and 4
pain$hukou <- ifelse(pain$hukou==1|pain$hukou==4, 1, 0)
table(pain$hukou)

#***********************
#individual-level health
#if heavy drinking
table(pain$num_drink)
pain$drink_heavy <- ifelse(pain$num_drink>3, 1, 0)
table(pain$drink_heavy, exclude = NULL)
prop.table(table(pain$drink_heavy))

#smoke status
table(pain$smoke_ever)
table(pain$smoke_now)
#1=never smoker, 2=former smoker, 3=current smoker
pain$smoke <- ifelse(pain$smoke_ever==0 & pain$smoke_now==0, 1,
                     ifelse(pain$smoke_ever==1 & pain$smoke_now==0, 2,
                            ifelse(pain$smoke_now==1, 3, NA)))
table(pain$smoke, exclude = NULL)

#days of vigorous/moderate activity
table(pain$vigor_act, exclude = NULL)
table(pain$moder_act, exclude = NULL)

#if depressed, 1=yes, 0=no
table(pain$CESD)
pain$depressed <- ifelse(pain$CESD>=10, 1, 0)
table(pain$depressed, exclude = NULL)

#ADL
table(pain$ADL)
table(pain$IADL)

#Health insurance, 1=yes, 0=no
table(pain$pub_ins)
table(pain$pri_ins)
table(pain$oth_ins)
pain$health_ins <- ifelse(pain$pub_ins==1| pain$pri_ins==1|
                            pain$oth_ins==1, 1, 0)
table(pain$health_ins, exclude = NULL)

#disease
table(pain$if_arthritis)#1=yes, 0=no
table(pain$if_cancer)#1=yes, 0=no
table(pain$num_6disease)#count, range 0-6


####
pain$parents_education<-pain$edu_mom
pain$parents_education[which(is.na(pain$edu_mom)==T)]<-pain$edu_dad[which(is.na(pain$edu_mom)==T)]
pain$parents_education[which(is.na(pain$edu_mom)== FALSE & is.na(pain$edu_dad)== FALSE & pain$edu_dad>pain$edu_mom)]<-pain$edu_dad[which(is.na(pain$edu_mom)==FALSE & is.na(pain$edu_dad)==FALSE & pain$edu_dad>pain$edu_mom)]

####missing percentage
summary(pain$age)*100/nrow(pain)
summary(pain$sex)*100/nrow(pain)
summary(pain$education)*100/nrow(pain)
summary(pain$parents_education)*100/nrow(pain)
summary(pain$liverural)*100/nrow(pain)
summary(pain$height)*100/nrow(pain)
summary(pain$race)*100/nrow(pain)
summary(pain$caste)*100/nrow(pain)
summary(pain$indlang)*100/nrow(pain)
summary(pain$hukou)*100/nrow(pain)
summary(pain$pain_status)*100/nrow(pain)
#table(pain$pain_interfere,pain$pain_status,exclude = NULL)*100/table(pain$pain_status)[2]
table(pain$pain_med,pain$pain_status,exclude = NULL)*100/table(pain$pain_status)[2]

pain$pain_med_check<-0
pain$pain_med_check[which(is.na(pain$pain_med_1)==T & is.na(pain$pain_med_2)==T & is.na(pain$pain_med_3)==T & is.na(pain$pain_med_4)==T & is.na(pain$pain_med_5)==T & is.na(pain$pain_med_6)==T)]<-NA
table(pain$pain_med_check,pain$pain_status,exclude = NULL)*100/table(pain$pain_status)[2]


#Imputation MICE set seed and only run five rounds and generate 1 complete dataset
#Only need 1 complete dataset to reduce computing time
CHARLS_pain_imputed <- mice(pain, m=1, maxit = 5, method = 'cart', seed = 1)

#Extract the data
CHARLS_pain_imputed <- complete(CHARLS_pain_imputed)
write.csv(CHARLS_pain_imputed, "pain_charls_imputed.csv")