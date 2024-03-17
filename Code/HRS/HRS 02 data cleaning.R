setwd("/Users/liuchunyu/Documents/Pain/Data")

library(psych)
library(car)
library(tidyverse)
library(tableone)
library(kableExtra)
library(nnet)
library(survey)
library(mice)

# read in the data
pain_hrs <- read.csv("pain_hrs.csv")
pain <- pain_hrs[, -1]
colnames(pain)

#-------------------------------------------------
#define sample size
nrow(pain)#42406 obs
table(pain$IWSTAT)#N=17146 participated in CHARLS 2015 survey
pain <- pain[which(pain$IWSTAT==1),]

#restrict to people with non-missing weight
pain <- pain[which(!is.na(pain$WEIGHT)), ]
17146-nrow(pain)#drop 0 obs

#restrict to people aged 50+
pain <- pain[which(pain$age>=50), ]
17146-nrow(pain)#drop 454 obs

#drop people without information on pain
pain <- pain[which(!is.na(pain$pain_status)), ]
16692-nrow(pain)#drop 35 obs

#drop people without information on education and rural/urban
pain <- pain[which(!is.na(pain$education) &!is.na(pain$sex)), ]
16657-nrow(pain)#drop 3 obs

#------------------------------------
#recode measures

#***********************
#pain status
table(pain$pain_status)
prop.table(table(pain$pain_status))#~41.74% had pain

table(pain$pain_interfere)
table(pain$pain_interfere,pain$pain_status)

#pain medication
table(pain$pain_med)
table(pain$pain_med,pain$pain_status)

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
#1.Less than high school, 2.GED, 3.High-school graduate, 4.Some college, 5.College and above

#labor force status
table(pain$lbrf)
#1.Works FT, 2.Works PT, 3.Unemployed, 4.Partly retired
#5.Retired, 6.Disabled, 7.Not in LbrF

#recode 1=employed, 2=retired, 3=other
pain$lbrf <- ifelse(pain$lbrf==1|pain$lbrf==2|
					pain$lbrf==4, 1, 
					ifelse(pain$lbrf==5, 2, 3))
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
pain$hhincome <- ifelse(pain$hhincome=="[0,1.59e+04]", 1, 
						ifelse(pain$hhincome=="(1.59e+04,3.13e+04]", 2,
							   ifelse(pain$hhincome=="(3.13e+04,5.58e+04]", 3,
									  ifelse(pain$hhincome=="(5.58e+04,1.04e+05]", 4, 5))))
table(pain$hhincome)

summary(pain$hhwealth)
pain$hhwealth <- quantcut(pain$hhwealth, q = 5, na.rm = TRUE)
table(pain$hhwealth)
pain$hhwealth <- ifelse(pain$hhwealth=="[-1.64e+06,5e+03]", 1,
						ifelse(pain$hhwealth=="(5e+03,8.4e+04]", 2,
							   ifelse(pain$hhwealth=="(8.4e+04,2.35e+05]", 3,
									  ifelse(pain$hhwealth=="(2.35e+05,6.43e+05]", 4, 5))))
table(pain$hhwealth)

#hhsize
table(pain$hhsize)

#race
table(pain$race1)
#1.white/caucasian, 2.Black/African American, 3.Other

table(pain$race2)
#0.Not Hispanic, 1.Hispanic

pain$race <- ifelse(pain$race1 == 1 & pain$race2 == 0, 0,
	   ifelse(pain$race1 == 2 & pain$race2 == 0, 1,
		   ifelse(pain$race2 == 1, 2, 3)))
table(pain$race)
#0.white/caucasian, 1.Black/African American, 2.Hispanic, 3.Other

#***********************
#individual-level health
#if heavy drinking
table(pain$num_drink)
pain$drink_heavy <- ifelse(pain$num_drink>=5 & pain$sex==1, 1, 
	ifelse(pain$num_drink>=4 & pain$sex==2, 1, 0))
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
#1. 3+ /wk, 2.1-2/wk, 3.1-3/mo, 4.Lt 1/mo 5. Never
pain$vigor_act_c<-ifelse(pain$vigor_act==5,0,
	ifelse(pain$vigor_act==4,0.23,
		ifelse(pain$vigor_act==3,0.47,
			ifelse(pain$vigor_act==2,1.5,
				ifelse(pain$vigor_act==1,5,NA)))))
table(pain$vigor_act_c)

table(pain$moder_act, exclude = NULL)
#1. 3+ /wk, 2.1-2/wk, 3.1-3/mo, 4.Lt 1/mo 5. Never
pain$moder_act_c<-ifelse(pain$moder_act==5,0,
		ifelse(pain$moder_act==4,0.23,
			ifelse(pain$moder_act==3,0.47,
				ifelse(pain$moder_act==2,1.5,
					ifelse(pain$moder_act==1,5,NA)))))
	table(pain$moder_act_c)
	
#if depressed, 1=yes, 0=no
table(pain$CESD)
pain$depressed <- ifelse(pain$CESD>=4, 1, 0)
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
table(pain$pain_level,pain$pain_status,exclude = NULL)*100/table(pain$pain_status)[2]
table(pain$pain_interfere,pain$pain_status,exclude = NULL)*100/table(pain$pain_status)[2]
table(pain$pain_med,pain$pain_status,exclude = NULL)*100/table(pain$pain_status)[2]

for(i in 5:47){pain[,i]<-as.numeric(as.character(pain[,i]))}
summary(pain)
#Imputation MICE set seed and only run five rounds and generate 1 complete dataset
#Only need 1 complete dataset to reduce computing time
HRS_pain_imputed <- mice(pain, m=1, maxit = 5, method = 'cart', seed = 1)

#Extract the data
HRS_pain_imputed <- complete(HRS_pain_imputed)
write.csv(HRS_pain_imputed, "pain_hrs_imputed.csv")
