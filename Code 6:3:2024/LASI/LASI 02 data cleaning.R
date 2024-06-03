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
pain_lasi <- read.csv("pain_lasi_retired.csv")
pain <- pain_lasi[, -1]
colnames(pain)

#-------------------------------------------------
#define sample size
nrow(pain)#73,408 obs
table(pain$IWSTAT)#N=73,408 participated in LASI survey
pain <- pain[which(pain$IWSTAT==1),]

#restrict to people with non-missing weight
pain <- pain[which(!is.na(pain$WEIGHT)), ]
73408-nrow(pain)#drop 0 obs

#restrict to people aged 50+
pain <- pain[which(pain$age>=50), ]
73408-nrow(pain)#drop 20,242 obs

#drop people without information on pain
pain <- pain[which(!is.na(pain$pain_status)), ]
53166-nrow(pain)#drop 182 obs

#drop people without information on education and rural/urban
pain <- pain[which(!is.na(pain$education) &!is.na(pain$sex)), ]
52984-nrow(pain)#drop 0 obs

#------------------------------------
#recode measures

#***********************
#pain status
table(pain$pain_status)
prop.table(table(pain$pain_status))#~41.74% had pain

#frequency

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
table(pain$education_l)
#1.less than upper secondary; 2.upper secondary and vocat; 3.tertiary

#labor force status
table(pain$lbrf)
#1.wage/salary worker, 2.paid family worker, 3.non-agri self-employed, 4.farm/fishery/forestry (own/family)
#5.agricultural laborer, 6.unemployed and looking for job, 7.disabled, 8.homemaker, 9.other, 10.never worked

#recode 1=employed, 2=unemployed
pain$lbrf <- ifelse(pain$lbrf==1|pain$lbrf==2|
					pain$lbrf==3|pain$lbrf==4|pain$lbrf==5, 1, 2)
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
pain$hhincome <- ifelse(pain$hhincome=="[-2.99e+07,2e+04]", 1, 
						ifelse(pain$hhincome=="(2e+04,6.63e+04]", 2,
							   ifelse(pain$hhincome=="(6.63e+04,1.35e+05]", 3,
									  ifelse(pain$hhincome=="(1.35e+05,2.7e+05]", 4, 5))))
table(pain$hhincome)

summary(pain$hhwealth)
pain$hhwealth <- quantcut(pain$hhwealth, q = 5, na.rm = TRUE)
table(pain$hhwealth)
pain$hhwealth <- ifelse(pain$hhwealth=="[-2.27e+07,2.4e+05]", 1,
						ifelse(pain$hhwealth=="(2.4e+05,6.15e+05]", 2,
							   ifelse(pain$hhwealth=="(6.15e+05,1.34e+06]", 3,
									  ifelse(pain$hhwealth=="(1.34e+06,3.11e+06]", 4, 5))))
table(pain$hhwealth)

summary(pain$hhconsum)
pain$hhconsum <- quantcut(pain$hhconsum, q = 5, na.rm = TRUE)
table(pain$hhconsum)
pain$hhconsum <- ifelse(pain$hhconsum=="[0,8.59e+04]", 1,
						ifelse(pain$hhconsum=="(8.59e+04,1.34e+05]", 2,
							   ifelse(pain$hhconsum=="(1.34e+05,1.91e+05]", 3,
									  ifelse(pain$hhconsum=="(1.91e+05,2.93e+05]", 4, 5))))
table(pain$hhconsum)


#hhsize
table(pain$hhsize)

#caste
table(pain$caste)
#1.scheduled caste, 2.scheduled tribe, 3.other backward class(obc), 4.no caste or other caste.

#***********************
#individual-level health
#if heavy drinking
# table(pain$num_drink)
# pain$drink_heavy <- ifelse(pain$num_drink>=5 & pain$sex==1, 1, 
# 	ifelse(pain$num_drink>=4 & pain$sex==2, 1, 0))
# table(pain$drink_heavy, exclude = NULL)
# prop.table(table(pain$drink_heavy))

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
#1. everyday, 2.more than once a week, 3.once a week, 4.1-3/mo 5. Never
pain$vigor_act_c<-ifelse(pain$vigor_act==5,0,
	ifelse(pain$vigor_act==4,0.47,
		ifelse(pain$vigor_act==3,1,
			ifelse(pain$vigor_act==2,3,
				ifelse(pain$vigor_act==1,7,NA)))))
table(pain$vigor_act_c)

table(pain$moder_act, exclude = NULL)
#1. everyday, 2.more than once a week, 3.once a week, 4.1-3/mo 5. Never
pain$moder_act_c<-ifelse(pain$moder_act==5,0,
		ifelse(pain$moder_act==4,0.47,
			ifelse(pain$moder_act==3,1,
				ifelse(pain$moder_act==2,1.3,
					ifelse(pain$moder_act==1,7,NA)))))
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
table(pain$emp_ins)
table(pain$oth_ins)
table(pain$den_ins)
table(pain$drug_ins)

pain$health_ins <- ifelse(pain$pub_ins==1| pain$emp_ins==1|
							pain$oth_ins==1| pain$den_ins==1|pain$drug_ins==1, 1, 0)
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
summary(pain$age)/nrow(pain)
summary(pain$sex)/nrow(pain)
summary(pain$education)/nrow(pain)
summary(pain$parents_education)/nrow(pain)
summary(pain$liverural)/nrow(pain)
summary(pain$height)/nrow(pain)
summary(pain$race)/nrow(pain)
summary(pain$caste)/nrow(pain)
summary(pain$pain_status)/nrow(pain)
summary(pain$retired)*100/nrow(pain)
table(pain$pain_interfere,pain$pain_status,exclude = NULL)*100/table(pain$pain_status)[2]
table(pain$pain_med,pain$pain_status,exclude = NULL)*100/table(pain$pain_status)[2]

####
for(i in 5:49){pain[,i]<-as.numeric(as.character(pain[,i]))}
summary(pain)
#Imputation MICE set seed and only run five rounds and generate 1 complete dataset
#Only need 1 complete dataset to reduce computing time
LASI_pain_imputed <- mice(pain, m=5, maxit = 10, method = 'cart', seed = 1)

#Extract the data
LASI_pain_imputed <- complete(LASI_pain_imputed)

write.csv(LASI_pain_imputed, "pain_lasi_retired_imputed.csv")
