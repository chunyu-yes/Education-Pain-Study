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
pain_elsa <- read.csv("pain_elsa_retired.csv")
pain <- pain_elsa[, -1]
colnames(pain)

#-------------------------------------------------
#define sample size
nrow(pain)#19802 obs
table(pain$IWSTAT)#N=8736 participated in ELSA 2015 survey
pain <- pain[which(pain$IWSTAT==1),]#8736

#restrict to people with non-missing weight
pain <- pain[which(!is.na(pain$WEIGHT)), ]
8736-nrow(pain)#drop 0 obs

#restrict to people aged 50+
pain <- pain[which(pain$age>=50), ]
8736-nrow(pain)#drop 179 obs

#drop people without information on pain
pain <- pain[which(!is.na(pain$pain_status)), ]
8557-nrow(pain)#drop 514 obs

#drop people without information on education and rural/urban
pain <- pain[which(!is.na(pain$education) &!is.na(pain$sex)), ]
8043-nrow(pain)#drop 727 obs
#7316
#------------------------------------
#recode measures

#***********************
#pain status
table(pain$pain_status)
prop.table(table(pain$pain_status))#~40.84% had pain

#pain medication
table(pain$pain_level)
table(pain$pain_level,pain$pain_status)

#***********************height

pain$height <- ifelse(is.na(pain$height1)== F,pain$height1,pain$height2)
pain$height <- ifelse(is.na(pain$height)==F,pain$height,pain$height3)
pain$height <- ifelse(is.na(pain$height)==F,pain$height,pain$height4)

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
#1.less than upper secondary; 2.upper secondary and vocat; 3.tertiary

#labor force status
table(pain$lbrf)
#1.Works FT, 2.Works PT, 3.Unemployed, 4.Partly retired
#5.Retired, 6.Disabled, 7.Not in LbrF

#recode 1=employed, 2=retired, 3=other
pain$lbrf <- ifelse(pain$lbrf==1|pain$lbrf==2|
					pain$lbrf==4, 1, 
					ifelse(pain$lbrf==5, 2, 3))
table(pain$lbrf)

table(pain$retired)
#***********************
#household-level measures

#make them 5 quintiles, from 1-5, richer
summary(pain$hhincome)
library(gtools)
pain$hhincome <- quantcut(pain$hhincome, q = 5, na.rm = TRUE)
table(pain$hhincome)
pain$hhincome <- ifelse(pain$hhincome=="[-2.47e+03,1.45e+04]", 1, 
						ifelse(pain$hhincome=="(1.45e+04,2.22e+04]", 2,
							   ifelse(pain$hhincome=="(2.22e+04,3.14e+04]", 3,
									  ifelse(pain$hhincome=="(3.14e+04,4.51e+04]", 4, 5))))
table(pain$hhincome)

summary(pain$hhwealth)
pain$hhwealth <- quantcut(pain$hhwealth, q = 5, na.rm = TRUE)
table(pain$hhwealth)
pain$hhwealth <- ifelse(pain$hhwealth=="[-1.48e+05,1.14e+05]", 1,
						ifelse(pain$hhwealth=="(1.14e+05,2.57e+05]", 2,
							   ifelse(pain$hhwealth=="(2.57e+05,4.19e+05]", 3,
									  ifelse(pain$hhwealth=="(4.19e+05,7.1e+05]", 4, 5))))
table(pain$hhwealth)

summary(pain$hhconsum)
pain$hhconsum <- quantcut(pain$hhconsum, q = 5, na.rm = TRUE)
table(pain$hhconsum)
pain$hhconsum <- ifelse(pain$hhconsum=="[761,5.17e+03]", 1,
						ifelse(pain$hhconsum=="(5.17e+03,7.16e+03]", 2,
							   ifelse(pain$hhconsum=="(7.16e+03,9.11e+03]", 3,
									  ifelse(pain$hhconsum=="(9.11e+03,1.21e+04]", 4, 5))))
table(pain$hhconsum)

#hhsize
table(pain$hhsize)

#race
table(pain$race)
#1.white, 4.non-white
pain$race <- cut(pain$race, breaks=c(-Inf,2,Inf),labels=c(0,1))

#***********************
#individual-level health
#if heavy drinking
summary(pain$num_drink)
pain$num_drink<-as.numeric(as.character(pain$num_drink))
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
#2. >1/wk, 3.1/wk, 4. 1-3/mo 5. Never
pain$vigor_act_c<-ifelse(pain$vigor_act==5,0,
	ifelse(pain$vigor_act==4,0.47,
		ifelse(pain$vigor_act==3,1,
			ifelse(pain$vigor_act==2,3, NA))))
table(pain$vigor_act_c)

table(pain$moder_act, exclude = NULL)
#2. >1/wk, 3.1/wk, 4. 1-3/mo 5. Never
pain$moder_act_c<-ifelse(pain$moder_act==5,0,
		ifelse(pain$moder_act==4,0.47,
			ifelse(pain$moder_act==3,1,
				ifelse(pain$moder_act==2,3, NA))))
table(pain$moder_act_c)
	
#if depressed, 1=yes, 0=no
table(pain$CESD)
pain$depressed <- ifelse(pain$CESD>=4, 1, 0)
table(pain$depressed, exclude = NULL)

#ADL
table(pain$ADL)
table(pain$IADL)

#Health insurance, 1=yes, 0=no
table(pain$pri_ins)
table(pain$long_ins)
pain$health_ins <- ifelse(pain$pri_ins==1|
							pain$long_ins==1, 1, 0)
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
summary(pain$retired)*100/nrow(pain)
table(pain$pain_level,pain$pain_status,exclude = NULL)*100/table(pain$pain_status)[2]
table(pain$pain_med,pain$pain_status,exclude = NULL)*100/table(pain$pain_status)[2]


for(i in 5:47){pain[,i]<-as.numeric(as.character(pain[,i]))}
summary(pain)

#Imputation MICE set seed and only run five rounds and generate 1 complete dataset
#Only need 1 complete dataset to reduce computing time
ELSA_pain_imputed <- mice(pain, m=5, maxit = 10, method = 'cart', seed = 1)

#Extract the data
ELSA_pain_imputed <- complete(ELSA_pain_imputed)

write.csv(ELSA_pain_imputed, "pain_elsa_retired_imputed.csv")
