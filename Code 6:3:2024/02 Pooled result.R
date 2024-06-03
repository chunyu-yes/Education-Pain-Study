setwd("/Users/liuchunyu/Documents/Pain/Data")

# Pooled result
# Read and combine all the data from different countries
# CHARLS
CHARLS_pain_imputed <- read.csv("pain_charls_imputed2.csv")
CHARLS_pain_imputed <- CHARLS_pain_imputed [,-1]
dim(CHARLS_pain_imputed)
names(CHARLS_pain_imputed)
CHARLS_pain_imputed$country<-"China"
CHARLS_pain_imputed2<-CHARLS_pain_imputed[,c("age","female","high_edu","agegroup","height","high_edu_parent","weight","pain_status","retired","country")]
dim(CHARLS_pain_imputed2)
# HRS
HRS_pain_imputed <- read.csv("pain_hrs_imputed2.csv")
HRS_pain_imputed <- HRS_pain_imputed[,-1]
dim(HRS_pain_imputed)
names(HRS_pain_imputed)
HRS_pain_imputed$country<-"US"
HRS_pain_imputed2<-HRS_pain_imputed[,c("age","female","high_edu","agegroup","height","high_edu_parent","weight","pain_status","retired","country")]
dim(HRS_pain_imputed2)
# ELSA
ELSA_pain_imputed <- read.csv("pain_elsa_imputed2.csv")
ELSA_pain_imputed <- ELSA_pain_imputed[,-1]
dim(ELSA_pain_imputed)
names(ELSA_pain_imputed)
ELSA_pain_imputed$country<-"UK"
ELSA_pain_imputed2<-ELSA_pain_imputed[,c("age","female","high_edu","agegroup","height","high_edu_parent","weight","pain_status","retired","country")]
dim(ELSA_pain_imputed2)
# LASI
LASI_pain_imputed <- read.csv("pain_lasi_imputed2.csv")
LASI_pain_imputed <- LASI_pain_imputed[,-1]
dim(LASI_pain_imputed)
names(LASI_pain_imputed)
LASI_pain_imputed$country<-"Indian"
LASI_pain_imputed2<-LASI_pain_imputed[,c("age","female","high_edu","agegroup","height","high_edu_parent","weight","pain_status","retired","country")]
dim(LASI_pain_imputed2)

com<-rbind(CHARLS_pain_imputed2,HRS_pain_imputed2,ELSA_pain_imputed2,LASI_pain_imputed2)
dim(com)
com$country[which(com$country=="Indian")]<-"India"
com$country <- factor(com$country,levels=c("US","UK","China","India"))

com[, c(2,3,4,6,8,9,10)] <- lapply (com[, c(2,3,4,6,8,9,10)], as.factor)

#
fit<-glm(pain_status ~ female  *country * high_edu + agegroup  + height + high_edu_parent + retired, family=binomial(link = "probit"), weights=weight, data=com)
summary(fit)

anova(fit)
