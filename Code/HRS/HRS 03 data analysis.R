library(lsmeans)
library(ggforce)
library(epiR)


setwd("/Users/liuchunyu/Documents/Pain/Data")
HRS_pain_imputed <- read.csv("pain_hrs_imputed.csv")
HRS_pain_imputed <- HRS_pain_imputed[,-1]
#--------------------------------------------------------------------------------
#need to rescale weight for regression analysis otherwise always statistically significant (use the sum of weight and # of participant)
HRS_pain_imputed$weight <- HRS_pain_imputed$WEIGHT/sum(HRS_pain_imputed[, 'WEIGHT'])*16654

#descriptive table 1
#recode education as 1=high vs 0=low education
table(HRS_pain_imputed$education)
HRS_pain_imputed$high_edu[HRS_pain_imputed$education == 2 | HRS_pain_imputed$education == 3] <- 1
HRS_pain_imputed$high_edu[HRS_pain_imputed$education == 1] <- 0
table(HRS_pain_imputed$high_edu)

#add highest parental education
table(HRS_pain_imputed$edu_dad)
table(HRS_pain_imputed$edu_mom)
#get the highest parental education
HRS_pain_imputed$edu_parent <- ifelse(HRS_pain_imputed$edu_dad > HRS_pain_imputed$edu_mom, HRS_pain_imputed$edu_dad, HRS_pain_imputed$edu_mom)
table(HRS_pain_imputed$edu_parent)
#dictomize edu_parent as high_edu_parent
HRS_pain_imputed$high_edu_parent <- ifelse(HRS_pain_imputed$edu_parent >= 2, 1, 0)
table(HRS_pain_imputed$high_edu_parent)

#***********************
#pain status
table(HRS_pain_imputed$pain_status)
prop.table(table(HRS_pain_imputed$pain_status))#~41.74% had pain

table(HRS_pain_imputed$pain_interfere)
table(HRS_pain_imputed$pain_interfere,HRS_pain_imputed$pain_status)
HRS_pain_imputed$pain_interfere[HRS_pain_imputed$pain_status == 0] <- NA

#pain medication
table(HRS_pain_imputed$pain_med)
table(HRS_pain_imputed$pain_med,HRS_pain_imputed$pain_status)

HRS_pain_imputed$pain_med[HRS_pain_imputed$pain_status == 0] <- NA
table(HRS_pain_imputed$pain_med)
table(HRS_pain_imputed$pain_status, HRS_pain_imputed$pain_med)

#pain level
table(HRS_pain_imputed$pain_level)
table(HRS_pain_imputed$pain_level,HRS_pain_imputed$pain_status)
HRS_pain_imputed$pain_level[HRS_pain_imputed$pain_level == 0] <- NA
HRS_pain_imputed$pain_level[HRS_pain_imputed$pain_level == 1|HRS_pain_imputed$pain_level == 2] <- 0
HRS_pain_imputed$pain_level[HRS_pain_imputed$pain_level == 3] <- 1
table(HRS_pain_imputed$pain_level)

#define factor 
colnames(HRS_pain_imputed)

HRS_pain_imputed[, c(7,17,36:40,41,49,51)] <- lapply(HRS_pain_imputed[, c(7,17,36:40,41,49,51)], as.factor)

HRS_pain_imputed$female[HRS_pain_imputed$sex == 2] <- 1
HRS_pain_imputed$female[HRS_pain_imputed$sex == 1] <- 0
HRS_pain_imputed$female <- as.factor(HRS_pain_imputed$female)

write.csv(HRS_pain_imputed,"pain_hrs_imputed2.csv")

#2 is women and 1 is men
table(HRS_pain_imputed$sex)

###########
table(HRS_pain_imputed$sex,HRS_pain_imputed$high_edu)


#Unweighted, use N
#overall
t1_unweighted = CreateTableOne(vars = c("age", "agegroup", "sex", "liverural","high_edu","high_edu_parent", "height","race", 
                                        "pain_status","pain_level",
                                        "pain_interfere","pain_med"), 
                               data=HRS_pain_imputed, includeNA = T)
print(t1_unweighted, showAllLevels = TRUE, catDigits=2, printToggle = FALSE) %>% 
  knitr::kable(caption = "unweighted descriptive charateristic (HRS)",
               booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")

#stratified by sex
t1_unweighted_stratified = CreateTableOne(vars = c("age", "agegroup", "sex", "liverural","high_edu","high_edu_parent", "height","race", 
"pain_status","pain_level",
"pain_interfere","pain_med"), 
                                          data=HRS_pain_imputed, addOverall = F,  strata="sex", test = T, includeNA = T)
print(t1_unweighted_stratified, showAllLevels = TRUE, catDigits=2, printToggle = FALSE) %>% 
  knitr::kable(caption = "unweighted descriptive charateristic (HRS)",
               booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")

## weighted. use weight %, mean and SD
## Create a weighted survey design object
HRSSvy <- svydesign(ids = ~ ID, weights = ~ weight, nest = F, data = HRS_pain_imputed)

#overall
t1_weighted <- svyCreateTableOne(vars = c("age", "agegroup", "sex", "liverural","high_edu","high_edu_parent", "height","race", 
"pain_status","pain_level",
"pain_interfere","pain_med"),
                                 data=HRSSvy, includeNA = F)
print(t1_weighted, catDigits=2,  showAllLevels = TRUE,  printToggle = FALSE) %>% 
  knitr::kable(caption = "Weighted descriptive charateristic (HRS)",
               booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")

#stratified by sex
t1_weighted_stratified <- svyCreateTableOne(vars = c("age", "agegroup", "sex", "liverural","high_edu","high_edu_parent", "height","race", 
"pain_status","pain_level",
"pain_interfere","pain_med"),data=HRSSvy, addOverall = F,  strata="sex", test = T, includeNA = T)
print(t1_weighted_stratified, catDigits=2,  showAllLevels = TRUE,  printToggle = FALSE) %>% 
  knitr::kable(caption = "Weighted descriptive charateristic (HRS)",
               booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")
#--------------------------------------------------------------------------------
#table 2
b<-NA

fit<-glm(pain_status ~ female + high_edu + agegroup + liverural  + height + high_edu_parent + race, family=binomial(link = "probit"), weights=weight, data=HRS_pain_imputed)

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[2:3,])
tidy$label<-'pain_status'
b<-rbind(b,tidy)

fit<-glm(pain_status ~ female*high_edu + agegroup + liverural + height + high_edu_parent + race, family=binomial(link = "probit"), weights=weight, data=HRS_pain_imputed)

epi.interaction(model = fit, coef= c(2,3,13), param ="product",conf.level = 0.95)

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[c(2:3,13),])
tidy$label<-'pain_status+interaction'
b<-rbind(b,tidy)

fit<-glm(pain_level ~ female + high_edu + agegroup + liverural  + height + high_edu_parent + race, family=binomial(link = "probit"), weights=weight, data=subset(HRS_pain_imputed, pain_status == 1))

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[2:3,])
tidy$label<-'pain_level'
b<-rbind(b,tidy)

fit<-glm(pain_level ~ female*high_edu + agegroup + liverural + height + high_edu_parent + race, family=binomial(link = "probit"), weights=weight, data=subset(HRS_pain_imputed, pain_status == 1))

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[c(2:3,13),])
tidy$label<-'pain_level+interaction'
b<-rbind(b,tidy)

fit<-glm(pain_interfere ~ female + high_edu + agegroup + liverural  + height + high_edu_parent + race, family=binomial(link = "probit"), weights=weight, data=subset(HRS_pain_imputed, pain_status == 1))

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[2:3,])
tidy$label<-'pain_interfere'
b<-rbind(b,tidy)

fit<-glm(pain_interfere ~ female*high_edu + agegroup + liverural + height + high_edu_parent + race, family=binomial(link = "probit"), weights=weight, data=subset(HRS_pain_imputed, pain_status == 1))

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[c(2:3,13),])
tidy$label<-'pain_interfere+interaction'
b<-rbind(b,tidy)

fit<-glm(pain_med ~ female + high_edu + agegroup + liverural  + height + high_edu_parent + race, family=binomial(link = "probit"), weights=weight, data=subset(HRS_pain_imputed, pain_status == 1))

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[2:3,])
tidy$label<-'pain_med'
b<-rbind(b,tidy)

fit<-glm(pain_med ~ female*high_edu + agegroup + liverural + height + high_edu_parent + race, family=binomial(link = "probit"), weights=weight, data=subset(HRS_pain_imputed, pain_status == 1))

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[c(2:3,13),])
tidy$label<-'pain_med+interaction'
b<-rbind(b,tidy)
b<-b[-1,]


b$OR<-sprintf("%.2f", b$Estimate)
b$ci<-paste0("(",sprintf("%.2f", b$Estimate-1.96*b$`Std. Error`),", ",sprintf("%.2f", b$Estimate+1.96*b$`Std. Error`),")")
b$orci<-paste0(b$OR," ",b$ci)
b$p<-sprintf("%.4f", b$`Pr(>|z|)`)
#--------------------------------------------------------------------------------
#sensitivity analysis (with percentile education)
#Similar result using percentile rank education
HRS_pain_imputed$education_cont <- as.numeric(HRS_pain_imputed$education_cont)

#Recode as every 10 percent increase
HRS_pain_imputed <- HRS_pain_imputed %>% 
  mutate(edu_rank = percent_rank(education_cont)*10)

b<-NA

fit<-glm(pain_status ~ female + edu_rank + agegroup + liverural  + height + high_edu_parent + race, family=binomial(link = "probit"), weights=weight, data=HRS_pain_imputed)

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[2:3,])
tidy$label<-'pain_status'
b<-rbind(b,tidy)

fit<-glm(pain_status ~ female*edu_rank + agegroup + liverural + height + high_edu_parent + race, family=binomial(link = "probit"), weights=weight, data=HRS_pain_imputed)

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[c(2:3,13),])
tidy$label<-'pain_status+interaction'
b<-rbind(b,tidy)

fit<-glm(pain_level ~ female + edu_rank + agegroup + liverural  + height + high_edu_parent + race, family=binomial(link = "probit"), weights=weight, data=subset(HRS_pain_imputed, pain_status == 1))

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[2:3,])
tidy$label<-'pain_level'
b<-rbind(b,tidy)

fit<-glm(pain_level ~ female*edu_rank + agegroup + liverural + height + high_edu_parent + race, family=binomial(link = "probit"), weights=weight, data=subset(HRS_pain_imputed, pain_status == 1))

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[c(2:3,13),])
tidy$label<-'pain_level+interaction'
b<-rbind(b,tidy)

fit<-glm(pain_interfere ~ female + edu_rank + agegroup + liverural  + height + high_edu_parent + race, family=binomial(link = "probit"), weights=weight, data=subset(HRS_pain_imputed, pain_status == 1))

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[2:3,])
tidy$label<-'pain_interfere'
b<-rbind(b,tidy)

fit<-glm(pain_interfere ~ female*edu_rank + agegroup + liverural + height + high_edu_parent + race, family=binomial(link = "probit"), weights=weight, data=subset(HRS_pain_imputed, pain_status == 1))

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[c(2:3,13),])
tidy$label<-'pain_interfere+interaction'
b<-rbind(b,tidy)


fit<-glm(pain_med ~ female + edu_rank + agegroup + liverural  + height + high_edu_parent + race, family=binomial(link = "probit"), weights=weight, data=subset(HRS_pain_imputed, pain_status == 1))

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[2:3,])
tidy$label<-'pain_med'
b<-rbind(b,tidy)

fit<-glm(pain_med ~ female*edu_rank + agegroup + liverural + height + high_edu_parent + race, family=binomial(link = "probit"), weights=weight, data=subset(HRS_pain_imputed, pain_status == 1))

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[c(2:3,13),])
tidy$label<-'pain_med+interaction'
b<-rbind(b,tidy)
b<-b[-1,]

b$OR<-sprintf("%.2f", b$Estimate)
b$ci<-paste0("(",sprintf("%.2f", b$Estimate-1.96*b$`Std. Error`),", ",sprintf("%.2f", b$Estimate+1.96*b$`Std. Error`),")")
b$orci<-paste0(b$OR," ",b$ci)
b$p<-sprintf("%.4f", b$`Pr(>|z|)`)

