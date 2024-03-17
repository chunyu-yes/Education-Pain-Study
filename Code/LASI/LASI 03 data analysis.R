library(lsmeans)
library(ggforce)
library(epiR)

LASI_pain_imputed<-read.csv("pain_lasi_imputed.csv")
LASI_pain_imputed<-LASI_pain_imputed[,-1]
#--------------------------------------------------------------------------------
#need to rescale weight for regression analysis otherwise always statistically significant (use the sum of weight and # of participant)
LASI_pain_imputed$weight <- LASI_pain_imputed$WEIGHT/sum(LASI_pain_imputed[, 'WEIGHT'])*52984

#descriptive table 1
#recode education as 1=high vs 0=low education
table(LASI_pain_imputed$education)
LASI_pain_imputed$high_edu[LASI_pain_imputed$education == 3|LASI_pain_imputed$education == 2] <- 1
LASI_pain_imputed$high_edu[LASI_pain_imputed$education == 1] <- 0
table(LASI_pain_imputed$high_edu)

#add highest parental education
table(LASI_pain_imputed$edu_dad)
table(LASI_pain_imputed$edu_mom)
#get the highest parental education
LASI_pain_imputed$edu_parent <- ifelse(LASI_pain_imputed$edu_dad > LASI_pain_imputed$edu_mom, LASI_pain_imputed$edu_dad, LASI_pain_imputed$edu_mom)
table(LASI_pain_imputed$edu_parent)
#dictomize edu_parent as high_edu_parent
LASI_pain_imputed$high_edu_parent <- ifelse(LASI_pain_imputed$edu_parent >= 5, 1, 0)
table(LASI_pain_imputed$high_edu_parent)

#***********************
#pain status
table(LASI_pain_imputed$pain_status)
prop.table(table(LASI_pain_imputed$pain_status))#~41.74% had pain

table(LASI_pain_imputed$pain_interfere)
table(LASI_pain_imputed$pain_interfere,LASI_pain_imputed$pain_status)
LASI_pain_imputed$pain_interfere[LASI_pain_imputed$pain_status == 0] <- NA

#pain medication
table(LASI_pain_imputed$pain_med)
table(LASI_pain_imputed$pain_med,LASI_pain_imputed$pain_status)

LASI_pain_imputed$pain_med[LASI_pain_imputed$pain_status == 0] <- NA
table(LASI_pain_imputed$pain_med)
table(LASI_pain_imputed$pain_status, LASI_pain_imputed$pain_med)



#define factor 
colnames(LASI_pain_imputed)

LASI_pain_imputed[, c(7,15,16,38,40,41,42,49,51)] <- lapply(LASI_pain_imputed[, c(7,15,16,38,40,41,42,49,51)], as.factor)

LASI_pain_imputed$female[LASI_pain_imputed$sex == 2] <- 1
LASI_pain_imputed$female[LASI_pain_imputed$sex == 1] <- 0
LASI_pain_imputed$female <- as.factor(LASI_pain_imputed$female)

#2 is women and 1 is men
table(LASI_pain_imputed$sex)

write.csv(LASI_pain_imputed,"pain_lasi_imputed2.csv")

#Unweighted, use N
#overall
t1_unweighted = CreateTableOne(vars = c("age", "agegroup", "sex","liverural", "high_edu", "high_edu_parent", "height","caste", "pain_status","pain_interfere","pain_med"), 
							   data=LASI_pain_imputed, includeNA = T)
print(t1_unweighted, showAllLevels = TRUE, catDigits=2, printToggle = FALSE) %>% 
  knitr::kable(caption = "unweighted descriptive charateristic (LASI)",
			   booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")

#stratified by sex
t1_unweighted_stratified = CreateTableOne(vars = c("age", "agegroup", "sex","liverural", "high_edu", "high_edu_parent", "height","caste", "pain_status","pain_interfere","pain_med"), 
										  data=LASI_pain_imputed, addOverall = F,  strata="sex", test = T, includeNA = T)
print(t1_unweighted_stratified, showAllLevels = TRUE, catDigits=2, printToggle = FALSE) %>% 
  knitr::kable(caption = "unweighted descriptive charateristic (LASI)",
			   booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")

## weighted. use weight %, mean and SD
## Create a weighted survey design object
charlsSvy <- svydesign(ids = ~ ID, weights = ~ weight, nest = F, data = LASI_pain_imputed)

#overall
t1_weighted <- svyCreateTableOne(vars = c("age", "agegroup", "sex","liverural", "high_edu", "high_edu_parent", "height","caste", "pain_status","pain_interfere","pain_med"),
								 data=charlsSvy, includeNA = F)
print(t1_weighted, catDigits=2,  showAllLevels = TRUE,  printToggle = FALSE) %>% 
  knitr::kable(caption = "Weighted descriptive charateristic (LASI)",
			   booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")

#stratified by sex
t1_weighted_stratified <- svyCreateTableOne(vars = c("age", "agegroup", "sex","liverural", "high_edu", "high_edu_parent", "height","caste", "pain_status","pain_interfere","pain_med"),
											data=charlsSvy, addOverall = F,  strata="sex", test = T, includeNA = T)
print(t1_weighted_stratified, catDigits=2,  showAllLevels = TRUE,  printToggle = FALSE) %>% 
  knitr::kable(caption = "Weighted descriptive charateristic (LASI)",
			   booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")

#--------------------------------------------------------------------------------
#table 2
b<-NA

fit<-glm(pain_status ~ female + high_edu + agegroup + liverural  + height + high_edu_parent + caste, family=binomial(link = "probit"), weights=weight, data=LASI_pain_imputed)

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[2:3,])
tidy$label<-'pain_status'
b<-rbind(b,tidy)

fit<-glm(pain_status ~ female*high_edu + agegroup + liverural + height + high_edu_parent + caste, family=binomial(link = "probit"), weights=weight, data=LASI_pain_imputed)

summary(fit)
epi.interaction(model = fit, coef= c(2,3,13), param ="product",conf.level = 0.95)

tidy<-as.data.frame(summary(fit)$coefficients[c(2:3,13),])
tidy$label<-'pain_status+interaction'
b<-rbind(b,tidy)

fit<-glm(pain_interfere ~ female + high_edu + agegroup + liverural  + height + high_edu_parent + caste, family=binomial(link = "probit"), weights=weight, data=subset(LASI_pain_imputed, pain_status == 1))

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[2:3,])
tidy$label<-'pain_interfere'
b<-rbind(b,tidy)

fit<-glm(pain_interfere ~ female*high_edu + agegroup + liverural + height + high_edu_parent + caste, family=binomial(link = "probit"), weights=weight, data=subset(LASI_pain_imputed, pain_status == 1))

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[c(2:3,13),])
tidy$label<-'pain_interfere+interaction'
b<-rbind(b,tidy)

fit<-glm(pain_med ~ female + high_edu + agegroup + liverural  + height + high_edu_parent + caste, family=binomial(link = "probit"), weights=weight, data=subset(LASI_pain_imputed, pain_status == 1))

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[2:3,])
tidy$label<-'pain_med'
b<-rbind(b,tidy)

fit<-glm(pain_med ~ female*high_edu + agegroup + liverural + height + high_edu_parent + caste, family=binomial(link = "probit"), weights=weight, data=subset(LASI_pain_imputed, pain_status == 1))

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
LASI_pain_imputed$education_cont <- as.numeric(LASI_pain_imputed$education_cont)

#Recode as every 10 percent increase
LASI_pain_imputed <- LASI_pain_imputed %>% 
  mutate(edu_rank = percent_rank(education_cont)*10)

b<-NA

fit<-glm(pain_status ~ female + edu_rank + agegroup + liverural  + height + high_edu_parent + caste, family=binomial(link = "probit"), weights=weight, data=LASI_pain_imputed)

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[2:3,])
tidy$label<-'pain_status'
b<-rbind(b,tidy)

fit<-glm(pain_status ~ female*edu_rank + agegroup + liverural + height + high_edu_parent + caste, family=binomial(link = "probit"), weights=weight, data=LASI_pain_imputed)

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[c(2:3,13),])
tidy$label<-'pain_status+interaction'
b<-rbind(b,tidy)

fit<-glm(pain_interfere ~ female + edu_rank + agegroup + liverural  + height + high_edu_parent + caste, family=binomial(link = "probit"), weights=weight, data=subset(LASI_pain_imputed, pain_status == 1))

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[2:3,])
tidy$label<-'pain_interfere'
b<-rbind(b,tidy)

fit<-glm(pain_interfere ~ female*edu_rank + agegroup + liverural + height + high_edu_parent + caste, family=binomial(link = "probit"), weights=weight, data=subset(LASI_pain_imputed, pain_status == 1))

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[c(2:3,13),])
tidy$label<-'pain_interfere+interaction'
b<-rbind(b,tidy)


fit<-glm(pain_med ~ female + edu_rank + agegroup + liverural  + height + high_edu_parent + caste, family=binomial(link = "probit"), weights=weight, data=subset(LASI_pain_imputed, pain_status == 1))

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[2:3,])
tidy$label<-'pain_med'
b<-rbind(b,tidy)

fit<-glm(pain_med ~ female*edu_rank + agegroup + liverural + height + high_edu_parent + caste, family=binomial(link = "probit"), weights=weight, data=subset(LASI_pain_imputed, pain_status == 1))

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
#regression analysis for binary body pain
#minimally adjusted for ses covariates
#no interaction between education and rural/urban observed among men and women
#men
summary(glm(pain_status ~ agegroup + high_edu + liverural + mstat + hhwealth + caste, family=binomial, data=subset(LASI_pain_imputed, sex == 1)))
summary(glm(pain_status ~ agegroup + high_edu*liverural + mstat + hhwealth + caste, family=binomial, data=subset(LASI_pain_imputed, sex == 1)))

summary(glm(pain_status ~ agegroup + high_edu + liverural + mstat + hhwealth + caste, family=binomial, weights=weight, data=subset(LASI_pain_imputed, sex == 1)))

#women
summary(glm(pain_status ~ agegroup + high_edu + liverural + mstat + hhwealth + caste, family=binomial, data=subset(LASI_pain_imputed, sex == 2)))
summary(glm(pain_status ~ agegroup + high_edu*liverural + mstat + hhwealth + caste, family=binomial, data=subset(LASI_pain_imputed, sex == 2)))

summary(glm(pain_status ~ agegroup + high_edu + liverural + mstat + hhwealth + caste, family=binomial, weights=weight, data=subset(LASI_pain_imputed, sex == 2)))

#both men and women with interaction between sex and education
#recode sex for easier interpretation
LASI_pain_imputed$female[LASI_pain_imputed$sex == 2] <- 1
LASI_pain_imputed$female[LASI_pain_imputed$sex == 1] <- 0
LASI_pain_imputed$female <- as.factor(LASI_pain_imputed$female)
summary(glm(pain_status ~ high_edu*female + agegroup + liverural + mstat + hhwealth + caste, family=binomial, weights=weight, data=LASI_pain_imputed))

#sensitivity analysis (additionally adjusted for physical health covariates) show similar results
summary(glm(pain_status ~ high_edu*female + agegroup + liverural + mstat + hhwealth + caste + if_arthritis + if_cancer + num_6disease + num_drink + smoke, 
			family=binomial, weights=weight, data=LASI_pain_imputed))

#RERI analysis for interaction (additive scale)
LASI_inter<-glm(pain_status ~ high_edu*female + agegroup + liverural + mstat + hhwealth + caste, family=binomial, weights=weight, data=LASI_pain_imputed)
epi.interaction(model = LASI_inter, coef = c(2,3,16), param = "product", conf.level = 0.95)

#regression analysis for pain management
#no interaction obeserved between sex and education
table(LASI_pain_imputed$pain_med)
table(LASI_pain_imputed$pain_status)
table(LASI_pain_imputed$pain_status, LASI_pain_imputed$pain_med) #3476 out of 4718 taking medicine or management measures
summary(glm(pain_med ~ agegroup + high_edu + female + liverural + mstat + hhwealth + caste, 
			family=binomial, weights=weight, data=subset(LASI_pain_imputed, pain_status == 1)))

summary(glm(pain_med ~ agegroup + high_edu*female + liverural + mstat + hhwealth + caste, 
			family=binomial, weights=weight, data=subset(LASI_pain_imputed, pain_status == 1)))

