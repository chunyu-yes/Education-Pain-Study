library(lsmeans)
library(ggforce)
library(epiR)
setwd("/Users/liuchunyu/Documents/Pain/Data")

CHARLS_pain_imputed <- read.csv("pain_charls_retired_imputed.csv")
CHARLS_pain_imputed <- CHARLS_pain_imputed[,-1]
#--------------------------------------------------------------------------------
#need to rescale weight for regression analysis otherwise always statistically significant (use the sum of weight and # of participant)
CHARLS_pain_imputed$weight <- CHARLS_pain_imputed$WEIGHT/sum(CHARLS_pain_imputed[, 'WEIGHT'])*15250

#descriptive table 1
#recode education as 1=high vs 0=low education
table(CHARLS_pain_imputed$education)
CHARLS_pain_imputed$high_edu[CHARLS_pain_imputed$education == 2 | CHARLS_pain_imputed$education == 3] <- 1
CHARLS_pain_imputed$high_edu[CHARLS_pain_imputed$education == 1] <- 0
table(CHARLS_pain_imputed$high_edu)

#add highest parental education
table(CHARLS_pain_imputed$edu_dad)
table(CHARLS_pain_imputed$edu_mom)
#get the highest parental education
CHARLS_pain_imputed$edu_parent <- ifelse(CHARLS_pain_imputed$edu_dad > CHARLS_pain_imputed$edu_mom, CHARLS_pain_imputed$edu_dad, CHARLS_pain_imputed$edu_mom)
table(CHARLS_pain_imputed$edu_parent)
#dictomize edu_parent as high_edu_parent
CHARLS_pain_imputed$high_edu_parent <- ifelse(CHARLS_pain_imputed$edu_parent >= 2, 1, 0)
table(CHARLS_pain_imputed$high_edu_parent)

#***********************
#pain status
table(CHARLS_pain_imputed$pain_status)
prop.table(table(CHARLS_pain_imputed$pain_status))#~41.74% had pain

#pain medication
table(CHARLS_pain_imputed$pain_med)
table(CHARLS_pain_imputed$pain_med,CHARLS_pain_imputed$pain_status)

CHARLS_pain_imputed$pain_med[CHARLS_pain_imputed$pain_status == 0] <- NA
table(CHARLS_pain_imputed$pain_med)
table(CHARLS_pain_imputed$pain_status, CHARLS_pain_imputed$pain_med)


#define factor 
colnames(CHARLS_pain_imputed)

CHARLS_pain_imputed[, c(7:18, 32:33, 36, 43:50, 52:54)] <- lapply(CHARLS_pain_imputed[, c(7:18, 32:33, 36, 43:50, 52:54)], as.factor)

CHARLS_pain_imputed$female[CHARLS_pain_imputed$sex == 2] <- 1
CHARLS_pain_imputed$female[CHARLS_pain_imputed$sex == 1] <- 0
CHARLS_pain_imputed$female <- as.factor(CHARLS_pain_imputed$female)

#write.csv(CHARLS_pain_imputed,"pain_charls_imputed2.csv")
#--------------------------------------------------------------------------------

#2 is women and 1 is men
table(CHARLS_pain_imputed$sex)

#Unweighted, use N
#overall
t1_unweighted = CreateTableOne(vars = c("age", "agegroup", "sex", "high_edu", "high_edu_parent", "height",
                                        "hukou", "liverural", "pain_status", "pain_med","retired"), 
                               data=CHARLS_pain_imputed, includeNA = T)
print(t1_unweighted, showAllLevels = TRUE, catDigits=2, printToggle = FALSE) %>% 
  knitr::kable(caption = "unweighted descriptive charateristic (CHARLS)",
               booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")

#stratified by sex
t1_unweighted_stratified = CreateTableOne(vars = c("age", "agegroup", "sex", "high_edu", "high_edu_parent", "height",
"hukou", "liverural", "pain_status", "pain_med","retired"),  
                     data=CHARLS_pain_imputed, addOverall = F,  strata="sex", test = T, includeNA = T)
print(t1_unweighted_stratified, showAllLevels = TRUE, catDigits=2, printToggle = FALSE) %>% 
  knitr::kable(caption = "unweighted descriptive charateristic (CHARLS)",
               booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")

## weighted. use weight %, mean and SD
## Create a weighted survey design object
charlsSvy <- svydesign(ids = ~ ID, weights = ~ weight, nest = FALSE, data = CHARLS_pain_imputed)

#overall
t1_weighted <- svyCreateTableOne(vars = c("age", "agegroup", "sex", "high_edu", "high_edu_parent", "height",
"hukou", "liverural", "pain_status", "pain_med", "retired"), 
                         data=charlsSvy, includeNA = FALSE)
print(t1_weighted, catDigits=2,  showAllLevels = TRUE,  printToggle = FALSE) %>% 
  knitr::kable(caption = "Weighted descriptive charateristic (CHARLS)",
               booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")

#stratified by sex
t1_weighted_stratified <- svyCreateTableOne(vars = c("age", "agegroup", "sex", "high_edu", "high_edu_parent", "height",
"hukou", "liverural", "pain_status", "pain_med","retired"), 
                                 data=charlsSvy, addOverall = F,  strata="sex", test = T, includeNA = T)
print(t1_weighted_stratified, catDigits=2,  showAllLevels = TRUE,  printToggle = FALSE) %>% 
  knitr::kable(caption = "Weighted descriptive charateristic (CHARLS)",
               booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")

#--------------------------------------------------------------------------------
#table 2
b<-NA

fit<-glm(pain_status ~ female + high_edu + agegroup + liverural  + height + high_edu_parent + hukou, family=binomial(link = "probit"), weights=weight, data=CHARLS_pain_imputed)

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[2:3,])
tidy$label<-'pain_status'
b<-rbind(b,tidy)

fit<-glm(pain_status ~ female*high_edu + agegroup + liverural + height + high_edu_parent + hukou, family=binomial(link = "probit"), weights=weight, data=CHARLS_pain_imputed)

summary(fit)
epi.interaction(model = fit, coef= c(2,3,11), param ="product",conf.level = 0.95)


tidy<-as.data.frame(summary(fit)$coefficients[c(2:3,11),])
tidy$label<-'pain_status+interaction'
b<-rbind(b,tidy)

fit<-glm(pain_med ~ female + high_edu + agegroup + liverural  + height + high_edu_parent + hukou, family=binomial(link = "probit"), weights=weight, data=subset(CHARLS_pain_imputed, pain_status == 1))

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[2:3,])
tidy$label<-'pain_med'
b<-rbind(b,tidy)

fit<-glm(pain_med ~ female*high_edu + agegroup + liverural + height + high_edu_parent + hukou, family=binomial(link = "probit"), weights=weight, data=subset(CHARLS_pain_imputed, pain_status == 1))

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[c(2:3,11),])
tidy$label<-'pain_med+interaction'
b<-rbind(b,tidy)
b<-b[-1,]


b$OR<-sprintf("%.2f", b$Estimate)
b$ci<-paste0("(",sprintf("%.2f", b$Estimate-1.96*b$`Std. Error`),", ",sprintf("%.2f", b$Estimate+1.96*b$`Std. Error`),")")
b$orci<-paste0(b$OR," ",b$ci)
b$p<-sprintf("%.4f", b$`Pr(>|z|)`)
#--------------------------------------------------------------------------------
# VIF
fit<-glm(pain_status ~ female + high_edu + agegroup + liverural + height + high_edu_parent + hukou, family=binomial(link = "probit"), weights=weight, data=CHARLS_pain_imputed)
vif(fit)
#--------------------------------------------------------------------------------
# Sensitivity analysis: table 2 with unadjusted model
b<-NA

fit<-glm(pain_status ~ female + high_edu, family=binomial(link = "probit"), weights=weight, data=CHARLS_pain_imputed)

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[2:3,])
tidy$label<-'pain_status'
b<-rbind(b,tidy)

fit<-glm(pain_status ~ female*high_edu, family=binomial(link = "probit"), weights=weight, data=CHARLS_pain_imputed)

summary(fit)
epi.interaction(model = fit, coef= c(2,3,4), param ="product",conf.level = 0.95)


tidy<-as.data.frame(summary(fit)$coefficients[c(2:4),])
tidy$label<-'pain_status+interaction'
b<-rbind(b,tidy)

fit<-glm(pain_med ~ female + high_edu, family=binomial(link = "probit"), weights=weight, data=subset(CHARLS_pain_imputed, pain_status == 1))

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[2:3,])
tidy$label<-'pain_med'
b<-rbind(b,tidy)

fit<-glm(pain_med ~ female*high_edu, family=binomial(link = "probit"), weights=weight, data=subset(CHARLS_pain_imputed, pain_status == 1))

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[c(2:4),])
tidy$label<-'pain_med+interaction'
b<-rbind(b,tidy)
b<-b[-1,]


b$OR<-sprintf("%.2f", b$Estimate)
b$ci<-paste0("(",sprintf("%.2f", b$Estimate-1.96*b$`Std. Error`),", ",sprintf("%.2f", b$Estimate+1.96*b$`Std. Error`),")")
b$orci<-paste0(b$OR," ",b$ci)
b$p<-sprintf("%.4f", b$`Pr(>|z|)`)
#--------------------------------------------------------------------------------
#sensitivity analysis (stratified by retirement status)
#retired
b<-NA

fit<-glm(pain_status ~ female + high_edu + agegroup + liverural  + height + high_edu_parent + hukou, family=binomial(link = "probit"), weights=weight, data=subset(CHARLS_pain_imputed,retired == 1))

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[2:3,])
tidy$label<-'pain_status'
b<-rbind(b,tidy)

fit<-glm(pain_status ~ female*high_edu + agegroup + liverural + height + high_edu_parent + hukou, family=binomial(link = "probit"), weights=weight, data=subset(CHARLS_pain_imputed,retired == 1))

summary(fit)
epi.interaction(model = fit, coef= c(2,3,11), param ="product",conf.level = 0.95)


tidy<-as.data.frame(summary(fit)$coefficients[c(2:3,11),])
tidy$label<-'pain_status+interaction'
b<-rbind(b,tidy)

fit<-glm(pain_med ~ female + high_edu + agegroup + liverural  + height + high_edu_parent + hukou, family=binomial(link = "probit"), weights=weight, data=subset(CHARLS_pain_imputed, pain_status == 1 & retired == 1))

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[2:3,])
tidy$label<-'pain_med'
b<-rbind(b,tidy)

fit<-glm(pain_med ~ female*high_edu + agegroup + liverural + height + high_edu_parent + hukou, family=binomial(link = "probit"), weights=weight, data=subset(CHARLS_pain_imputed, pain_status == 1 & retired == 1))

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[c(2:3,11),])
tidy$label<-'pain_med+interaction'
b<-rbind(b,tidy)
b<-b[-1,]


b$OR<-sprintf("%.2f", b$Estimate)
b$ci<-paste0("(",sprintf("%.2f", b$Estimate-1.96*b$`Std. Error`),", ",sprintf("%.2f", b$Estimate+1.96*b$`Std. Error`),")")
b$orci<-paste0(b$OR," ",b$ci)
b$p<-sprintf("%.4f", b$`Pr(>|z|)`)

#unretired
b<-NA

fit<-glm(pain_status ~ female + high_edu + agegroup + liverural  + height + high_edu_parent + hukou, family=binomial(link = "probit"), weights=weight, data=subset(CHARLS_pain_imputed,retired == 0))

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[2:3,])
tidy$label<-'pain_status'
b<-rbind(b,tidy)

fit<-glm(pain_status ~ female*high_edu + agegroup + liverural + height + high_edu_parent + hukou, family=binomial(link = "probit"), weights=weight, data=subset(CHARLS_pain_imputed,retired == 0))

summary(fit)
epi.interaction(model = fit, coef= c(2,3,11), param ="product",conf.level = 0.95)


tidy<-as.data.frame(summary(fit)$coefficients[c(2:3,11),])
tidy$label<-'pain_status+interaction'
b<-rbind(b,tidy)

fit<-glm(pain_med ~ female + high_edu + agegroup + liverural  + height + high_edu_parent + hukou, family=binomial(link = "probit"), weights=weight, data=subset(CHARLS_pain_imputed, pain_status == 1 & retired == 0))

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[2:3,])
tidy$label<-'pain_med'
b<-rbind(b,tidy)

fit<-glm(pain_med ~ female*high_edu + agegroup + liverural + height + high_edu_parent + hukou, family=binomial(link = "probit"), weights=weight, data=subset(CHARLS_pain_imputed, pain_status == 1 & retired == 0))

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[c(2:3,11),])
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
CHARLS_pain_imputed$education_cont <- as.numeric(CHARLS_pain_imputed$education_cont)

#Recode as every 10 percent increase
CHARLS_pain_imputed <- CHARLS_pain_imputed %>% 
  mutate(edu_rank = percent_rank(education_cont)*10)

b<-NA

fit<-glm(pain_status ~ female + edu_rank + agegroup + liverural  + height + high_edu_parent + hukou, family=binomial(link = "probit"), weights=weight, data=CHARLS_pain_imputed)

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[2:3,])
tidy$label<-'pain_status'
b<-rbind(b,tidy)

fit<-glm(pain_status ~ female*edu_rank + agegroup + liverural + height + high_edu_parent + hukou, family=binomial(link = "probit"), weights=weight, data=CHARLS_pain_imputed)

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[c(2:3,11),])
tidy$label<-'pain_status+interaction'
b<-rbind(b,tidy)

fit<-glm(pain_med ~ female + edu_rank + agegroup + liverural  + height + high_edu_parent + hukou, family=binomial(link = "probit"), weights=weight, data=subset(CHARLS_pain_imputed, pain_status == 1))

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[2:3,])
tidy$label<-'pain_med'
b<-rbind(b,tidy)

fit<-glm(pain_med ~ female*edu_rank + agegroup + liverural + height + high_edu_parent + hukou, family=binomial(link = "probit"), weights=weight, data=subset(CHARLS_pain_imputed, pain_status == 1))

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[c(2:3,11),])
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
summary(glm(pain_status ~ age + high_edu + high_edu_parent + liverural + hukou + height, family=binomial, data=subset(CHARLS_pain_imputed, sex == 1)))
summary(glm(pain_status ~ age + high_edu*liverural + high_edu_parent + hukou + height, family=binomial, data=subset(CHARLS_pain_imputed, sex == 1)))

summary(glm(pain_status ~ age + high_edu + high_edu_parent + liverural + hukou + height, family=binomial, weights=weight, data=subset(CHARLS_pain_imputed, sex == 1)))

#women
summary(glm(pain_status ~ age + high_edu + high_edu_parent + liverural + hukou + height, family=binomial, data=subset(CHARLS_pain_imputed, sex == 2)))
summary(glm(pain_status ~ age + high_edu*liverural + high_edu_parent + hukou + height, family=binomial, data=subset(CHARLS_pain_imputed, sex == 2)))

summary(glm(pain_status ~ age + high_edu + high_edu_parent + liverural + hukou + height, family=binomial, weights=weight, data=subset(CHARLS_pain_imputed, sex == 2)))

#both men and women with interaction between sex and education
#recode sex for easier interpretation
CHARLS_pain_imputed$female[CHARLS_pain_imputed$sex == 2] <- 1
CHARLS_pain_imputed$female[CHARLS_pain_imputed$sex == 1] <- 0
CHARLS_pain_imputed$female <- as.factor(CHARLS_pain_imputed$female)
#adding parental education and height as proxy for early life conditions show almost identical resutls
summary(glm(pain_status ~ high_edu*female + age + liverural + hukou, family=binomial, weights=weight, data=CHARLS_pain_imputed))
summary(glm(pain_status ~ high_edu*female + age + high_edu_parent + liverural + hukou + height, family=binomial, weights=weight, data=CHARLS_pain_imputed))

#sensitivity analysis (additionally adjusted for physical health covariates) show similar results
summary(glm(pain_status ~ high_edu*female + agegroup + liverural + mstat + hhwealth + hukou + lbrf + if_arthritis + if_cancer + num_6disease + drink_heavy + smoke, 
            family=binomial, weights=weight, data=CHARLS_pain_imputed))

#RERI analysis for interaction (additive scale)
#No interaction based on the additive scale: need to think about how to interpret this as addtive and multiplicative are in the same direction but did not achieve statistical significance
CHARLS_inter<-glm(pain_status ~ high_edu*female + agegroup + liverural + mstat + hhwealth + hukou, family=binomial, weights=weight, data=CHARLS_pain_imputed)
epi.interaction(model = CHARLS_inter, coef = c(2,3,14), param = "product", conf.level = 0.95)

#sensitivity analysis using education as rank
#Similar result using percentile rank education
CHARLS_pain_imputed$education_cont <- as.numeric(CHARLS_pain_imputed$education_cont)

#Recode as every 10 percent increase
CHARLS_pain_imputed <- CHARLS_pain_imputed %>% 
  mutate(edu_rank = percent_rank(education_cont)*10)
  
summary(glm(pain_status ~ edu_rank*female + agegroup + liverural + mstat + hhwealth + hukou + if_arthritis + if_cancer + num_6disease + drink_heavy + smoke, 
            family=binomial, weights=weight, data=CHARLS_pain_imputed))

#regression analysis for pain management
#no interaction obeserved between sex and education; women more likely to take medicine but not people who have higher education
table(CHARLS_pain_imputed$pain_med)
table(CHARLS_pain_imputed$pain_status)
table(CHARLS_pain_imputed$pain_status, CHARLS_pain_imputed$pain_med) #2952 out of 4718 taking medicine or management measures
summary(glm(pain_med ~ agegroup + high_edu + female + liverural + mstat + hhwealth + hukou, 
            family=binomial, weights=weight, data=subset(CHARLS_pain_imputed, pain_status == 1)))

summary(glm(pain_med ~ agegroup + high_edu*female + liverural + mstat + hhwealth + hukou, 
            family=binomial, weights=weight, data=subset(CHARLS_pain_imputed, pain_status == 1)))

#probit models
#pain
summary(glm(pain_status ~ high_edu*female + age + high_edu_parent + liverural + hukou + height, 
            family=binomial(link = "probit"), weights=weight, data=CHARLS_pain_imputed))

#pain management
summary(glm(pain_med ~ high_edu + female + age + high_edu_parent + liverural + hukou + height, 
            family=binomial(link = "probit"), weights=weight, data=subset(CHARLS_pain_imputed, pain_status == 1)))



