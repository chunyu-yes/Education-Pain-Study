library(lsmeans)
library(ggforce)
library(epiR)

ELSA_pain_imputed<-read.csv("pain_elsa_imputed.csv")
ELSA_pain_imputed<-ELSA_pain_imputed[,-1]
#--------------------------------------------------------------------------------
#need to rescale weight for regression analysis otherwise always statistically significant (use the sum of weight and # of participant)
ELSA_pain_imputed$weight <- ELSA_pain_imputed$WEIGHT/sum(ELSA_pain_imputed[, 'WEIGHT'])*7316

#descriptive table 1
#recode education as 1=high vs 0=low education
table(ELSA_pain_imputed$education)
ELSA_pain_imputed$high_edu[ELSA_pain_imputed$education == 2 | ELSA_pain_imputed$education == 3] <- 1
ELSA_pain_imputed$high_edu[ELSA_pain_imputed$education == 1] <- 0
table(ELSA_pain_imputed$high_edu)

#add highest parental education
table(ELSA_pain_imputed$edu_dad)
table(ELSA_pain_imputed$edu_mom)
#get the highest parental education
ELSA_pain_imputed$edu_parent <- ifelse(ELSA_pain_imputed$edu_dad > ELSA_pain_imputed$edu_mom, ELSA_pain_imputed$edu_dad, ELSA_pain_imputed$edu_mom)
table(ELSA_pain_imputed$edu_parent)
#dictomize edu_parent as high_edu_parent
ELSA_pain_imputed$high_edu_parent <- ifelse(ELSA_pain_imputed$edu_parent >= 6, 1, 0)
table(ELSA_pain_imputed$high_edu_parent)

#***********************
#pain status
table(ELSA_pain_imputed$pain_status)
prop.table(table(ELSA_pain_imputed$pain_status))#~41.74% had pain

# table(ELSA_pain_imputed$pain_interfere)
# table(ELSA_pain_imputed$pain_interfere,ELSA_pain_imputed$pain_status)
# ELSA_pain_imputed$pain_interfere[ELSA_pain_imputed$pain_status == 0] <- NA

#pain medication
# table(ELSA_pain_imputed$pain_med)
# table(ELSA_pain_imputed$pain_med,ELSA_pain_imputed$pain_status)
# 
# ELSA_pain_imputed$pain_med[ELSA_pain_imputed$pain_status == 0] <- NA
# table(ELSA_pain_imputed$pain_med)
# table(ELSA_pain_imputed$pain_status, ELSA_pain_imputed$pain_med)

#pain level
table(ELSA_pain_imputed$pain_level)
table(ELSA_pain_imputed$pain_level,ELSA_pain_imputed$pain_status)
ELSA_pain_imputed$pain_level[ELSA_pain_imputed$pain_level == 0] <- NA
ELSA_pain_imputed$pain_level[ELSA_pain_imputed$pain_level == 1|ELSA_pain_imputed$pain_level == 2] <- 0
ELSA_pain_imputed$pain_level[ELSA_pain_imputed$pain_level == 3] <- 1
table(ELSA_pain_imputed$pain_level)

#define factor 
colnames(ELSA_pain_imputed)

ELSA_pain_imputed[, c(7,40,48,50,18,37,38)] <- lapply(ELSA_pain_imputed[, c(7,40,48,50,18,37,38)], as.factor)

#2 is women and 1 is men
table(ELSA_pain_imputed$sex)

ELSA_pain_imputed$female[ELSA_pain_imputed$sex == 2] <- 1
ELSA_pain_imputed$female[ELSA_pain_imputed$sex == 1] <- 0
ELSA_pain_imputed$female <- as.factor(ELSA_pain_imputed$female)

write.csv(ELSA_pain_imputed,'pain_elsa_imputed2.csv')

#Unweighted, use N
#overall
t1_unweighted = CreateTableOne(vars = c("age", "agegroup", "sex","high_edu","high_edu_parent", "height","race", 
"pain_status","pain_level"), 
							   data=ELSA_pain_imputed, includeNA = T)
print(t1_unweighted, showAllLevels = TRUE, catDigits=2, printToggle = FALSE) %>% 
  knitr::kable(caption = "unweighted descriptive charateristic (ELSA)",
			   booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")

#stratified by sex
t1_unweighted_stratified = CreateTableOne(vars = c("age", "agegroup", "sex","high_edu","high_edu_parent", "height","race", 
"pain_status","pain_level"), 
										  data=ELSA_pain_imputed, addOverall = F,  strata="sex", test = T, includeNA = T)
print(t1_unweighted_stratified, showAllLevels = TRUE, catDigits=2, printToggle = FALSE) %>% 
  knitr::kable(caption = "unweighted descriptive charateristic (ELSA)",
			   booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")

## weighted. use weight %, mean and SD
## Create a weighted survey design object
charlsSvy <- svydesign(ids = ~ ID, weights = ~ weight, nest = F, data = ELSA_pain_imputed)

#overall
t1_weighted <- svyCreateTableOne(vars = c("age", "agegroup", "sex","high_edu","high_edu_parent", "height","race", 
"pain_status","pain_level"),
								 data=charlsSvy, includeNA = F)
print(t1_weighted, catDigits=2,  showAllLevels = TRUE,  printToggle = FALSE) %>% 
  knitr::kable(caption = "Weighted descriptive charateristic (ELSA)",
			   booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")

#stratified by sex
t1_weighted_stratified <- svyCreateTableOne(vars = c("age", "agegroup", "sex","high_edu","high_edu_parent", "height","race", 
"pain_status","pain_level"),
											data=charlsSvy, addOverall = F,  strata="sex", test = T, includeNA = T)
print(t1_weighted_stratified, catDigits=2,  showAllLevels = TRUE,  printToggle = FALSE) %>% 
  knitr::kable(caption = "Weighted descriptive charateristic (ELSA)",
			   booktabs = T, linesep = '') %>% 
  kable_styling(latex_options = "hold_position")

#--------------------------------------------------------------------------------
#table 2
b<-NA

fit<-glm(pain_status ~ female + high_edu + agegroup   + height + high_edu_parent + race, family=binomial(link = "probit"), weights=weight, data=ELSA_pain_imputed)

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[2:3,])
tidy$label<-'pain_status'
b<-rbind(b,tidy)

fit<-glm(pain_status ~ female*high_edu + agegroup  + height + high_edu_parent + race, family=binomial(link = "probit"), weights=weight, data=ELSA_pain_imputed)

epi.interaction(model = fit, coef= c(2,3,10), param ="product",conf.level = 0.95)

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[c(2:3,10),])
tidy$label<-'pain_status+interaction'
b<-rbind(b,tidy)

fit<-glm(pain_level ~ female + high_edu + agegroup   + height + high_edu_parent + race, family=binomial(link = "probit"), weights=weight, data=ELSA_pain_imputed)

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[2:3,])
tidy$label<-'pain_level'
b<-rbind(b,tidy)

fit<-glm(pain_level ~ female*high_edu + agegroup  + height + high_edu_parent + race, family=binomial(link = "probit"), weights=weight, data=ELSA_pain_imputed)

epi.interaction(model = fit, coef= c(2,3,10), param ="product",conf.level = 0.95)

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[c(2:3,10),])
tidy$label<-'pain_level+interaction'
b<-rbind(b,tidy)

# fit<-glm(pain_interfere ~ female + high_edu + agegroup   + height + high_edu_parent + race, family=binomial(link = "probit"), weights=weight, data=subset(ELSA_pain_imputed, pain_status == 1))
# 
# summary(fit)
# tidy<-as.data.frame(summary(fit)$coefficients[2:3,])
# tidy$label<-'pain_interfere'
# b<-rbind(b,tidy)
# 
# fit<-glm(pain_interfere ~ female*high_edu + agegroup  + height + high_edu_parent + race, family=binomial(link = "probit"), weights=weight, data=subset(ELSA_pain_imputed, pain_status == 1))
# 
# summary(fit)
# tidy<-as.data.frame(summary(fit)$coefficients[c(2:3,13),])
# tidy$label<-'pain_interfere+interaction'
# b<-rbind(b,tidy)
# 
# fit<-glm(pain_med ~ female + high_edu + agegroup   + height + high_edu_parent + race, family=binomial(link = "probit"), weights=weight, data=subset(ELSA_pain_imputed, pain_status == 1))
# 
# summary(fit)
# tidy<-as.data.frame(summary(fit)$coefficients[2:3,])
# tidy$label<-'pain_med'
# b<-rbind(b,tidy)
# 
# fit<-glm(pain_med ~ female*high_edu + agegroup  + height + high_edu_parent + race, family=binomial(link = "probit"), weights=weight, data=subset(ELSA_pain_imputed, pain_status == 1))
# 
# summary(fit)
# tidy<-as.data.frame(summary(fit)$coefficients[c(2:3,13),])
# tidy$label<-'pain_med+interaction'
# b<-rbind(b,tidy)
b<-b[-1,]


b$OR<-sprintf("%.2f", b$Estimate)
b$ci<-paste0("(",sprintf("%.2f", b$Estimate-1.96*b$`Std. Error`),", ",sprintf("%.2f", b$Estimate+1.96*b$`Std. Error`),")")
b$orci<-paste0(b$OR," ",b$ci)
b$p<-sprintf("%.4f", b$`Pr(>|z|)`)

#--------------------------------------------------------------------------------
#sensitivity analysis (with percentile education)
#Similar result using percentile rank education
ELSA_pain_imputed$education_cont <- as.numeric(ELSA_pain_imputed$education_cont)

#Recode as every 10 percent increase
ELSA_pain_imputed <- ELSA_pain_imputed %>% 
  mutate(edu_rank = percent_rank(education_cont)*10)

b<-NA

fit<-glm(pain_status ~ female + edu_rank + agegroup   + height + high_edu_parent + race, family=binomial(link = "probit"), weights=weight, data=ELSA_pain_imputed)

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[2:3,])
tidy$label<-'pain_status'
b<-rbind(b,tidy)

fit<-glm(pain_status ~ female*edu_rank + agegroup  + height + high_edu_parent + race, family=binomial(link = "probit"), weights=weight, data=ELSA_pain_imputed)

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[c(2:3,10),])
tidy$label<-'pain_status+interaction'
b<-rbind(b,tidy)

fit<-glm(pain_level ~ female + edu_rank + agegroup   + height + high_edu_parent + race, family=binomial(link = "probit"), weights=weight, data=ELSA_pain_imputed)

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[2:3,])
tidy$label<-'pain_level'
b<-rbind(b,tidy)

fit<-glm(pain_level ~ female*edu_rank + agegroup  + height + high_edu_parent + race, family=binomial(link = "probit"), weights=weight, data=ELSA_pain_imputed)

summary(fit)
tidy<-as.data.frame(summary(fit)$coefficients[c(2:3,10),])
tidy$label<-'pain_level+interaction'
b<-rbind(b,tidy)

# fit<-glm(pain_interfere ~ female + edu_rank + agegroup   + height + high_edu_parent + race, family=binomial(link = "probit"), weights=weight, data=subset(ELSA_pain_imputed, pain_status == 1))
# 
# summary(fit)
# tidy<-as.data.frame(summary(fit)$coefficients[2:3,])
# tidy$label<-'pain_interfere'
# b<-rbind(b,tidy)
# 
# fit<-glm(pain_interfere ~ female*edu_rank + agegroup  + height + high_edu_parent + race, family=binomial(link = "probit"), weights=weight, data=subset(ELSA_pain_imputed, pain_status == 1))
# 
# summary(fit)
# tidy<-as.data.frame(summary(fit)$coefficients[c(2:3,13),])
# tidy$label<-'pain_interfere+interaction'
# b<-rbind(b,tidy)
# 
# 
# fit<-glm(pain_med ~ female + edu_rank + agegroup   + height + high_edu_parent + race, family=binomial(link = "probit"), weights=weight, data=subset(ELSA_pain_imputed, pain_status == 1))
# 
# summary(fit)
# tidy<-as.data.frame(summary(fit)$coefficients[2:3,])
# tidy$label<-'pain_med'
# b<-rbind(b,tidy)
# 
# fit<-glm(pain_med ~ female*edu_rank + agegroup  + height + high_edu_parent + race, family=binomial(link = "probit"), weights=weight, data=subset(ELSA_pain_imputed, pain_status == 1))
# 
# summary(fit)
# tidy<-as.data.frame(summary(fit)$coefficients[c(2:3,13),])
# tidy$label<-'pain_med+interaction'
# b<-rbind(b,tidy)
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
summary(glm(pain_status ~ agegroup + high_edu + mstat + hhwealth + race, family=binomial, data=subset(ELSA_pain_imputed, sex == 1)))

summary(glm(pain_status ~ agegroup + high_edu + mstat + hhwealth + race, family=binomial, weights=weight, data=subset(ELSA_pain_imputed, sex == 1)))

#women
summary(glm(pain_status ~ agegroup + high_edu + mstat + hhwealth + race, family=binomial, data=subset(ELSA_pain_imputed, sex == 2)))

summary(glm(pain_status ~ agegroup + high_edu + mstat + hhwealth + race, family=binomial, weights=weight, data=subset(ELSA_pain_imputed, sex == 2)))

#both men and women with interaction between sex and education
#recode sex for easier interpretation
ELSA_pain_imputed$female[ELSA_pain_imputed$sex == 2] <- 1
ELSA_pain_imputed$female[ELSA_pain_imputed$sex == 1] <- 0
ELSA_pain_imputed$female <- as.factor(ELSA_pain_imputed$female)
summary(glm(pain_status ~ high_edu*female + agegroup + mstat + hhwealth + race, family=binomial, weights=weight, data=ELSA_pain_imputed))

#sensitivity analysis (additionally adjusted for physical health covariates) show similar results
summary(glm(pain_status ~ high_edu*female + agegroup + mstat + hhwealth + race + if_arthritis + if_cancer + num_6disease + drink_heavy + smoke, 
			family=binomial, weights=weight, data=ELSA_pain_imputed))

#RERI analysis for interaction (additive scale)
ELSA_inter<-glm(pain_status ~ high_edu*female + agegroup + mstat + hhwealth + race, family=binomial, weights=weight, data=ELSA_pain_imputed)
epi.interaction(model = ELSA_inter, coef = c(2,3,13), param = "product", conf.level = 0.95)



