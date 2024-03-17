#read data
#read hrs

library(psych)
library(car)
library(tidyverse)
library(tableone)
library(kableExtra)
library(nnet)
library(survey)
library(mice)
library(lsmeans)
library(ggforce)
library(epiR)

setwd("/Users/liuchunyu/Documents/Pain/Data")
HRS_pain_imputed<-read.csv('pain_hrs_imputed2.csv')
HRS_pain_imputed<-HRS_pain_imputed[,-1]

#read elsa
ELSA_pain_imputed<-read.csv('pain_elsa_imputed2.csv')
ELSA_pain_imputed<-ELSA_pain_imputed[,-1]
#read charls
CHARLS_pain_imputed<-read.csv('pain_charls_imputed2.csv')
CHARLS_pain_imputed<-CHARLS_pain_imputed[,-1]
#read lasi
LASI_pain_imputed<-read.csv('pain_lasi_imputed2.csv')
LASI_pain_imputed<-LASI_pain_imputed[,-1]

#HRS
HRS_inter<-glm(pain_status ~ female + high_edu + agegroup + liverural  + height + high_edu_parent + race, family=binomial(link = "probit"), weights=weight, data=HRS_pain_imputed)

HRS_inter

HRS_inter.rg <- ref.grid(HRS_inter)
HRS.pain_adjusted <- as.data.frame(lsmeans(HRS_inter.rg, ~ high_edu + female, type = "response"))
HRS.pain_adjusted<-HRS.pain_adjusted[order(HRS.pain_adjusted$high_edu),]

#ELSA
ELSA_inter<-glm(pain_status ~ female + high_edu + agegroup   + height + high_edu_parent + race, family=binomial(link = "probit"), weights=weight, data=ELSA_pain_imputed)

ELSA_inter

ELSA_inter.rg <- ref.grid(ELSA_inter)
ELSA.pain_adjusted <- as.data.frame(lsmeans(ELSA_inter.rg, ~ high_edu + female, type = "response"))
ELSA.pain_adjusted<-ELSA.pain_adjusted[order(ELSA.pain_adjusted$high_edu),]

#CHARLS
CHARLS_inter<-glm(pain_status ~ female*high_edu + agegroup + liverural + height + high_edu_parent + hukou, family=binomial(link = "probit"), weights=weight, data=CHARLS_pain_imputed)

CHARLS_inter

CHARLS_inter.rg <- ref.grid(CHARLS_inter)
CHARLS.pain_adjusted <- as.data.frame(lsmeans(CHARLS_inter.rg, ~ high_edu + female, type = "response"))
CHARLS.pain_adjusted<-CHARLS.pain_adjusted[order(CHARLS.pain_adjusted$high_edu),]

#LASI
LASI_inter<-glm(pain_status ~ female*high_edu + agegroup + liverural + height + high_edu_parent + caste, family=binomial(link = "probit"), weights=weight, data=LASI_pain_imputed)

LASI_inter

LASI_inter.rg <- ref.grid(LASI_inter)
LASI.pain_adjusted <- as.data.frame(lsmeans(LASI_inter.rg, ~ high_edu + female, type = "response"))
LASI.pain_adjusted<-LASI.pain_adjusted[order(LASI.pain_adjusted$high_edu),]


bmi_fig <- rbind(HRS.pain_adjusted, ELSA.pain_adjusted, CHARLS.pain_adjusted, LASI.pain_adjusted)
bmi_fig$position <- c(1:4, 6:9, 11:14, 16:19)

# bmi_fig1$ci<-paste0(sprintf("%.2f",bmi_fig$lsmean)," ","(",sprintf("%.2f", bmi_fig$asymp.LCL),", ",sprintf("%.2f", bmi_fig$asymp.UCL),")")
# write.csv(bmi_fig1,"figure.data.csv")

#####make the figure

jpeg("pain4.jpg", res = 300, w = 4000, h = 3000)

ggplot(bmi_fig, aes(position, lsmean, color=as.factor(female))) +        
  geom_point(shape=18, size = 5, position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.5, size = 1, position = position_dodge(0.5)) +
  scale_x_continuous(limits = c(0.5, 19.5), breaks = c(1.5,3.5,6.5,8.5,11.5,13.5,16.5,18.5), label = c("Lower", "Higher", "Lower", "Higher", "Lower", "Higher", "Lower", "Higher")) +
  scale_y_continuous(limits = c(-0.01, 0.7), breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7), 
                     label = c(0, 10, 20, 30, 40, 50, 60, 70)) +
  scale_color_manual("Gender", values = c("0" = "#0072B2", "1" = "#D55E00"), labels = c("Men", "Women"))  + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"), axis.text = element_text(size=16), axis.title = element_text(size=16),
        legend.key.height = unit(0.75, "cm"), legend.text = element_text(size = 16), legend.position=c(0.12, 0.12),
        legend.key = element_blank(), legend.box="vertical", legend.title = element_text(size = 16),
        strip.text.x = element_text(size = 16), 
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'),
        plot.title = element_text(size = 18, hjust = 0.5),
        axis.title.x = element_text(margin = margin(t = 10),size=18), 
        axis.title.y = element_text(margin = margin(r = 10),size=18))+
  geom_vline(xintercept = c(5, 10, 15), color = 'black', linetype = 'dashed', size = 1) +
  annotate(geom="text", x = 2.5, y = 0.7, color = 'black', size = 8, label="US") +
  annotate(geom="text", x = 7.5, y = 0.7, color = 'black', size = 8, label="England") +
  annotate(geom="text", x = 12.5, y = 0.7, color = 'black', size = 8, label="China") +
  annotate(geom="text", x = 17.5, y = 0.7, color = 'black', size = 8, label="India") +
  xlab("Education levels") +
  ylab("Prevalence of pain (%)")

dev.off()

jpeg("pain3.jpg", res = 300, w = 4000, h = 3000)

ggplot(bmi_fig, aes(position, lsmean, color=as.factor(female))) +        
  geom_point(shape=18, size = 5, position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.5, size = 1, position = position_dodge(0.5)) +
  scale_x_continuous(limits = c(0.5, 19.5), breaks = c(1.5,3.5,6.5,8.5,11.5,13.5,16.5,18.5), label = c("Lower", "Higher", "Lower", "Higher", "Lower", "Higher", "Lower", "Higher")) +
  scale_y_continuous(limits = c(-0.01, 0.7), breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7), 
					 label = c(0, 10, 20, 30, 40, 50, 60, 70)) +
  scale_color_manual("Gender", values = c("0" = "#0072B2", "1" = "#D55E00"), labels = c("Men", "Women"))  + 
	theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
		  axis.line = element_line(colour = "black"), axis.text = element_text(size=16), axis.title = element_text(size=16),
		  legend.key.height = unit(0.75, "cm"), legend.text = element_text(size = 16), legend.position=c(0.12, 0.12),
		  legend.key = element_blank(), legend.box="vertical", legend.title = element_text(size = 16),
		  strip.text.x = element_text(size = 16), 
		  legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'),
		  plot.title = element_text(size = 18, hjust = 0.5),
	  axis.title.x = element_text(margin = margin(t = 10),size=18), 
	  axis.title.y = element_text(margin = margin(r = 10),size=18))+
	geom_vline(xintercept = c(5, 10, 15), color = 'black', linetype = 'dashed', size = 1) +
	annotate(geom="text", x = 2.5, y = 0.7, color = 'black', size = 8, label="US") +
	annotate(geom="text", x = 7.5, y = 0.7, color = 'black', size = 8, label="England") +
	annotate(geom="text", x = 12.5, y = 0.7, color = 'black', size = 8, label="China") +
	annotate(geom="text", x = 17.5, y = 0.7, color = 'black', size = 8, label="India") +
	xlab("Education levels: Low vs. High") +
	ylab("Prevalence of pain (%)")
  
dev.off()

jpeg("pain2.jpg", res = 300, w = 4000, h = 3000)

ggplot(bmi_fig, aes(position, lsmean, color=as.factor(female))) +        
  geom_point(shape=18, size = 5, position = position_dodge(0.5)) +
  geom_errorbar(aes(ymin = asymp.LCL, ymax = asymp.UCL), width = 0.5, size = 1, position = position_dodge(0.5)) +
  scale_x_continuous(limits = c(0.5, 19.5), breaks = c(1.5,3.5,6.5,8.5,11.5,13.5,16.5,18.5), label = c("Low", "High", "Low", "High", "Low", "High", "Low", "High")) +
  scale_y_continuous(limits = c(-0.01, 0.7), breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7), 
					 label = c(0, 10, 20, 30, 40, 50, 60, 70)) +
  scale_color_manual("Gender", values = c("0" = "#0072B2", "1" = "#D55E00"), labels = c("Male", "Female"))  + 
	theme_classic()+
	theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
		  axis.line = element_line(colour = "black"), axis.text = element_text(size=16), axis.title = element_text(size=16),
		  legend.key.height = unit(0.75, "cm"), legend.text = element_text(size = 16), legend.position=c(0.12, 0.12),
		  legend.key = element_blank(), legend.box="vertical", legend.title = element_text(size = 16),
		  strip.text.x = element_text(size = 16), 
		  legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid'),
		  plot.title = element_text(size = 18, hjust = 0.5),
	  axis.title.x = element_text(margin = margin(t = 10),size=18), 
	  axis.title.y = element_text(margin = margin(r = 10),size=18))+
	geom_vline(xintercept = c(5, 10, 15), color = 'black', linetype = 'dashed', size = 1) +
	annotate(geom="text", x = 2.5, y = 0.7, color = 'black', size = 8, label="US") +
	annotate(geom="text", x = 7.5, y = 0.7, color = 'black', size = 8, label="England") +
	annotate(geom="text", x = 12.5, y = 0.7, color = 'black', size = 8, label="China") +
	annotate(geom="text", x = 17.5, y = 0.7, color = 'black', size = 8, label="India") +
	xlab("Education levels: Low vs. High") +
	ylab("Prevalence of pain (%)")
  
dev.off()








