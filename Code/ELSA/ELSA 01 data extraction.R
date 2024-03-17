setwd("/Users/liuchunyu/Documents/Pain/Data")
#use ELSA 2018 

################################################################################
#read in Harmonized CHARLS
#read in Harmonized HRS
library(haven)
ELSA <- read_dta("/Users/liuchunyu/Documents/Pain/Data/ELSA/H_ELSA_g3.dta")

#-----------------------
#individual-level demographic covariate
demog <- subset(ELSA, select = c(idauniq,hh9hhid,pn,
									r9iwstat, #w9 r interview status
									r9cwtresp,#r person-level cross-sectional weight, core sam
									r9agey, ragender, raedyrs_e,raeducl,#age, gender, education
									ramomeduage,radadeduage,
									r8mheight,r6mheight,r4mheight,r2mheight,
									r9mstat,  #marital status
									r9lbrf_e #w9 r labor force status
									))
names(demog) <- c("ID", "householdID", "pnID",
				  "IWSTAT", "WEIGHT", 
				  "age", "sex", "education_cont","education",
				  "edu_mom", "edu_dad",
				  "height1","height2","height3","height4",
				  "mstat", "lbrf")
				  

#-----------------------
#household-level covariate
hhcov <- subset(ELSA, select = c(raracem,
									h9atotb, #w9 Asset: r+s total wealth
									h9itot, #w9 income: HHold total household income
									hh9ctot, #w9 total household consumption
									h9hhres #w9 number of people living in this household
									))
names(hhcov) <- c("race", 
  "hhwealth", "hhincome","hhconsum", "hhsize")
  
#-----------------------
#individual-level health covariate
healthcov <- subset(ELSA, select = c(r9shlt, #w14 r Self-report of health alt
										r9adltot6, #w9 r Some Diff-6 item ADL scale
										#(bathing, dressing, eating, getting in and out of bed, using the toilet, and walking across a room.)
										r9iadltot1_e, #w9 r any diff-total iadls/0-7
										#(managing money, taking medications, shopping for groceries, preparing meals, using a map, and using the phone，doing housework)
										r9cesd, #w9 r CESD Score
										r9vgactx_e, #w9 r # R Freq vigorous phys activ {finer scale} 0-5
										r9mdactx_e, #w9 r # freq moderate phys activ {finer scale} 0-5
										r9drinkwn_e, #w9 r # drinks/wk when drinks
										r9smokev, #w9 r smoke ever
										r9smoken, #w9 r smoke now
										r9hipriv, #w9 R covered by private health insurance
										r9hiltc_e #w9 R has long term care ins
										))
names(healthcov) <- c("SHLT", "ADL", "IADL", "CESD",
					  "vigor_act", "moder_act",
					  "num_drink", 
					  "smoke_ever", "smoke_now",
					 	 "pri_ins", "long_ins")  
  
healthcov$num_drink<-healthcov$num_drink/7
#-----------------------
#disease covariates
disease <- subset(ELSA, select = c(r9arthre, #w14 r ever had arthritis
									  r9cancre, #w14 r ever had cancer
									  r9hibpe, #w14 r Ever had high blood pressure
									  r9diabe, #w14 r ever had diabetes
									  r9lunge, #w14 r ever had lung disease
									  r9hearte, #w14 r ever had heart problem
									  r9stroke, #w14 r ever had stroke
									  r9psyche #w14 r ever had psych problem
									  ))

#single out indicators of arthritis and cancer
names(disease)[1] <- "if_arthritis"
names(disease)[2] <- "if_cancer"  

#sum all other 6 diseases
disease$num_6disease <- rowSums(disease[, 3:8], na.rm = T)
table(disease$num_6disease)

#extract the variables we only need
disease <- subset(disease, select = c(if_arthritis, if_cancer, num_6disease))

################################################################################
#Pain variables
pain <- subset(ELSA, select = c(r9painfr,#w14 r frequent problems with pain
										r9painlv#w14 r usual level of pain
									
										))
names(pain) <- c("pain_status","pain_level")

#------------------------------
#merge all HCHARLS measures
pain_elsa <- cbind(demog, hhcov, healthcov, disease, pain)
colnames(pain_elsa)

write.csv(pain_elsa, "pain_elsa.csv")




