setwd("/Users/liuchunyu/Documents/Pain/Data")
#use HRS 2018 r14

################################################################################
#read in Harmonized HRS
library(haven)
HHRS1 <- read_dta("/Users/liuchunyu/Documents/Pain/Data/HRS /randhrs1992_2020v1.dta")
HHRS2 <- read_dta("/Users/liuchunyu/Documents/Pain/Data/HRS /H_HRS_c.dta")

#-----------------------
#combine the two data
library(dplyr)
HHRS <- left_join(HHRS1,HHRS2,by=c('hhid','pn'))

#-----------------------
#individual-level demographic covariate
demog <- subset(HHRS, select = c(hhidpn.x,hhid,pn,
									r14iwstat, #w14 r interview status
									r14wtresp,#w14 individual weight with HH/ind non-response adju
									r14agey_b, ragender, raedyrs, raeducl,#age, gender, education
									ramomeducl, radadeducl,#
									r14height,
									r14mstat,  #marital status
									r14lbrf #w14 r labor force status
									))
names(demog) <- c("ID", "householdID", "pnID",
				  "IWSTAT", "WEIGHT", 
				  "age", "sex", "education_cont", "education",
				  "edu_mom", "edu_dad","height",
				  "mstat", "lbrf")

#-----------------------
#household-level covariate
hhcov <- subset(HHRS, select = c(raracem,rahispan,h14rural,
									h14atotb, #w14 Asset: r+s total wealth
									h14itot, #w14 income: HHold total household income
									#h14ctot, #w14 total household consumption
									h14hhres #w14 number of people living in this household
									))
names(hhcov) <- c("race1","race2","liverural", 
				  "hhwealth", "hhincome", "hhsize")

#-----------------------
#individual-level health covariate
healthcov <- subset(HHRS, select = c(r14shlt, #w14 r Self-report of health alt
										r14adltot_h, #w14 r Some Diff-6 item ADL scale
										#(bathing, dressing, eating, getting in and out of bed, using the toilet, and walking across a room.)
										r14iadltot_h, #w14 r any diff-total iadls/0-6
										#(managing money, taking medications, shopping for groceries, preparing meals, using a map, and using the phone.)
										r14cesd, #w14 r CESD Score
										r14vgactx, #w14 r # R Freq vigorous phys activ {finer scale} 0-5
										r14mdactx, #w14 r # freq moderate phys activ {finer scale} 0-5
										r14drinkn, #w14 r # drinks/day when drinks
										r14smokev, #w14 r smoke ever
										r14smoken, #w14 r smoke now
										r14higov, #w14 r is covered by Gov plan
										r14prpcnt, #w14 r Number of Private Insurance Plans
										r14hiothp#w14 r cover by other health ins
										))
names(healthcov) <- c("SHLT", "ADL", "IADL", "CESD",
					  "vigor_act", "moder_act",
					  "num_drink", 
					  "smoke_ever", "smoke_now",
					  "pub_ins", "pri_ins", "oth_ins")

healthcov$pri_ins[which(healthcov$pri_ins>1)]<-1 #change Number of Private Insurance Plans to whether have Private Insurance Plans

#-----------------------
#disease covariates
disease <- subset(HHRS, select = c(r14arthre, #w14 r ever had arthritis
									  r14cancre, #w14 r ever had cancer
									  r14hibpe, #w14 r Ever had high blood pressure
									  r14diabe, #w14 r ever had diabetes
									  r14lunge, #w14 r ever had lung disease
									  r14hearte, #w14 r ever had heart problem
									  r14stroke, #w14 r ever had stroke
									  r14psyche #w14 r ever had psych problem
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
pain <- subset(HHRS, select = c(r14painfr,#w14 r frequent problems with pain
										r14painlv,#w14 r usual level of pain
										r14paina,#w14 r pain interferes with normal activities
										r14rxpain#w14 r takes meds for pain
										))
names(pain) <- c("pain_status","pain_level","pain_interfere",
				 "pain_med")

#------------------------------
#merge all HCHARLS measures
pain_hrs <- cbind(demog, hhcov, healthcov, disease, pain)
colnames(pain_hrs)

write.csv(pain_hrs, "pain_hrs.csv")




