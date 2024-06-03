setwd("/Users/liuchunyu/Documents/Pain/Data")
#use LASI 

################################################################################
#read in Harmonized HRS
library(haven)
LASI <- read_dta("/Users/liuchunyu/Documents/Pain/Data/LASI/H_LASI_a3.dta")

#-----------------------
#individual-level demographic covariate
demog <- subset(LASI, select = c(prim_key,hhid,pn,
									r1iwstat, #w14 r interview status
									r1wtresp,#w14 individual weight with HH/ind non-response adju
									r1agey, ragender, raedyrs, raeducl,#age, gender, education
									rameduc_l,rafeduc_l,
									r1mheight,
									r1mstat,  #marital status
									r1lbrf_l, #w14 r labor force status
									r1sayret_l
									))
names(demog) <- c("ID", "householdID", "pnID",
				  "IWSTAT", "WEIGHT", 
				  "age", "sex", "education_cont","education",
				  "edu_mom", "edu_dad","height",
				  "mstat", "lbrf","retired")

#-----------------------
#household-level covariate
hhcov <- subset(LASI, select = c(r1caste,#caste system
	hh1rural,
									hh1atotb, #w14 Asset: r+s total wealth
									hh1itot, #w14 income: HHold total household income
									hh1ctot, #w14 total household consumption
									hh1hhres #w14 number of people living in this household
									))
names(hhcov) <- c("caste","liverural", 
				  "hhwealth", "hhincome","hhconsum","hhsize")

#-----------------------
#individual-level health covariate
healthcov <- subset(LASI, select = c(r1shlt, #w14 r Self-report of health alt
										r1adltot6, #w14 r Some Diff-6 item ADL scale
										#(bathing, dressing, eating, getting in and out of bed, using the toilet, and walking across a room.)
										r1iadltot_l, #Some Diff-IADLs:Total /0-7
										#(managing money, taking medications, shopping for groceries, preparing meals, using a map, and using the phone, and housework)
										r1cesd10_l, #w14 r CESD Score
										r1vgactx, #w14 r # R Freq vigorous phys activ {finer scale} 0-5
										r1mdactx, #w14 r # freq moderate phys activ {finer scale} 0-5
										r1drinkx_l, #r frequency of drinking in the past 3 months
										r1smokev, #w14 r smoke ever
										r1smoken, #w14 r smoke now
										r1higov, #w14 r is covered by Gov plan
										r1covr, #w1 r covered by employer plan
										r1hiothp, #w1 r covered by other ins
										r1hident, #w1 r covered by dental ins
										r1hidrug #w1 r drug expenses covered
										))
names(healthcov) <- c("SHLT", "ADL", "IADL", "CESD",
					  "vigor_act", "moder_act",
					  "num_drink", 
					  "smoke_ever", "smoke_now",
					  "pub_ins", "emp_ins", "oth_ins", "den_ins", "drug_ins")

#-----------------------
#disease covariates
disease <- subset(LASI, select = c(r1arthre, #w14 r ever had arthritis
									  r1cancre, #w14 r ever had cancer
									  r1hibpe, #w14 r Ever had high blood pressure
									  r1diabe, #w14 r ever had diabetes
									  r1lunge, #w14 r ever had lung disease
									  r1hearte, #w14 r ever had heart problem
									  r1stroke, #w14 r ever had stroke
									  r1psyche #w14 r ever had psych problem
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
pain <- subset(LASI, select = c(r1painfr,#w14 r frequent problems with pain
										r1painfrq,#w1 r frequency experiences pain
										r1paina,#w14 r pain interferes with normal activities
										r1rxpain#w14 r takes meds for pain
										))
names(pain) <- c("pain_status","pain_freq","pain_interfere",
				 "pain_med")

#------------------------------
#merge all LASI measures
pain_lasi <- cbind(demog, hhcov, healthcov, disease, pain)
colnames(pain_lasi)

#write.csv(pain_lasi, "pain_lasi.csv")
write.csv(pain_lasi, "pain_lasi_retired.csv")


