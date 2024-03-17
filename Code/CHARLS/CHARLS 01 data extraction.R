#setwd("H:/.shortcut-targets-by-id/1HabiYPUMbe3Be1C6JGRH6MI3u1x2Dzfh/Pain research/Data analysis")
setwd("G:/My Drive/Pain research/Data analysis")
#use CHARLS 2015 (Wave 3) due to binary answer to pain

################################################################################
#read in Harmonized CHARLS
library(haven)
#HCHARLS <- read_dta("E:/CHARLS data/H_CHARLS_D_Data.dta")
HCHARLS <- read_dta("/Users/liuchunyu/Documents/Lumey New/CHARLS/CHARLS/H_CHARLS_D_Data.dta")

#-----------------------
#individual-level demographic covariate
demog <- subset(HCHARLS, select = c(ID, householdID, communityID,
                                    r3iwstat, #w3 r interview status
                                    r3wtrespb, #w3 individual weight with HH/ind non-response adju
                                    r3agey, ragender, raeduc_c, raeducl, #age, gender, education multiple group, education
                                    ramomeducl, radadeducl, # parental education
                                    r3mstat, #marital status
                                    r3lbrf_c #w3 r labor force status
                                    ))
names(demog) <- c("ID", "householdID", "communityID",
                  "IWSTAT", "WEIGHT", 
                  "age", "sex", "education_cont", "education",
                  "edu_mom", "edu_dad",
                  "mstat", "lbrf")

#-----------------------
#household-level covariate
hhcov <- subset(HCHARLS, select = c(r3hukou, h3rural, #hukou, live in rural
                                    h3atotb, #w3 Asset: r+s total wealth
                                    hh3itot, #w3 income: HHold total household income
                                    hh3ctot, #w3 total household consumption
                                    h3hhres #w3 number of people living in this household
                                    ))
names(hhcov) <- c("hukou", "liverural", 
                  "hhwealth", "hhincome", "hhconsum", "hhsize")

#-----------------------
#individual-level health covariate
healthcov <- subset(HCHARLS, select = c(r3shlta, #w3 r Self-report of health alt
                                        r3adlab_c, #w3 r Some Diff-6 item ADL scale
                                        r3iadlza, #w3 r Some Diff-IADLs:summary /0-5
                                        r3cesd10, #w3 r CESD Score
                                        r3vgactx_c, #w3 r # days/wk vigorous physical activity or exer
                                        r3mdactx_c, #w3 r # days/wk moderate physical activity or exer
                                        r3drinkr_c, #w3 r range of # drinks/day
                                        r3smokev, #w3 r smoke ever
                                        r3smoken, #w3 r smoke now
                                        r3higov, #w3 r cover by public health insurance
                                        r3hipriv, #w3 r cover by private health ins
                                        r3hiothp #w3 r cover by other health ins
                                        ))
names(healthcov) <- c("SHLT", "ADL", "IADL", "CESD",
                      "vigor_act", "moder_act",
                      "num_drink", 
                      "smoke_ever", "smoke_now",
                      "pub_ins", "pri_ins", "oth_ins")

#-----------------------
#disease covariates
disease <- subset(HCHARLS, select = c(r3arthre, #w3 r ever had arthritis
                                      r3cancre, #w3 r ever had cancer
                                      r3hibpe, #w3 r Ever had high blood pressure
                                      r3diabe, #w3 r ever had diabetes
                                      r3lunge, #w3 r ever had lung disease
                                      r3hearte, #w3 r ever had heart problem
                                      r3stroke, #w3 r ever had stroke
                                      r3psyche, #w3 r ever had psych problem
                                      r3mheight #w3 height in meters
                                      ))

#single out indicators of arthritis and cancer
names(disease)[1] <- "if_arthritis"
names(disease)[2] <- "if_cancer"
names(disease)[9] <- "height"

#sum all other 6 diseases
disease$num_6disease <- rowSums(disease[, 3:8], na.rm = T)
table(disease$num_6disease)

#extract the variables we only need
disease <- subset(disease, select = c(if_arthritis, if_cancer, num_6disease, height))

#------------------------------
#merge all HCHARLS measures
hchalrs_cov <- cbind(demog, hhcov, healthcov, disease)
colnames(hchalrs_cov)

################################################################################
#read in CHARLS raw data 2015
#pain-related questions are in D HEALTH STATUS AND FUNCTIONING
CHARLS2015_D <- read_dta("/Users/liuchunyu/Documents/Lumey New/CHARLS/CHARLS/CHARLS2015r/Health_Status_and_Functioning.dta")
table(CHARLS2015_D$da041, exclude = NULL)
table(CHARLS2015_D$da042_w2_1s1, exclude = NULL)
pain <- subset(CHARLS2015_D, select = c(ID, 
                                        da041,#if/how often feel pain
                                        da042_w2_1s1, da042_w2_1s2, da042_w2_1s3,
                                        da042_w2_1s4, da042_w2_1s5, da042_w2_1s6 #if take measure to manage pain
                                        ))
names(pain) <- c("ID",
                 "pain_status",
                 "pain_med_1", "pain_med_2", "pain_med_3", 
                 "pain_med_4", "pain_med_5", "pain_med_6")

#merge Harmonized and raw CHARLS
pain_charls <- merge(hchalrs_cov, pain, by= "ID", all.y = T)
colnames(pain_charls)
write.csv(pain_charls, "pain_charls.csv")
