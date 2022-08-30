###Set up working directory, load packages
setwd("C:/Users/lmacconn/OneDrive - Umich/Desktop")
install.packages("haven")
library(haven)

##Reading VBS, LHS, and RAND data from SAS format (No LBS)
vbs_data <- read_sav("hrs2016vbs.sav")
vbsfcyto_data <- read_sas("flocyt2016.sas7bdat")
vbsimmune <- read_sas("vbs16aa.sas7bdat")
lhs_data <- read_sas("lhms1517a_r.sas7bdat")
hrs_data <- read_sas("randhrs1992_2018v2.sas7bdat")

##Converting to dataframes
vbs = data.frame(vbs_data)
vbsfcyto = data.frame(vbsfcyto_data)
lhs = data.frame(lhs_data)
hrs = data.frame(hrs_data)
vbs_sup= data.frame(vbsimmune)

###Extracting HRS LBS data (2012-2014 only)
install.packages("SAScii")
library(SAScii)

fn12 <-  "C:/Users/lmacconn/OneDrive - Umich/Desktop/H12LB_R.da"
sas.input12 <-  "C:/Users/lmacconn/OneDrive - Umich/Desktop/H12LB_R.sas"
hrs12_lbs<- read.SAScii(fn12, sas.input12)


fn14 <-  "C:/Users/lmacconn/OneDrive - Umich/Desktop/H14LB_R.da"
sas.input14 <-  "C:/Users/lmacconn/OneDrive - Umich/Desktop/H14LB_R.sas"
hrs14_lbs<- read.SAScii(fn14, sas.input14)

m114 <- "C:/Users/lmacconn/OneDrive - Umich/Desktop/H14M1_R.da" ##Reading in disability data 
m114sas <- "C:/Users/lmacconn/OneDrive - Umich/Desktop/H14M1_R.sas"
m1_14 <- read.SAScii(m114, m114sas)

m214 <- "C:/Users/lmacconn/OneDrive - Umich/Desktop/H14M2_R.da"
m214sas <- "C:/Users/lmacconn/OneDrive - Umich/Desktop/H14M2_R.sas"
m2_14 <- read.SAScii(m214, m214sas)

m112 <- "C:/Users/lmacconn/OneDrive - Umich/Desktop/H12M1_R.da"
m112sas <- "C:/Users/lmacconn/OneDrive - Umich/Desktop/H12M1_R.sas"
m1_12 <- read.SAScii(m112, m112sas)

m212 <- "C:/Users/lmacconn/OneDrive - Umich/Desktop/H12M2_R.da"
m212sas <- "C:/Users/lmacconn/OneDrive - Umich/Desktop/H12M2_R.sas"
m2_12 <- read.SAScii(m214, m212sas)

m12014 <- m1_14[,c("HHID", "PN", "OM002", "OM004", "OM005", "OM006", "OM007", "OM008", "OM009")]
m12012 <- m1_12[,c("HHID", "PN", "NM002", "NM004", "NM005", "NM006", "NM007", "NM008", "NM009")]

###Read in tracker file
tracker <- read_sas("trk2020tr_r.sas7bdat")
library(dplyr)
###Select all participants who completed a HRS Core from 2006-2016
tracker06_16 <- tracker %>% filter(KIWWAVE == 1 | LIWWAVE == 1 | MIWWAVE == 1 | NIWWAVE == 1 | OIWWAVE == 1 | PIWWAVE == 1)
nrow(tracker06_16)

### Select those who completed 2012 or 2014 HRS Core and were eligible for LBS
lbs2012_elig <-merge( (hrs12_lbs[which(hrs12_lbs$NLBELIG == 1),]), (tracker06_16[which(tracker06_16$NIWWAVE== 1),]), by=c("HHID", "PN"))
nrow(lbs2012_elig)

lbs2014_elig <- merge((hrs14_lbs[which(hrs14_lbs$OLBELIG ==1),]), (tracker06_16[which(tracker06_16$OIWWAVE ==1),]), by=c("HHID", "PN"))
nrow(lbs2014_elig)

###From those who completed 2014 HRS core and were eligible for LBS, select those who were eligible for LHMS2015
trackerLHS2015elig <- lbs2014_elig[which(lbs2014_elig$OIWWAVE == 1 & lbs2014_elig$LHMS15 != 99 ),]
nrow(trackerLHS2015elig)

###From those who completed 2012 HRS Core (part of that wave) & eligible for LBS, select those who were eligible for LHMS2017
trackerLHS2017elig <-lbs2012_elig[which(lbs2012_elig$PIWWAVE == 1 & lbs2012_elig$LHMS17FALL != 99 | lbs2012_elig$LHMS17SPR !=99),]
nrow(trackerLHS2017elig)

###Merge above, then select all those who completed LBS2012/2014 or LHS2015/2017
tracker_elig <- merge(trackerLHS2015elig, trackerLHS2017elig, by=c("HHID", "PN"), all.x = TRUE, all.y = TRUE)
trackerlbs_lhs <- tracker_elig[which(tracker_elig$LHMS15.x == 1 | tracker_elig$LHMS17FALL.y == 1 | tracker_elig$LHMS17SPR.y == 1 | tracker_elig$NLBCOMP !=5 | tracker_elig$OLBCOMP != 5),]
nrow(trackerlbs_lhs)


##Now select from above for VBS eligible -> consented -> complete and valid
##Must create variable as merging created multiple 
trackerlbs_lhs$VBS16ELIG = ifelse(trackerlbs_lhs$VBS16ELIG.x == 1 | trackerlbs_lhs$VBS16ELIG.y == 1, 1, 0)
trackerlbs_lhs$VBS16CONSENT = ifelse(trackerlbs_lhs$VBS16CONSENT.x == 1 | trackerlbs_lhs$VBS16CONSENT.y == 1, 1, 0)
trackerlbs_lhs$VBS16COMPLETE = ifelse(trackerlbs_lhs$VBS16COMPLETE.x == 1 | trackerlbs_lhs$VBS16COMPLETE.y == 1, 1, 0)
trackerlbs_lhs$VBS16VALID = ifelse(trackerlbs_lhs$VBS16VALID.x == 1 | trackerlbs_lhs$VBS16VALID.y == 1, 1, 0)
trackerlbs_lhs$ZEROWGT = ifelse(is.na(trackerlbs_lhs$VBS16WGTR.x) & is.na(trackerlbs_lhs$VBS16WGTR.y), 0, 1)

##Select for those who are eligible first
trackerlbs_lhs_vbselig <- trackerlbs_lhs[which(trackerlbs_lhs$VBS16ELIG== 1),]
nrow(trackerlbs_lhs_vbselig)

###From all eligible for VBS, select all that consented 
trackerlbs_lhs_vbsconsent <- trackerlbs_lhs_vbselig[which(trackerlbs_lhs_vbselig$VBS16CONSENT ==1),]
nrow(trackerlbs_lhs_vbsconsent)
##From all eligible and consented, select those with complete and valid test results
trackerlbs_lhs_vbs <- trackerlbs_lhs_vbsconsent[which(trackerlbs_lhs_vbsconsent$VBS16COMPLETE == 1 & trackerlbs_lhs_vbsconsent$VBS16VALID),]
nrow(trackerlbs_lhs_vbs)

##From all eligible, consented, complete, valid, select those with non-zero survey weights
tracker_final <- trackerlbs_lhs_vbs[which(trackerlbs_lhs_vbs$ZEROWGT == 1),]
nrow(tracker_final)

###Now create data set with full VBS data + tracker, then select for cases with complete outcome data
d1= merge(vbs, vbsfcyto, by=c("HHID", "PN"))
d2 = merge(d1, vbs_sup, by=c("HHID", "PN"))
vbs_tracker = merge(d2, tracker_final, by=c("HHID", "PN"))
vbs_tracker$OUTCOME = ifelse(is.na(vbs_tracker$PIL6) | is.na(vbs_tracker$PCRP) | is.na(vbs_tracker$PTNFR1) |is.na(vbs_tracker$PCMVGE) | is.na(vbs_tracker$PCMVGINT) | is.na(vbs_tracker$PCD4N_PCT) | is.na(vbs_tracker$PCD4TEMRA_PCT) | is.na(vbs_tracker$PCD4T_PCT) | is.na(vbs_tracker$PCD8N_PCT) | is.na(vbs_tracker$PCD8TEMRA_PCT) | is.na(vbs_tracker$PCD8T_PCT), 0, 1)
vbs_complete = vbs_tracker %>% filter(OUTCOME == 1)

##Now merge in the LHS and RAND results
library(stringr)
lhs$HHID = lhs$hhid ##recode ID variables for merging
lhs$PN = lhs$pn

vbs_lhs = merge(vbs_complete, lhs, by=c("HHID", "PN"), all.x = TRUE, all.y = FALSE)

vbs_lhs$ID = paste0(vbs_lhs$HHID, vbs_lhs$PN) ##Create new ID to merge with RAND
vbs_lhs$HHIDPN = str_remove(vbs_lhs$ID, "^0+")

datav1 = merge(vbs_lhs, hrs, by = "HHIDPN", all.x=TRUE) ## This data set contains all participants in final sample

###Needed to add in functional limitations data 
m12012$ID = paste0(m12012$HHID, m12012$PN)
m12012$HHIDPN = str_remove(m12012$ID, "^0+")
datav2 = merge(datav1, m12012, by= "HHIDPN", all.x=TRUE)

datav3 <- datav2 %>% select(-HHID.x, -HHID.y, -PN.x, -PN.y) ###Avoid duplicates that cause issues later in code

m12014$ID = paste0(m12014$HHID, m12014$PN)
m12014$HHIDPN = str_remove(m12014$ID, "^0+")
data = merge(datav3, m12014, by="HHIDPN", all.x=TRUE) ###This is final data set (with disability data included)

###Begin Data Cleaning 
###Install packages for data cleaning and manipulation 
install.packages("dplyr")
install.packages("tidyr")
install.packages("janitor")
install.packages("gtsummary")
library(dplyr)
library(tidyr)
library(janitor)
library(gtsummary)

###Recode LHMS POIs into categorical variables 
data$orphanage <- factor(data$LH2A, c(1, 5), labels=c("Yes", "No"))
data$foster <- factor(data$LH2B, c(1, 5), labels=c("Yes", "No"))
data$prison_lhs <- factor(data$LH4A, c(1, 5), labels=c("Yes", "No"))
data$long_term_hospital <- factor(data$LH4B, c(1,5), labels=c("Yes", "No"))
data$combatzone <- factor(data$LH4C, c(1,5), labels=c("Yes", "No"))
data$homeless_lhs <- factor(data$LH4E, c(1,5), labels=c("Yes", "No"))
data$long_term_pysch<- factor(data$LH4F, c(1,5), labels=c("Yes", "No"))
data$naturaldisater<- factor(data$LH4G, c(1,5), labels=c("Yes", "No"))

###Recode LBS POIs into categorical variables
data$prison12 = factor(data$NLB035_B, c(1,5), labels=c("Yes", "No"))
data$prison14 = factor(data$OLB033_B, c(1,5), labels=c("Yes", "No"))
data$homeless12 = factor(data$NLB035_C, c(1,5), labels = c("Yes", "No"))
data$homeless14 = factor(data$OLB033_A, c(1,5), labels = c("Yes", "No"))
data$prisontime12 = factor(data$NLB035_C,  c(1,2,3,4,5), labels=c("lessthan_1month", "1month-1year", "1-5 years", ">5years", "Don't Know"))
data$prisontime14 = factor(data$OLB033_C,  c(1,2,3,4,5), labels=c("lessthan_1month", "1month-1year", "1-5 years", ">5years", "Don't Know"))

###Compare LBS 2012 and 2014 data to see if there are discrepancies 
data$prison12_14 = ifelse(data$prison12 == data$prison14, "Match", "No Match")
nrow((data[which(data$prison12_14 == "No Match"),])) ##0 no matches
data$homeless12_14 = ifelse(data$homeless12 == data$homeless14, "Match", "No Match") 
nrow((data[which(data$homeless12_14 == "No Match"),])) ## 0 no matches

### Compare prison/homelessness from LBS data and LHS to see if there are discrepancies 
data$prisonlbs = ifelse(is.na(data$prison12), data$prison14, data$prison12)###Create variable that contains all LBS
nrow(data[which(data$prison14 == "Yes" & data$prison12 == "No"),]) ##Check if there is a possibility that we could miss a yes

data$homelesslbs = ifelse(is.na(data$homeless12), data$homeless14, data$homeless12) ##Do same for homeless 
nrow(data[which(data$homeless14 == "Yes" & data$homeless12 == "No"),])

data$prison_lbs = factor(data$prisonlbs, c(1,2), labels=c("Yes", "No")) ##change lbs variables into factors
data$homeless_lbs = factor(data$homelesslbs, c(1,2), labels = c("Yes", "No"))
View(data[,c("prison_lbs", "prison_lhs", "prison")])
###Create variable to compare LHS and LBS answers
data$prisonlbslhs = ifelse(data$prison_lbs == data$prison_lhs, "Match", "No Match")
nrow(data[which(data$prisonlbslhs == "No Match"),]) ###224 Cases data does not match 

data$homelesslbslhs = ifelse(data$homeless_lbs == data$homeless_lhs, "Match", "No Match")
nrow(data[which(data$homelesslbslhs == "No Match"),]) ###212 Cases data does not match

###Create variable that measures homelessness and prison across LBS and LHS, that is, if a participant has ever answered Yes on prison, then the answer should be yes
data$prisonv1 = ifelse(data$prison_lbs == "Yes" | data$prison_lhs == "Yes","Yes", "No")
data$prison = ifelse(is.na(data$prisonv1) & (data$prison_lbs == "No" | data$prison_lhs == "No"), "No", data$prisonv1) ### New prison variable (combines LHS and LBS)
nrow(data[which((data$prison_lbs == "Yes" | data$prison_lhs == "Yes") & data$prison == "No"),]) ##Checking to see if prison is correctly coded (should be 0)
nrow(data[which((data$prison_lbs == "No" & data$prison_lhs == "No") & data$prison == "Yes"),])##Checking to see if prison is correctly coded (should be 0)
nrow(data[which((data$prison_lbs == "No"| data$prison_lhs == "No") & is.na(data$prison)),])##Checking to see if prison is correctly coded (should be 0)

data$homev1 = ifelse(data$homeless_lbs == "Yes" | data$homeless_lhs == "Yes", "Yes", "No")
data$homeless = ifelse((data$homeless_lbs == "No" | data$homeless_lhs == "No") & is.na(data$homev1), "No", data$homev1) ##New homelessness variable
nrow(data[which((data$homeless_lhs == "Yes" | data$homeless_lbs == "Yes") & data$homeless == "No"),]) ##Checking Code
nrow(data[which((data$homeless_lhs == "Yes" | data$homeless_lbs == "Yes") & is.na(data$homeless)),]) 
nrow(data[which((data$homeless_lbs == "No" | data$homeless_lhs == "No") & is.na(data$homeless)),])


###Cumulative trauma score for analysis (did not include orphanage and foster, as I felt too similar to proceed)
data$p =  ifelse(data$prison =="No", 0, 1)
data$h = ifelse(data$homeless =="No" , 0, 1)
data$cz = ifelse(data$combatzone =="No", 0, 1)
data$lth = ifelse(data$long_term_hospital == "No", 0, 1)
data$ltp = ifelse(data$long_term_pysch == "No", 0, 1)
data$nd = ifelse(data$naturaldisater =="No", 0, 1)
data$CumlativeScore = rowSums(data[,c("p", "h", "cz", "lth", "ltp", "nd")], na.rm=TRUE)
data$CumlativeScore_red = rowSums(data[,c("p", "h", "cz")], na.rm=TRUE)

###Cleaning variables to stratify analysis by
data$sex = ifelse(data$RAGENDER == 1, "Male", "Female")
data$hispanic = ifelse(data$RAHISPAN == 0, "Not hispanic", "Hispanic")

data <- data %>% mutate(race = case_when(
  RARACEM == 1 & RAHISPAN ==0 ~ "Non hispanic white",
  RARACEM == 2 & RAHISPAN ==0 ~ "Non hispanic black",
  RAHISPAN == 1 ~ "Hispanic",
  RARACEM== 3 & RAHISPAN ==0 ~ "Other",
))

###Recode Outcome Variables
data$IL6 = log( (data$PIL6 + 0.001) )
hist(data$IL6)

data$TNF1 = log( (data$PTNFR1 + .001 ))
hist(data$TNF1)

data$CRP= log( (data$PCRP+.001) )
hist(data$CRP)

data$CMV_sero = ifelse((data$PCMVGINT == 1 | data$PCMVGINT == 3), "Yes", "No") ##Recode seropositivity as yes or no (yes if reactive or boderline reactive)
data$CMV = ifelse((data$PCMVGINT == 1 | data$PCMVGINT == 3), log( (data$PCMVGE+.001) ), 0) ##CMV=0 if non reactive 
hist(data$CMV)

###T Cell % 
data$CD4_total = data$PCD4T_PCT + .001
data$CD8_total = data$PCD8T_PCT + .001
data$CD4N = data$PCD4N_PCT + .001
data$CD8N = data$PCD8N_PCT + .001
data$CD4M = data$PCD4TEMRA_PCT + .001
data$CD8M = data$PCD8TEMRA_PCT + .001

###Creating T Cell Phenotype Outcome Variables 
data$CD4_CD8 = log((data$CD4_total / data$CD8_total)) 
hist(data$CD4_CD8)
data$CD8_CD4 = log( (data$CD8_total / data$CD4_total))

data$CD4M_N = log((data$CD4M / data$CD4N ))
data$CD8M_N = log((data$CD8M / data$CD8N ))
data$CDM_N = log( ( (data$CD4M + data$CD8M)/ (data$CD4N+data$CD8N) ))
hist(data$CDM_N)

###Use functional limitations as proxy for disability (from section M1 and M2 of HRS Core_)
data$yes_limits = ifelse(data$NM002 == 1 | data$NM006 == 1 | data$NM007 == 1 | data$NM008 == 1 | data$OM002==1 | data$OM006==1 | data$OM007==1 | data$OM008==1, "Yes", "No") ##If answers yes to any limitations question, consider yes
data$no_limits = ifelse(is.na(data$NM002) & is.na(data$NM006) & is.na(data$NM007) & is.na(data$NM008) & is.na(data$OM002) & is.na(data$OM006) & is.na(data$OM007) & is.na(data$OM008), NA, "No") ###Recode so anyone who answered gets a no, anyone with no answers gets NA
data$funx_limits = ifelse(is.na(data$yes_limits), data$no_limits, data$yes_limits) ###This variable states whether or not they reported disability<--one we will use 

data$limitation_lifelong = ifelse(data$funx_limits == "Yes" & (data$OM009==9995 | data$NM009==9995), "Yes", "No") ###Determines whether limitation from birth 

###Determining age (before or after 18) of functional limitation if not lifelong 
data$limit_yearv1 = ifelse(data$OM009 < data$NM009, data$OM009, data$NM009)
data$limit_yearv2 = ifelse(is.na(data$limit_yearv1) & is.na(data$OM009), data$NM009, data$limit_yearv1) 
data$limit_yearv3 = ifelse(is.na(data$limit_yearv2) & is.na(data$NM009), data$OM009, data$limit_yearv2)
data$birthyear = ifelse( is.na(data$BIRTHYR.x), data$BIRTHYR.y, data$BIRTHYR.x)
data$limit_yearv4 = ifelse(data$limit_yearv3<3000, data$limit_yearv3-data$birthyear, data$limit_yearv3)
data$limitation_childhood = ifelse(data$limit_yearv4 < 18, "Yes", "No")

###Creating variable that determines life course of functional limitation
data$limit_time = ifelse(data$limitation_lifelong == "Yes" | data$limitation_childhood =="Yes", "Childhood", "Adult")
data$limit_time2 = ifelse(is.na(data$limit_time) & is.na(data$limitation_lifelong) & data$limitation_childhood > 18, "Adult", data$limit_time)
data$limit=ifelse(is.na(data$limit_time2), data$funx_limits, data$limit_time2)

###Yuan Disability Variables (use wave that corresponds to VBS draw)
View(data[,c("R13MOBILA", "R13LGMUSA","R13ADLA", "R13ADLWA", "R13IADLA", "R13IADLZA")]) ###Possible Indicators of disability 
data$funxlimitindex = rowSums(data[,c("R13MOBILA", "R13LGMUSA")], na.rm = TRUE) ###functional limitations 
data$disability = ifelse(data$R13ADLA !=0, 1, 0) ##used 



###Variable Cleaning & Histograms for Effect Modifiers
data$age = data$R13AGEY_E ##Using wave 13 as this corresponds to when VBS data collected
nrow(data[which(is.na(data$R13AGEY_E)),]) ##No missing age data

##Smoking status
data$smoking_status= ifelse(data$R13SMOKEN == 1 | data$R13SMOKEV == 1, "Yes", "No") ##Yes if ever or currently smokes, No if never reported smoking 

###For parent SES, use cumulative of mom and dads years of education 
data$ped1 = ifelse((data$RAFEDUC > data$RAMEDUC), data$RAFEDUC, (ifelse(data$RAFEDUC < data$RAMEDUC, data$RAMEDUC, data$RAFEDUC)))
data$ped2 = ifelse(!is.na(data$ped1), data$ped1, (ifelse(!is.na(data$RAMEDUC), data$RAMEDUC, (ifelse(!is.na(data$RAFEDUC), data$RAFEDUC, NA)))))
data$parents_ed = ifelse(data$ped2 <8, "Less than 8 years", (ifelse(data$ped2<11, "8-11", ifelse(data$ped2<=12, "HS Graduate", "Beyond HS"))))
data <- data %>% mutate(parent_edu = case_when(
  ped2 <8 ~ "<8", 
))
data$parents_ed = rowSums(data[,c("RAMEDUC", "RAFEDUC")], na.rm = TRUE)

###Respondant SES
data <- data %>% mutate(education = case_when(
  RAEDUC == 1  ~ "Less than High School",
  RAEDUC == 3 | RAEDUC ==2 ~ "High School Graduate or GED",
  RAEDUC== 4 ~ "Some College",
  RAEDUC == 5 ~ "College or beyond",
))

##Should we collapse ged and high school graduate? Thinking no?
data$wealth = quantcut(data$H13ATOTA, q=4, na.rm=T) 
###turned into quantile factor variable 

###Recent Health (Adult/current health)
data$R13SHLTC ##change in self reported health
data$R13HLTC ## self report in health change
data$R13CONDE ## chronic condition index
data$R13ADLC ## change in ADL
data$R13CESD ###depression score
data$bmi = (data$R13BMI-mean(data$R13BMI, na.rm = T))/(sd(data$R13BMI, na.rm=T))


###Creating categorical for missing (aids in exploratory)
data$PRISON= ifelse(is.na(data$prison), "Missing", data$prison)
data$UNHOUSED= ifelse(is.na(data$homeless), "Missing", data$homeless)
data$LTHOSP= ifelse(is.na(data$long_term_hospital), "Missing", factor(data$long_term_hospital))
data$LTPSYCH=ifelse(is.na(data$long_term_pysch), "Missing", data$long_term_pysch)
data$COMBAT=ifelse(is.na(data$combatzone), "Missing", data$combatzone)
data$NATDIST=ifelse(is.na(data$naturaldisater), "Missing", data$naturaldisater)
data$HISP = ifelse(is.na(data$hispanic), "Missing", data$hispanic)
data$SEX = ifelse(is.na(data$sex1), "Missing", data$sex1) 
data$RACE = ifelse(is.na(data$race), "Missing", data$race)
data$LIMIT = ifelse(is.na(data$limit), "Missing", data$limit)
data$SMOKE=ifelse(is.na(data$smoking_status), "Missing", data$smoking_status)

###Assessing missing-ness in:
###Outcome: (None, as we selected for full outcome data)
data%>%select(IL6, CRP, TNF1, CMV_sero, CMV, CD4_CD8, CD8_CD4, CD8M_N, CD4M_N, CDM_N)%>%
  tbl_summary()
###Con founders
data %>% select(RACE, sex, LIMIT, limits)%>%
  tbl_summary(type=everything()~"categorical") 
###POI
data %>% select(PRISON, UNHOUSED, LTHOSP, LTPSYCH, COMBAT, NATDIST, cumlative_score) %>%
  tbl_summary(type=everything()~"categorical") %>%
  modify_footnote(all_stat_cols()~"n(%), 1=Yes 2=No")
###Effect Modifiers
data %>% select(R13AGEY_E, SMOKE, RAEDUC, RAEDYRS, parent_ses, R13SHLTC, R13HLTC, R13CONDE, R13BMI, R13CESD) %>%
  tbl_summary()

###Remove data missing con founders variables (race, sex,age,  or limits)
df1 <- data[which(!is.na(data$race) & !is.na(data$sex) & !is.na(data$limits) & !is.na(data$age)),]
nrow(data)-nrow(df1) ###lost 23 participants 

###Remove data missing control variables 
df2 <-df1[which(!is.na(df1$parents_ed) & !is.na(df1$wealth) & !is.na(df1$education) & !is.na(df1$bmi) &!is.na(df1$R13SHLTC) & !is.na(df1$R13HLTC) & !is.na(df1$R13ADLC) & !is.na(df1$R13CONDE) & !is.na(df1$R13CESD)),]
nrow(df1)-nrow(df2) ##Lost 69 total 

df<-df2[!(is.na(df2$prison) & is.na(df2$combatzone) & is.na(df2$homeless)),]
nrow(df2)-nrow(df) ##Lost 20 total 

###Add sample weights
install.packages("srvyr")
library(survey)
library(srvyr)
dfw <- df %>% as_survey_design(weights = PVBSWGTR)

###Save Data 
save(data, file="datafile.Rdata")
save(df, file="final_data_nowgts.Rdata")
save(dfw, file="final_data_wgts.Rdata")
