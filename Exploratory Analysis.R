library(ggcorrplot)
library(ggplot2)
library(kableExtra)
###Assessing cell sizes for stratification and general statistics 
data_w %>% select(race, sex, R13AGEY_E, IL6, CRP,  )
###Assess correlation of POI variables 
data_c <- data_w %>% select(p, h, c, lth, ltp, nd)
model.matrix(~0+., data=data_c) %>% cor(use="pairwise.complete.obs") %>%
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)
###Seemingly a strong correlation between prison/homelessness, long-term-hospitalization and long-term-pysch 


###Assessing missing-ness in:
###Outcome: (None, as we selected for full outcome data)
data%>%select(IL6, CRP, TNF1, CMV_sero, CMV, CD4_CD8, CD8_CD4, CD8M_N, CD4M_N, CDM_N)%>%
  tbl_summary()
###Confounders
data %>% select(RACE, sex, LIMIT)%>%
  tbl_summary(type=everything()~"categorical") 
###POI
data %>% select(PRISON, UNHOUSED, LTHOSP, LTPSYCH, COMBAT, NATDIST) %>%
  tbl_summary(type=everything()~"categorical") %>%
  modify_footnote(all_stat_cols()~"n(%), 1=Yes 2=No")
###Effect Modifiers
data %>% select(R13AGEY_E, SMOKE, RAEDUC, parent_ses, R13SHLTC, R13HLTC, R13BMI, R13CESD) %>%
  tbl_summary()

###POI by gender
gxPOI <- data %>% select(prison, homeless, combatzone, long_term_hospital, long_term_pysch, naturaldisater, sex) %>%
  tbl_summary(by="sex", type=everything()~"categorical") %>% 
  add_p()
gxPOI

###POI by limitations (disability)
dxPOI <- data %>% select(prison, homeless, combatzone, long_term_hospital, long_term_pysch, naturaldisater, limitations) %>%
  tbl_summary(by="limitations", type=everything()~"categorical") %>% 
  add_p()
dxPOI

###Outcome data RaceXPOI

xtabs(IL6~prison+race, data = data)
xtabs(CRP~prison+race, data = data)
