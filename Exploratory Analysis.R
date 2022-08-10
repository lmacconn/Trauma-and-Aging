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

data%>%select(prison,h, sex) %>%
  dplyr::mutate(prison=factor(prison)) %>% 
  forcats::fct_explicit_na()%>%
  tbl_summary(type=everything()~"categorical")


data %>% select(PRISON, UNHOUSED, LTHOSP, LTPSYCH, COMBAT, NATDIST) %>%
  tbl_summary(type=everything()~"categorical") 

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
