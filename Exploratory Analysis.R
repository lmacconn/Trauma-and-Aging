library(ggcorrplot)
library(ggplot2)
library(kableExtra)
###Assess correlation of POI variables 
data_c <- data_w %>% select(p, h, c, lth, ltp, nd)
model.matrix(~0+., data=data_c) %>% cor(use="pairwise.complete.obs") %>%
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)
###Seemingly a strong correlation between prison/homelessness, long-term-hospitalization and long-term-pysch 

## POI variables by race
rxPOI <- data %>% select(prison, homeless, combatzone, long_term_hospital, long_term_pysch, naturaldisater, race) %>%
  tbl_summary(by="race", type=everything()~"categorical") %>% 
  add_p()
rxPOI

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

###Mean outcomes by 
rxPOI <- data %>% group_by(race, prison) %>% 
  summarise(Count=n(), 
            Mean.IL6 = round(mean(IL6), digits = 2),
            Mean.TNF1 = round(mean(TNF1), digits=2), 
            Mean.CRP = round(mean(CRP), digits=2),
            Mean.CMV = round(mean(CMV), digits=2), 
  )
