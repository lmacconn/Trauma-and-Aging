library(ggcorrplot)
library(ggplot2)
library(kableExtra)
###Assess correlation of POI variables 
df_c <- df %>% select(p, h, cz, lth, ltp, nd)
model.matrix(~0+., data=df_c) %>% cor(use="pairwise.complete.obs") %>%
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)
###Strongest correlations between prison & homelessness, long-term-hospitalization & long-term-pysch 

###Assessing correlation of disability and POI
df_c2 <- df %>% select(disability, p, h, cz, lth, ltp, nd) 
model.matrix(~0+., data=df_c2) %>% cor(use="pairwise.complete.obs") %>%
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2) ##Correlation matrix

###Descriptive table of disability X outcomes & exposures
###With Weights
dfw %>% select(disability, logIL6, logCRP, CMV,logCD4_CD8, logCD8_CD4, logCD8M_N, logCD4M_N, logCDM_N, prison, homeless, combatzone)%>%
  tbl_svysummary(by=disability, 
                 type = c(CMV~"categorical", disability ~ "categorical", prison ~ "categorical", homeless~ "categorical", combatzone ~ "categorical"),
                 statistic = list(all_continuous() ~ "{mean} ({sd})",
                                  all_categorical() ~ "{p}%")
  ) %>%
  add_n() %>%
  add_p()
###No weights 
data %>% select(disability, logIL6, logCRP, CMV,logCD4_CD8, logCD8_CD4, logCD8M_N, logCD4M_N, logCDM_N, prison, homeless, combatzone)%>%
  tbl_summary(by=disability, 
                 type = c(CMV~"categorical", disability ~ "categorical", prison ~ "categorical", homeless~ "categorical", combatzone ~ "categorical"),
                 statistic = list(all_continuous() ~ "{mean} ({sd})",
                                  all_categorical() ~ "{p}%")
  ) %>%
  add_n() %>%
  add_p() %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Reported impairment of ADL**")
###Which to use? Why is the N for the survey design data so large?? (For now moving forward with descriptive stats not including svy weights)
###Disability seems to impact (we know this)

##Descriptive table sex X outcomes & exposure
data %>% select(sex, logIL6, logCRP, CMV,logCD4_CD8, logCD8_CD4, logCD8M_N, logCD4M_N, logCDM_N, prison, homeless, combatzone)%>%
  tbl_summary(by=sex, 
              type = c(CMV~"categorical", sex ~ "categorical", prison ~ "categorical", homeless~ "categorical", combatzone ~ "categorical"),
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{p}%")
  ) %>%
  add_n() %>%
  add_p() %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Reported Sex**")
###Sex differences exist among exposure, with females experiencing the traumatic events at a lower rate. 

##Descriptive table race X outcomes & exposure
data %>% select(race, logIL6, logCRP, CMV,logCD4_CD8, logCD8_CD4, logCD8M_N, logCD4M_N, logCDM_N, prison, homeless, combatzone)%>%
  tbl_summary(by=race, 
              type = c(CMV~"categorical", race ~ "categorical", prison ~ "categorical", homeless~ "categorical", combatzone ~ "categorical"),
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{p}% ({n})")
  ) %>%
  add_n() %>%
  add_p() %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Race/Ethnicity**")
###Different race/ethinicites (black hispanic, and other) experience higher rates of incareration and homelessness
#Interestingly, other also has highest rates of experiencing combat zones, with white next, and then black/hispanic 
#Data agrees with well documented racial inequites of CMV infections


###Summary of full population (without removing missing con founders/controls/exposures)
data %>% 
  select(logIL6, logCRP, logTNF1, CMV, logCMVS, logCD4_CD8, logCD8_CD4, logCD8M_N, logCD4M_N, logCDM_N, age, sex, race, disability, prison, homeless, combatzone, parents_ed, education, wealth, R13BMI, HLTC, SHLTC, CCI, CESD, smoking_status) %>%
  tbl_summary(
    type = c(CMV~"categorical", disability ~ "categorical", prison ~ "categorical", homeless~ "categorical", combatzone ~ "categorical", smoking_status~ "categorical", HLTC ~ "continuous", SHLTC~"continuous", CCI~ "continuous", CESD~ "continuous"), 
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p}%")
  ) %>%
  add_n()


###Full summary by prison (25 missing data removed)
data %>% 
  select(logIL6, logCRP, logTNF1, CMV, logCMVS, logCD4_CD8, logCD8_CD4, logCD8M_N, logCD4M_N, logCDM_N, age, sex, race, disability, prison, homeless, combatzone, parents_ed, education, wealth, R13BMI, HLTC, SHLTC, CCI, CESD, smoking_status) %>%
  tbl_summary(by=prison,
    type = c(CMV~"categorical", disability ~ "categorical", prison ~ "categorical", homeless~ "categorical", combatzone ~ "categorical", smoking_status~ "categorical", HLTC ~ "continuous", SHLTC~"continuous", CCI~ "continuous", CESD~ "continuous"), 
    statistic = list(all_continuous() ~ "{mean} ({sd})",
                     all_categorical() ~ "{p}%")
  ) %>%
  add_n()%>%
  add_p(test=list(all_continuous()~"t.test")) %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Experienced Incareration**")


###Full summary by homeless (404 missing data removed)
data %>% 
  select(logIL6, logCRP, logTNF1, CMV, logCMVS, logCD4_CD8, logCD8_CD4, logCD8M_N, logCD4M_N, logCDM_N, age, sex, race, disability, prison, homeless, combatzone, parents_ed, education, wealth, R13BMI, HLTC, SHLTC, CCI, CESD, smoking_status) %>%
  tbl_summary(by=homeless,
              type = c(CMV~"categorical", disability ~ "categorical", prison ~ "categorical", homeless~ "categorical", combatzone ~ "categorical", smoking_status~ "categorical", HLTC ~ "continuous", SHLTC~"continuous", CCI~ "continuous", CESD~ "continuous"), 
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{p}%")
  ) %>%
  add_n()%>%
  add_p(test=list(all_continuous()~"t.test")) %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Experienced Homelessness**")

###Full summary by Combat zone (612 missing data removed)
data %>% 
  select(logIL6, logCRP, logTNF1, CMV, logCMVS, logCD4_CD8, logCD8_CD4, logCD8M_N, logCD4M_N, logCDM_N, age, sex, race, disability, prison, homeless, combatzone, parents_ed, education, wealth, R13BMI, HLTC, SHLTC, CCI, CESD, smoking_status) %>%
  tbl_summary(by=combatzone,
              type = c(CMV~"categorical", disability ~ "categorical", prison ~ "categorical", homeless~ "categorical", combatzone ~ "categorical", smoking_status~ "categorical", HLTC ~ "continuous", SHLTC~"continuous", CCI~ "continuous", CESD~ "continuous"), 
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{p}%")
  ) %>%
  add_n()%>%
  add_p(test=list(all_continuous()~"t.test")) %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Experienced a Combat Zone**")



###Supplemental Tables
#Natural Disater X variables (missing 4104 was not collected in 2015 LHMS)
data %>% 
  select(logIL6, logCRP, logTNF1, CMV, logCMVS, logCD4_CD8, logCD8_CD4, logCD8M_N, logCD4M_N, logCDM_N, age, sex, race, disability, prison, homeless, combatzone, long_term_hospital, long_term_pysch, naturaldisater, parents_ed, education, wealth, R13BMI, HLTC, SHLTC, CCI, CESD, smoking_status) %>%
  tbl_summary(by=naturaldisater,
              type = c(CMV~"categorical", disability ~ "categorical", prison ~ "categorical", homeless~ "categorical", combatzone ~ "categorical", smoking_status~ "categorical", HLTC ~ "continuous", SHLTC~"continuous", CCI~ "continuous", CESD~ "continuous"), 
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{p}%")
  ) %>%
  add_n()%>%
  add_p(test=list(all_continuous()~"t.test")) %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Experienced a Natural Disater**")
#Nothing really jumps out here to me...possibly due to the data collected? does not encompass whehter experienced personal loss (ie property destruction, death, etc)

#Long term hospital X variables (missing 4104 was not collected in 2015 LHMS, 599 missing)
data %>% 
  select(logIL6, logCRP, logTNF1, CMV, logCMVS, logCD4_CD8, logCD8_CD4, logCD8M_N, logCD4M_N, logCDM_N, age, sex, race, disability, prison, homeless, combatzone, long_term_hospital, long_term_pysch, naturaldisater, parents_ed, education, wealth, R13BMI, HLTC, SHLTC, CCI, CESD, smoking_status) %>%
  tbl_summary(by=long_term_hospital,
              type = c(CMV~"categorical", disability ~ "categorical", prison ~ "categorical", homeless~ "categorical", combatzone ~ "categorical", smoking_status~ "categorical", HLTC ~ "continuous", SHLTC~"continuous", CCI~ "continuous", CESD~ "continuous"), 
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{p}%")
  ) %>%
  add_n()%>%
  add_p(test=list(all_continuous()~"t.test")) %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Experienced a Hospitilization for Physical condition or illness >1month**")
#Expected trends with long term hospitalization history and more advanced immune aging profiles 

#Long term pysch related stay X variables (missing 4104 was not collected in 2015 LHMS, 599 missing)
data %>% 
  select(logIL6, logCRP, logTNF1, CMV, logCMVS, logCD4_CD8, logCD8_CD4, logCD8M_N, logCD4M_N, logCDM_N, age, sex, race, disability, prison, homeless, combatzone, long_term_hospital, long_term_pysch, naturaldisater, parents_ed, education, wealth, R13BMI, HLTC, SHLTC, CCI, CESD, smoking_status) %>%
  tbl_summary(by=long_term_pysch,
              type = c(CMV~"categorical", disability ~ "categorical", prison ~ "categorical", homeless~ "categorical", combatzone ~ "categorical", smoking_status~ "categorical", HLTC ~ "continuous", SHLTC~"continuous", CCI~ "continuous", CESD~ "continuous"), 
              statistic = list(all_continuous() ~ "{mean} ({sd})",
                               all_categorical() ~ "{p}%")
  ) %>%
  add_n()%>%
  add_p(test=list(all_continuous()~"t.test")) %>%
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Experienced a Hospitilization for Psychiartic condition >1month**")
#Those who have not epxerienced care seeminly have worse off t cell profiles?











