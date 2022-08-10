library(ggcorrplot)
library(ggplot2)
library(kableExtra)
###Assessing cell sizes for stratification and general statistics 
summarystat <- data %>% select(RACE, sex, R13AGEY_E, IL6, CRP, TNF1, CMV_sero, CMV, CD4_CD8, CD8_CD4, CD8M_N, CD4M_N, CDM_N) %>%
  group_by(sex, RACE) %>%
  summarise(Count=n(),
            Mean.Age = mean(R13AGEY_E))
summarystat

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

###Outcomes x Confounders
data%>%select(RACE,IL6, CRP, TNF1, CMV, CD4_CD8, CD8_CD4, CD8M_N, CD4M_N, CDM_N)%>%
  mutate(RACE=factor(RACE, levels = c("White", "Black", "Hispanic Other", "Not hispanic Other", "Missing")))%>%
  tbl_summary(by=RACE)%>%
  add_p() %>%
  bold_p(t=0.05)

data%>%select(sex,IL6, CRP, TNF1, CMV, CD4_CD8, CD8_CD4, CD8M_N, CD4M_N, CDM_N)%>%
  tbl_summary(by=sex)%>%
  add_p() %>%
  bold_p(t=0.05)

data%>%select(LIMIT,IL6, CRP, TNF1, CMV, CD4_CD8, CD8_CD4, CD8M_N, CD4M_N, CDM_N)%>%
  mutate(LIMIT=factor(LIMIT, levels=c("Yes", "Adult", "Childhood", "No", "Missing")))%>%
  tbl_summary(by=LIMIT)%>%
  add_p() %>%
  bold_p(t=0.05)

###CMV Re activity by confounder 
data%>%select(sex, CMV_sero) %>%
  tbl_summary(by=sex)%>%
data%>%select(RACE, CMV_sero) %>%
  tbl_summary(by=RACE)
data%>%select(LIMIT, CMV_sero) %>%
  tbl_summary(by=LIMIT)




