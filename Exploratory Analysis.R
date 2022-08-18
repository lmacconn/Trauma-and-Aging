library(ggcorrplot)
library(ggplot2)
library(kableExtra)
###Assessing cell sizes for stratification and general statistics 
ss<- df %>% select(race, sex, age) %>%
  group_by(sex, race) %>%
  summarise(Count=n(),
            Mean.Age = mean(age))
##Male/female other are n=65/95
ss2 <- df %>% select(race, limits, age) %>%
  group_by(race, limits) %>%
  summarise(Count=n(), 
            Mean.age=mean(age))
##Reported limits/did not other are n<100
ss3 <- df %>% select(race, limits, sex, age) %>%
  group_by(race, limits,sex) %>%
  summarise(Count=n(), 
            Mean.age=mean(age))
###Could we stratify on three levels (race, sex, and reported limitations)?
###I think it is better to stratify on sex and race, then control for disability 

###Assess correlation of POI variables 
df_c <- df %>% select(p, h, cz, lth, ltp, nd)
model.matrix(~0+., data=df_c) %>% cor(use="pairwise.complete.obs") %>%
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)
###Strongest correlations between prison & homelessness, long-term-hospitalization & long-term-pysch 

###Assessing correlation of limitations and POI
df_c2 <- df %>% select(fl, p, h, cz, lth, ltp, nd) 
model.matrix(~0+., data=df_c2) %>% cor(use="pairwise.complete.obs") %>%
  ggcorrplot(show.diag = F, type="lower", lab=TRUE, lab_size=2)




###Outcomes x Confounders
df %>% select(race, IL6, CRP, TNF1, CMV, CD4_CD8, CD8_CD4, CD8M_N, CD4M_N, CDM_N)%>%
  tbl_summary(by=race)%>%
  add_p()%>%
  bold_p(t=0.05)


df%>%select(sex,IL6, CRP, TNF1, CMV, CD4_CD8, CD8_CD4, CD8M_N, CD4M_N, CDM_N)%>%
  tbl_summary(by=sex)%>%
  add_p() %>%
  bold_p(t=0.05)

df%>%select(limits,IL6, CRP, TNF1, CMV, CD4_CD8, CD8_CD4, CD8M_N, CD4M_N, CDM_N)%>%
  tbl_summary(by=limits)%>%
  add_p() %>%
  bold_p(t=0.05) ###For when Limits is coded to consider lifetime period when reported (or began)

###CMV Re activity by confounder 
df%>%select(sex, CMV_sero) %>%
  tbl_summary(by=sex)%>%


df%>%select(limits, CMV_sero) %>%
  tbl_summary(by=limits)

df%>%select(race, CMV_sero) %>%
  tbl_summary(by=race)

###POI x Confounders
df %>% select(race, prison, homeless, combatzone,  CumlativeScore_red) %>%
  tbl_summary(by=race) %>%
  add_p(test= everything()~"chisq.test")

data %>% select(sex, prison, homeless, combatzone,  CumlativeScore_red) %>%
  tbl_summary(by=sex) %>%
  add_p(test= everything()~"chisq.test")

df %>% select(limits, prison, homeless, combatzone,  CumlativeScore_red) %>%
  tbl_summary(by=limits)%>%
  add_p(test= everything()~"chisq.test") 

###Assessing continuous variables and correlation (need to work on )
library(corrplot)
scatmatrixData = data[,c("IL6", "CRP", "TNF1", "CMV", "CD4_CD8", "CD8_CD4", "CD8M_N", "CD4M_N", "CDM_N", "R13AGEY_E", "RAEDYRS", "parent_ses", "R13BMI", "R13SHLTC", "R13HLTC", "R13CONDE", "R13CESD")]
panel.hist <- function(x, ...)
  {
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col = "cyan", ...)
}
pairs(scatmatrixData, pch = 19, diag.panel=panel.hist)
cormat = cor(scatmatrixData)
pres <- cor.mtest(scatmatrixData, conf.level = .95)
corrplot.mixed(cormat, lower.col = "black", number.cex = 1,p.mat = pres$p, sig.level = .05)

  



