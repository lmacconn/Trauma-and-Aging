###Create subset of data for stratified analysis 

library(srvyr)
library(survey)
df = read.csv("df.csv")
dfw <- df %>% as_survey_design(weights = PVBSWGTR)
df_black <- subset(dfw, race == "Non hispanic black")
df_women <- subset(dfw, sex == "Female")
df_men <- subset(dfw, sex == "Male")
df_white <- subset(dfw, race == "Non hispanic white")
df_hispanic <- subset(dfw, race == "Hispanic")
df_other <- subset(dfw, race == "Other")

##packages needed 
install.packages("jtools")
install.packages("jstable")
library(jtools)
library(survey)
library(jstable)
install.packages("modelsummary")
library(modelsummary)
library(broom)
library(dplyr)


##Model 1: Adjusting only for age and gender, stratify on race (would stratify on sex, however, cell sizes are too small)
m1pf = svyglm(logIL6~prison+age+sex, data=df, design=dfw)
x = as.data.frame(summary(m1pf))
m1pw= svyglm(logIL6~prison+age+sex,data=df, design = df_white)
tbl_regression(m1pw, exponentiate = TRUE)
m1pb = svyglm(logIL6~prison+age+sex,data=df, design = df_black)
tbl_regression(m1pb, exponentiate = TRUE)
m1ph =svyglm(logIL6~prison+age+sex,data=df, design = df_hispanic)
tbl_regression(m1ph, exponentiate = TRUE)
m1po =svyglm(logIL6~prison+age+sex,data=df, design = df_other)
tbl_regression(m1po, exponentiate = TRUE)


m1hf = svyglm(logIL6~homeless+age+sex, data=df, design=dfw)
tbl_regression(m1hf, exponentiate = TRUE)
m1hw= svyglm(logIL6~homeless+age+sex,data=df, design = df_white)
tbl_regression(m1hw, exponentiate = TRUE)
m1hb = svyglm(logIL6~homeless+age+sex,data=df, design = df_black)
tbl_regression(m1hb, exponentiate = TRUE)
m1hh =svyglm(logIL6~homeless+age+sex,data=df, design = df_hispanic)
tbl_regression(m1hh, exponentiate = TRUE)
m1ho =svyglm(logIL6~homeless+age+sex,data=df, design = df_other)
tbl_regression(m1ho, exponentiate = TRUE)
library(gtsummary)
m1cf = svyglm(logIL6~combatzone+age+sex, data=df, design=dfw)
tbl_regression(m1cf, exponentiate = TRUE)
m1cw= svyglm(logIL6~combatzone+age+sex,data=df, design = df_white)
tbl_regression(m1cw, exponentiate = TRUE)
m1cb = svyglm(logIL6~combatzone+age+sex,data=df, design = df_black)
tbl_regression(m1cb, exponentiate = TRUE)
m1ch =svyglm(logIL6~combatzone+age+sex,data=df, design = df_hispanic)
tbl_regression(m1ch, exponentiate = TRUE)
m1co =svyglm(logIL6~combatzone+age+sex,data=df, design = df_other)
p = tbl_regression(m1co, exponentiate = TRUE)

###alternate approach, get model data into data.frame
model1 <- summary(m1pw)$coefficients[,1] %>% as.data.frame
w = tbl_merge(tbls=list(y,z,p))

###Trying to write functions to get model data in clean tables 
models <- list(
  "Full" = list(
    m1pf,
    m1hf,
    m1cf),
  "Non Hispanic White" = list(
    m1pw, 
    m1hw,
    m1cw),
  "Non Hispanic Black" = list(
    m1pb,
    m1hb, 
    m1cb),
  "Hispanic" = list(
    m1ph, 
    m1hh,
    m1ch),
  "Other" = list(
    m1po,
    m1ho, 
    m1co))
tidy_model <- function(model_list){
  tidy_full <- broom::tidy(model_list[[1]])
  tidy_white <- broom::tidy(model_list[[2]])
  tidy_black <- broom::tidy(model_list[[3]])
  tidy_hispanic <- broom::tidy(model_list[[4]])
  tidy_other <- broom::tidy(model_list[[5]])
  tidy_full$group <- "Full"
  tidy_white$group <- "White"
  tidy_black$group <- "Black"
  tidy_hispanic$group <- "Hispanic"
  tidy_other$group <- "Other"
  ti <- bind_rows(tidy_full, tidy_white, tidy_black, tidy_hispanic, tidy_other)
  gl <- data.frame("N"=stats::nobs(model_list[[1]]))
  out <- list(tidy=ti, glance=gl)
  class(out)
  return(out)
}
models <- lapply(models, tidy_model)


m1.2pf = svyglm(logTNF1~prison+age+sex, data=df, design=dfw)
m1.2pw= svyglm(logTNF1~prison+age+sex,data=df, design = df_white)
m1.2pb = svyglm(logTNF1~prison+age+sex,data=df, design = df_black)
m1.2ph =svyglm(logTNF1~prison+age+sex,data=df, design = df_hispanic)
m1.2po =svyglm(logTNF1~prison+age+sex,data=df, design = df_other)

m1.2hf = svyglm(logTNF1~homeless+age+sex, data=df, design=dfw)
m1.2hw= svyglm(logTNF1~homeless+age+sex,data=df, design = df_white)
m1.2hb = svyglm(logTNF1~homeless+age+sex,data=df, design = df_black)
m1.2hh =svyglm(logTNF1~homeless+age+sex,data=df, design = df_hispanic)
m1.2ho =svyglm(logTNF1~homeless+age+sex,data=df, design = df_other)

m1.2cf = svyglm(logTNF1~combatzone+age+sex, data=df, design=dfw)
m1.2cw= svyglm(logTNF1~combatzone+age+sex,data=df, design = df_white)
m1.2cb = svyglm(logTNF1~combatzone+age+sex,data=df, design = df_black)
m1.2ch =svyglm(logTNF1~combatzone+age+sex,data=df, design = df_hispanic)
m1.2co =svyglm(logTNF1~combatzone+age+sex,data=df, design = df_other)


m1.3pf = svyglm(logCRP~prison+age+sex, data=df, design=dfw)
m1.3pw= svyglm(logCRP~prison+age+sex,data=df, design = df_white)
m1.3pb = svyglm(logCRP~prison+age+sex,data=df, design = df_black)
m1.3ph =svyglm(logCRP~prison+age+sex,data=df, design = df_hispanic)
m1.3po =svyglm(logCRP~prison+age+sex,data=df, design = df_other)

m1.3hf = svyglm(logCRP~homeless+age+sex, data=df, design=dfw)
m1.3hw= svyglm(logCRP~homeless+age+sex,data=df, design = df_white)
m1.3hb = svyglm(logCRP~homeless+age+sex,data=df, design = df_black)
m1.3hh =svyglm(logCRP~homeless+age+sex,data=df, design = df_hispanic)
m1.3ho =svyglm(logCRP~homeless+age+sex,data=df, design = df_other)

m1.3cf = svyglm(logCRP~combatzone+age+sex, data=df, design=dfw)
m1.3cw= svyglm(logCRP~combatzone+age+sex,data=df, design = df_white)
m1.3cb = svyglm(logCRP~combatzone+age+sex,data=df, design = df_black)
m1.3ch =svyglm(logCRP~combatzone+age+sex,data=df, design = df_hispanic)
m1.3co =svyglm(logCRP~combatzone+age+sex,data=df, design = df_other)

m1.4pf = svyglm(logCMVS~prison+age+sex, data=df, design=dfw)
m1.4pw= svyglm(logCMVS~prison+age+sex,data=df, design = df_white)
m1.4pb = svyglm(logCMVS~prison+age+sex,data=df, design = df_black)
m1.4ph =svyglm(logCMVS~prison+age+sex,data=df, design = df_hispanic)
m1.4po =svyglm(logCMVS~prison+age+sex,data=df, design = df_other)

m1.4hf = svyglm(logCMVS~homeless+age+sex, data=df, design=dfw)
m1.4hw= svyglm(logCMVS~homeless+age+sex,data=df, design = df_white)
m1.4hb = svyglm(logCMVS~homeless+age+sex,data=df, design = df_black)
m1.4hh =svyglm(logCMVS~homeless+age+sex,data=df, design = df_hispanic)
m1.4ho =svyglm(logCMVS~homeless+age+sex,data=df, design = df_other)

m1.4cf = svyglm(logCMVS~combatzone+age+sex, data=df, design=dfw)
m1.4cw= svyglm(logCMVS~combatzone+age+sex,data=df, design = df_white)
m1.4cb = svyglm(logCMVS~combatzone+age+sex,data=df, design = df_black)
m1.4ch =svyglm(logCMVS~combatzone+age+sex,data=df, design = df_hispanic)
m1.4co =svyglm(logCMVS~combatzone+age+sex,data=df, design = df_other)

model1 = data.frame(Data = 0, Outcome,  )


