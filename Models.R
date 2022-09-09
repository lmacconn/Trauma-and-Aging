###Create subset of data for stratified analysis 
df_black <- subset(dfw, race == "Non hispanic black")
df_women <- subset(dfw, sex == "Female")
df_men <- subset(dfw, sex == "Male")
df_white <- subset(dfw, race == "Non hispanic white")
df_hispanic <- subset(dfw, race == "Hispanic")
df_other <- subset(dfw, race == "Other")

##packages needed 
install.packages("jtools")
library(jtools)
library(survey)
install.packages("sjPlot")
library(sjPlot)
install.packages("sjmisc")
library(sjmisc)
library(sjlabelled)

##Model 1: Adjusting only for age and gender, stratify on race (would stratify on sex, however, cell sizes are too small)
m1pf = svyglm(logIL6~prison+age+sex, data=df, design=dfw)
m1pw= svyglm(logIL6~prison+age+sex,data=df, design = df_white)
m1pb = svyglm(logIL6~prison+age+sex,data=df, design = df_black)
m1ph =svyglm(logIL6~prison+age+sex,data=df, design = df_hispanic)
m1po =svyglm(logIL6~prison+age+sex,data=df, design = df_other)

m1hf = svyglm(logIL6~homeless+age+sex, data=df, design=dfw)
m1hw= svyglm(logIL6~homeless+age+sex,data=df, design = df_white)
m1hb = svyglm(logIL6~homeless+age+sex,data=df, design = df_black)
m1hh =svyglm(logIL6~homeless+age+sex,data=df, design = df_hispanic)
m1ho =svyglm(logIL6~homeless+age+sex,data=df, design = df_other)

m1cf = svyglm(logIL6~combatzone+age+sex, data=df, design=dfw)
m1cw= svyglm(logIL6~combatzone+age+sex,data=df, design = df_white)
m1cb = svyglm(logIL6~combatzone+age+sex,data=df, design = df_black)
m1ch =svyglm(logIL6~combatzone+age+sex,data=df, design = df_hispanic)
m1co =svyglm(logIL6~combatzone+age+sex,data=df, design = df_other)

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
##ran up to here 

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

m1T1pf = svyglm(logCD4_CD8~prison+age+sex, data=df, design=dfw)
m1T1pw= svyglm(logCD4_CD8~prison+age+sex,data=df, design = df_white)
m1T1pb = svyglm(logCD4_CD8~prison+age+sex,data=df, design = df_black)
m1T1ph =svyglm(logCD4_CD8~prison+age+sex,data=df, design = df_hispanic)
m1T1po =svyglm(logCD4_CD8~prison+age+sex,data=df, design = df_other)

m1T1hf = svyglm(logCD4_CD8~homeless+age+sex, data=df, design=dfw)
m1T1hw= svyglm(logCD4_CD8~homeless+age+sex,data=df, design = df_white)
m1T1hb = svyglm(logCD4_CD8~homeless+age+sex,data=df, design = df_black)
m1T1hh =svyglm(logCD4_CD8~homeless+age+sex,data=df, design = df_hispanic)
m1T1ho =svyglm(logCD4_CD8~homeless+age+sex,data=df, design = df_other)

m1T1cf = svyglm(logCD4_CD8~combatzone+age+sex, data=df, design=dfw)
m1T1cw= svyglm(logCD4_CD8~combatzone+age+sex,data=df, design = df_white)
m1T1cb = svyglm(logCD4_CD8~combatzone+age+sex,data=df, design = df_black)
m1T1ch =svyglm(logCD4_CD8~combatzone+age+sex,data=df, design = df_hispanic)
m1T1co =svyglm(logCD4_CD8~combatzone+age+sex,data=df, design = df_other)

m1T2pf = svyglm(logCD4M_N~prison+age+sex, data=df, design=dfw)
m1T2pw= svyglm(logCD4M_N~prison+age+sex,data=df, design = df_white)
m1T2pb = svyglm(logCD4M_N~prison+age+sex,data=df, design = df_black)
m1T2ph =svyglm(logCD4M_N~prison+age+sex,data=df, design = df_hispanic)
m1T2po =svyglm(logCD4M_N~prison+age+sex,data=df, design = df_other)

m1T2hf = svyglm(logCD4M_N~homeless+age+sex, data=df, design=dfw)
m1T2hw= svyglm(logCD4M_N~homeless+age+sex,data=df, design = df_white)
m1T2hb = svyglm(logCD4M_N~homeless+age+sex,data=df, design = df_black)
m1T2hh =svyglm(logCD4M_N~homeless+age+sex,data=df, design = df_hispanic)
m1T2ho =svyglm(logCD4M_N~homeless+age+sex,data=df, design = df_other)

m1T2cf = svyglm(logCD4M_N~combatzone+age+sex, data=df, design=dfw)
m1T2cw= svyglm(logCD4M_N~combatzone+age+sex,data=df, design = df_white)
m1T2cb = svyglm(logCD4M_N~combatzone+age+sex,data=df, design = df_black)
m1T2ch =svyglm(logCD4M_N~combatzone+age+sex,data=df, design = df_hispanic)
m1T2co =svyglm(logCD4M_N~combatzone+age+sex,data=df, design = df_other)

m1T3pf = svyglm(logCD8M_N~prison+age+sex, data=df, design=dfw)
m1T3pw= svyglm(logCD8M_N~prison+age+sex,data=df, design = df_white)
m1T3pb = svyglm(logCD8M_N~prison+age+sex,data=df, design = df_black)
m1T3ph =svyglm(logCD8M_N~prison+age+sex,data=df, design = df_hispanic)
m1T3po =svyglm(logCD8M_N~prison+age+sex,data=df, design = df_other)

m1T3hf = svyglm(logCD8M_N~homeless+age+sex, data=df, design=dfw)
m1T3hw= svyglm(logCD8M_N~homeless+age+sex,data=df, design = df_white)
m1T3hb = svyglm(logCD8M_N~homeless+age+sex,data=df, design = df_black)
m1T3hh =svyglm(logCD8M_N~homeless+age+sex,data=df, design = df_hispanic)
m1T3ho =svyglm(logCD8M_N~homeless+age+sex,data=df, design = df_other)

m1T3cf = svyglm(logCD8M_N~combatzone+age+sex, data=df, design=dfw)
m1T3cw= svyglm(logCD8M_N~combatzone+age+sex,data=df, design = df_white)
m1T3cb = svyglm(logCD8M_N~combatzone+age+sex,data=df, design = df_black)
m1T3ch =svyglm(logCD8M_N~combatzone+age+sex,data=df, design = df_hispanic)
m1T3co =svyglm(logCD8M_N~combatzone+age+sex,data=df, design = df_other)

m1T4pf = svyglm(logCDM_N~prison+age+sex, data=df, design=dfw)
m1T4pw= svyglm(logCDM_N~prison+age+sex,data=df, design = df_white)
m1T4pb = svyglm(logCDM_N~prison+age+sex,data=df, design = df_black)
m1T4ph =svyglm(logCDM_N~prison+age+sex,data=df, design = df_hispanic)
m1T4po =svyglm(logCDM_N~prison+age+sex,data=df, design = df_other)

m1T4hf = svyglm(logCDM_N~homeless+age+sex, data=df, design=dfw)
m1T4hw= svyglm(logCDM_N~homeless+age+sex,data=df, design = df_white)
m1T4hb = svyglm(logCDM_N~homeless+age+sex,data=df, design = df_black)
m1T4hh =svyglm(logCDM_N~homeless+age+sex,data=df, design = df_hispanic)
m1T4ho =svyglm(logCDM_N~homeless+age+sex,data=df, design = df_other)

m1T4cf = svyglm(logCDM_N~combatzone+age+sex, data=df, design=dfw)
m1T4cw= svyglm(logCDM_N~combatzone+age+sex,data=df, design = df_white)
m1T4cb = svyglm(logCDM_N~combatzone+age+sex,data=df, design = df_black)
m1T4ch =svyglm(logCDM_N~combatzone+age+sex,data=df, design = df_hispanic)
m1T4co =svyglm(logCDM_N~combatzone+age+sex,data=df, design = df_other)

               




###Attempt at writing functions
regression <- function(Y, X, A){
  svyglm(paste(Y, "~", X, "+", A), design=dfw, data=df)
}
regression()