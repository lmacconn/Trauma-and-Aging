library(srvyr)
library(survey)
library(modelsummary)
library(broom)
library(dplyr)
library(jtools)
library(survey)
library(jstable)
library(gtsummary)
setwd("~/Desktop/Data Files")
df = read.csv("df.csv")
dfw <- df %>% as_survey_design(weights = PVBSWGTR)
df_black <- subset(dfw, race == "Non hispanic black")
df_women <- subset(dfw, sex == "Female")
df_men <- subset(dfw, sex == "Male")
df_white <- subset(dfw, race == "Non hispanic white")
df_hispanic <- subset(dfw, race == "Hispanic")
df_other <- subset(dfw, race == "Other")

###Model 1: CD8:CD4
p_full= tbl_regression(svyglm(logCD8_CD4~prison+age+sex, data=df, design=dfw), exponentiate=FALSE)
p_white= tbl_regression(svyglm(logCD8_CD4~prison+age+sex,data=df, design = df_white), exponentiate=FALSE)
p_black = tbl_regression(svyglm(logCD8_CD4~prison+age+sex,data=df, design = df_black), exponentiate=FALSE)
p_hispanic =tbl_regression(svyglm(logCD8_CD4~prison+age+sex,data=df, design = df_hispanic), exponentiate=FALSE)
p_other =tbl_regression(svyglm(logCD8_CD4~prison+age+sex,data=df, design = df_other), exponentiate=FALSE)

prison_CD8_CD4 = tbl_merge(tbls=list(p_full, p_white, p_black, p_hispanic, p_other), tab_spanner = c("**All Participants**", "**Non Hispanic White**", "**Non-Hispanic Black**", "**Hispanic**", "**Other**"))

h_full= tbl_regression(svyglm(logCD8_CD4~homeless+age+sex, data=df, design=dfw), exponentiate=FALSE)
h_white= tbl_regression(svyglm(logCD8_CD4~homeless+age+sex,data=df, design = df_white), exponentiate=FALSE)
h_black = tbl_regression(svyglm(logCD8_CD4~homeless+age+sex,data=df, design = df_black), exponentiate=FALSE)
h_hispanic =tbl_regression(svyglm(logCD8_CD4~homeless+age+sex,data=df, design = df_hispanic), exponentiate=FALSE)
h_other =tbl_regression(svyglm(logCD8_CD4~homeless+age+sex,data=df, design = df_other), exponentiate=FALSE)
homeless_CD8_CD4 = tbl_merge(tbls=list(h_full, h_white, h_black, h_hispanic, h_other), tab_spanner = c("**All Participants**", "**Non Hispanic White**", "**Non-Hispanic Black**", "**Hispanic**", "**Other**"))

c_full= tbl_regression(svyglm(logCD8_CD4~combatzone+age+sex, data=df, design=dfw), exponentiate=FALSE)
c_white= tbl_regression(svyglm(logCD8_CD4~combatzone+age+sex,data=df, design = df_white), exponentiate=FALSE)
c_black = tbl_regression(svyglm(logCD8_CD4~combatzone+age+sex,data=df, design = df_black), exponentiate=FALSE)
c_hispanic =tbl_regression(svyglm(logCD8_CD4~combatzone+age+sex,data=df, design = df_hispanic), exponentiate=FALSE)
c_other =tbl_regression(svyglm(logCD8_CD4~combatzone+age+sex,data=df, design = df_other), exponentiate=FALSE)
combat_CD8_CD4 = tbl_merge(tbls=list(c_full, c_white, c_black, c_hispanic, c_other), tab_spanner = c("**All Participants**", "**Non Hispanic White**", "**Non-Hispanic Black**", "**Hispanic**", "**Other**"))

CD8_CD4 = tbl_stack(tbls=list(prison_CD8_CD4, homeless_CD8_CD4, combat_CD8_CD4)) ###CD4_CD8 Table

###Model 1: CD4 Memory:Naive 
p2_full= tbl_regression(svyglm(logCD4M_N~prison+age+sex, data=df, design=dfw), exponentiate=FALSE)
p2_white= tbl_regression(svyglm(logCD4M_N~prison+age+sex,data=df, design = df_white), exponentiate=FALSE)
p2_black = tbl_regression(svyglm(logCD4M_N~prison+age+sex,data=df, design = df_black), exponentiate=FALSE)
p2_hispanic =tbl_regression(svyglm(logCD4M_N~prison+age+sex,data=df, design = df_hispanic), exponentiate=FALSE)
p2_other =tbl_regression(svyglm(logCD4M_N~prison+age+sex,data=df, design = df_other), exponentiate=FALSE)

prison_CD4M_N = tbl_merge(tbls=list(p2_full, p2_white, p2_black, p2_hispanic, p2_other), tab_spanner = c("**All Participants**", "**Non Hispanic White**", "**Non-Hispanic Black**", "**Hispanic**", "**Other**"))

h2_full= tbl_regression(svyglm(logCD4M_N~homeless+age+sex, data=df, design=dfw), exponentiate=FALSE)
h2_white= tbl_regression(svyglm(logCD4M_N~homeless+age+sex,data=df, design = df_white), exponentiate=FALSE)
h2_black = tbl_regression(svyglm(logCD4M_N~homeless+age+sex,data=df, design = df_black), exponentiate=FALSE)
h2_hispanic =tbl_regression(svyglm(logCD4M_N~homeless+age+sex,data=df, design = df_hispanic), exponentiate=FALSE)
h2_other =tbl_regression(svyglm(logCD4M_N~homeless+age+sex,data=df, design = df_other), exponentiate=FALSE)
homeless_CD4M_N = tbl_merge(tbls=list(h2_full, h2_white, h2_black, h2_hispanic, h2_other), tab_spanner = c("**All Participants**", "**Non Hispanic White**", "**Non-Hispanic Black**", "**Hispanic**", "**Other**"))

c2_full= tbl_regression(svyglm(logCD4M_N~combatzone+age+sex, data=df, design=dfw), exponentiate=FALSE)
c2_white= tbl_regression(svyglm(logCD4M_N~combatzone+age+sex,data=df, design = df_white), exponentiate=FALSE)
c2_black = tbl_regression(svyglm(logCD4M_N~combatzone+age+sex,data=df, design = df_black), exponentiate=FALSE)
c2_hispanic =tbl_regression(svyglm(logCD4M_N~combatzone+age+sex,data=df, design = df_hispanic), exponentiate=FALSE)
c2_other =tbl_regression(svyglm(logCD4M_N~combatzone+age+sex,data=df, design = df_other), exponentiate=FALSE)
combat_CD4M_N = tbl_merge(tbls=list(c2_full, c2_white, c2_black, c2_hispanic, c2_other), tab_spanner = c("**All Participants**", "**Non Hispanic White**", "**Non-Hispanic Black**", "**Hispanic**", "**Other**"))

CD4M_N = tbl_stack(tbls=list(prison_CD4M_N, homeless_CD4M_N, combat_CD4M_N))

###CD8 Memory to Naive 

p3_full= tbl_regression(svyglm(logCD8M_N~prison+age+sex, data=df, design=dfw), exponentiate=FALSE)
p3_white= tbl_regression(svyglm(logCD8M_N~prison+age+sex,data=df, design = df_white), exponentiate=FALSE)
p3_black = tbl_regression(svyglm(logCD8M_N~prison+age+sex,data=df, design = df_black), exponentiate=FALSE)
p3_hispanic =tbl_regression(svyglm(logCD8M_N~prison+age+sex,data=df, design = df_hispanic), exponentiate=FALSE)
p3_other =tbl_regression(svyglm(logCD8M_N~prison+age+sex,data=df, design = df_other), exponentiate=FALSE)

prison_CD8M_N = tbl_merge(tbls=list(p3_full, p3_white, p3_black, p3_hispanic, p3_other), tab_spanner = c("**All Participants**", "**Non Hispanic White**", "**Non-Hispanic Black**", "**Hispanic**", "**Other**"))

h3_full= tbl_regression(svyglm(logCD8M_N~homeless+age+sex, data=df, design=dfw), exponentiate=FALSE)
h3_white= tbl_regression(svyglm(logCD8M_N~homeless+age+sex,data=df, design = df_white), exponentiate=FALSE)
h3_black = tbl_regression(svyglm(logCD8M_N~homeless+age+sex,data=df, design = df_black), exponentiate=FALSE)
h3_hispanic =tbl_regression(svyglm(logCD8M_N~homeless+age+sex,data=df, design = df_hispanic), exponentiate=FALSE)
h3_other =tbl_regression(svyglm(logCD8M_N~homeless+age+sex,data=df, design = df_other), exponentiate=FALSE)
homeless_CD8M_N = tbl_merge(tbls=list(h3_full, h3_white, h3_black, h3_hispanic, h3_other), tab_spanner = c("**All Participants**", "**Non Hispanic White**", "**Non-Hispanic Black**", "**Hispanic**", "**Other**"))

c3_full= tbl_regression(svyglm(logCD8M_N~combatzone+age+sex, data=df, design=dfw), exponentiate=FALSE)
c3_white= tbl_regression(svyglm(logCD8M_N~combatzone+age+sex,data=df, design = df_white), exponentiate=FALSE)
c3_black = tbl_regression(svyglm(logCD8M_N~combatzone+age+sex,data=df, design = df_black), exponentiate=FALSE)
c3_hispanic =tbl_regression(svyglm(logCD8M_N~combatzone+age+sex,data=df, design = df_hispanic), exponentiate=FALSE)
c3_other =tbl_regression(svyglm(logCD8M_N~combatzone+age+sex,data=df, design = df_other), exponentiate=FALSE)
combat_CD8M_N = tbl_merge(tbls=list(c3_full, c3_white, c3_black, c3_hispanic, c3_other), tab_spanner = c("**All Participants**", "**Non Hispanic White**", "**Non-Hispanic Black**", "**Hispanic**", "**Other**"))

CD8M_N = tbl_stack(tbls=list(prison_CD8M_N, homeless_CD8M_N, combat_CD8M_N))

###Total CD  Memory : Naive 
p4_full= tbl_regression(svyglm(logCDM_N~prison+age+sex, data=df, design=dfw), exponentiate=FALSE)
p4_white= tbl_regression(svyglm(logCDM_N~prison+age+sex,data=df, design = df_white), exponentiate=FALSE)
p4_black = tbl_regression(svyglm(logCDM_N~prison+age+sex,data=df, design = df_black), exponentiate=FALSE)
p4_hispanic =tbl_regression(svyglm(logCDM_N~prison+age+sex,data=df, design = df_hispanic), exponentiate=FALSE)
p4_other =tbl_regression(svyglm(logCDM_N~prison+age+sex,data=df, design = df_other), exponentiate=FALSE)

prison_CDM_N = tbl_merge(tbls=list(p4_full, p4_white, p4_black, p4_hispanic, p4_other), tab_spanner = c("**All Participants**", "**Non Hispanic White**", "**Non-Hispanic Black**", "**Hispanic**", "**Other**"))

h4_full= tbl_regression(svyglm(logCDM_N~homeless+age+sex, data=df, design=dfw), exponentiate=FALSE)
h4_white= tbl_regression(svyglm(logCDM_N~homeless+age+sex,data=df, design = df_white), exponentiate=FALSE)
h4_black = tbl_regression(svyglm(logCDM_N~homeless+age+sex,data=df, design = df_black), exponentiate=FALSE)
h4_hispanic =tbl_regression(svyglm(logCDM_N~homeless+age+sex,data=df, design = df_hispanic), exponentiate=FALSE)
h4_other =tbl_regression(svyglm(logCDM_N~homeless+age+sex,data=df, design = df_other), exponentiate=FALSE)
homeless_CDM_N = tbl_merge(tbls=list(h4_full, h4_white, h4_black, h4_hispanic, h4_other), tab_spanner = c("**All Participants**", "**Non Hispanic White**", "**Non-Hispanic Black**", "**Hispanic**", "**Other**"))

c4_full= tbl_regression(svyglm(logCDM_N~combatzone+age+sex, data=df, design=dfw), exponentiate=FALSE)
c4_white= tbl_regression(svyglm(logCDM_N~combatzone+age+sex,data=df, design = df_white), exponentiate=FALSE)
c4_black = tbl_regression(svyglm(logCDM_N~combatzone+age+sex,data=df, design = df_black), exponentiate=FALSE)
c4_hispanic =tbl_regression(svyglm(logCDM_N~combatzone+age+sex,data=df, design = df_hispanic), exponentiate=FALSE)
c4_other =tbl_regression(svyglm(logCDM_N~combatzone+age+sex,data=df, design = df_other), exponentiate=FALSE)
combat_CDM_N = tbl_merge(tbls=list(c4_full, c4_white, c4_black, c4_hispanic, c4_other), tab_spanner = c("**All Participants**", "**Non Hispanic White**", "**Non-Hispanic Black**", "**Hispanic**", "**Other**"))

CDM_N = tbl_stack(tbls=list(prison_CDM_N, homeless_CDM_N, combat_CDM_N))

T_Cell_Model_1 = tbl_stack(tbls=list(CD8_CD4, CD4M_N, CD8M_N, CDM_N), group_header = c("CD8:CD4", "CD4 Memory:Naive", "CD8 Memory:Naive", "Total Memory:Naive") )

install.packages("flextable")
library(flextable)
install.packages("officer")
library(officer)

tcell_m1 = as_flex_table(T_Cell_Model_1)
sect_properties <- prop_section(
  page_size = page_size(orient = "landscape",
                        width = 8, height = 11),
  type = "continuous",
  page_margins = page_mar()
)

save_as_docx(tcell_m1, path="~/Desktop/Data Files/Table_notexp.docx", pr_section = sect_properties)


