###Set up working directory, load packages
setwd("~/Desktop/Data Files")
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

fn12 <-  "~/Desktop/Data Files/H12LB_R.da"
sas.input12 <-  "~/Desktop/Data Files/H12LB_R.sas"
hrs12_lbs<- read.SAScii(fn12, sas.input12)


fn14 <-  "~/Desktop/Data Files/H14LB_R.da"
sas.input14 <-  "~/Desktop/Data Files/H14LB_R.sas"
hrs14_lbs<- read.SAScii(fn14, sas.input14)

m114 <- "~/Desktop/Data Files/H14M1_R.da" ##Reading in disability data 
m114sas <- "~/Desktop/Data Files/H14M1_R.sas"
m1_14 <- read.SAScii(m114, m114sas)

m214 <- "~/Desktop/Data Files/H14M2_R.da"
m214sas <- "~/Desktop/Data Files/H14M2_R.sas"
m2_14 <- read.SAScii(m214, m214sas)

m112 <- "~/Desktop/Data Files/H12M1_R.da"
m112sas <- "~/Desktop/Data Files/H12M1_R.sas"
m1_12 <- read.SAScii(m112, m112sas)

m212 <- "~/Desktop/Data Files/H12M2_R.da"
m212sas <- "~/Desktop/Data Files/H12M2_R.sas"
m2_12 <- read.SAScii(m214, m212sas)

m12014 <- m1_14[,c("HHID", "PN", "OM002", "OM004", "OM005", "OM006", "OM007", "OM008", "OM009")]
m12012 <- m1_12[,c("HHID", "PN", "NM002", "NM004", "NM005", "NM006", "NM007", "NM008", "NM009")]
