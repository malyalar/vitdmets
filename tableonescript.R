library(survival) # only needed for the dataset in this example
library(dplyr) # to modify the needed dataframe
library(tibble) # for rownames_to_column() function
library(stringr) # for str_squish()
library(flextable)
library(officer)
library(forcats)
library(tableone)
library(knitr)
library(kableExtra)
library(plyr)
library(labelled)
#source("/Users/malyalar/Dropbox/Renal Transplant Research/GoalDirAnesthesiaKT/src/rscripts/customtab.R")


#df_plus <- read.csv("/Users/malyalar/Dropbox/Renal Transplant Research/vitd_mets_study/data/processed/ptFeaturesPlus.csv", header=TRUE, stringsAsFactors=FALSE)
#df_plus <- within(df_plus, rm("otherKidneyDis"))                        


df_plus <- txdata_bw1_rv1 
df_plus <- df_plus[df_plus$ESRD_cause_coded != 2, ]
df_plus <- df_plus[-2139, ]  
df_plus <- df_plus[df_plus$NCEPATPIII %in% c('yes', 'no'), ]
df_plus$ethnicity_coded <- ifelse(df_plus$ethnicity_coded == 1, "South Asian",
                                  ifelse(df_plus$ethnicity_coded == 2, "East Asian",
                                         ifelse(df_plus$ethnicity_coded == 3, "Black/Afro-Caribbean",
                                                ifelse(df_plus$ethnicity_coded == 4, "White",
                                                       "Other/unknown"))))
df_plus$ESRD_cause_coded <- ifelse(df_plus$ESRD_cause_coded == 1, "hypertension",
                                   ifelse(df_plus$ESRD_cause_coded == 0, "other",
                                          df_plus$ESRD_cause_coded))


# Variable modifications to play nicely with tableone package
#df_plus$PRA <- as.numeric(df_plus$PRA)
#df_plus$dialysisDuration <- df_plus$dialysisDuration/365

#df_plus$donorHTN <- revalue(df_plus$donorHTN, c("Y"=1))
#df_plus$donorHTN <- revalue(df_plus$donorHTN, c("N"=0))
#df_plus$donorHTN <- as.numeric(df_plus$donorHTN)

#df_plus$donorDM <- revalue(df_plus$donorDM, c("Y"=1))
#df_plus$donorDM <- revalue(df_plus$donorDM, c("N"=0))
#df_plus$donorDM <- as.numeric(df_plus$donorDM)

#df_plus$donorSex <- revalue(df_plus$donorSex, c("M"=1))
#df_plus$donorSex <- revalue(df_plus$donorSex, c("F"=0))
#df_plus$donorSex <- as.numeric(df_plus$donorSex)

#df_plus$donorSide <- revalue(df_plus$donorSide, c("L"=1))
#df_plus$donorSide <- revalue(df_plus$donorSide, c("R"=0))
#df_plus$donorSide <- as.numeric(df_plus$donorSide)

# first -- establish variables for table_1_summary
# strata are MetS/no MetS

numericRecipPretxVars <- c("AgeAtTx", "AgeAtVitD", "TimeFollowed", "BMI", 
                           "SBP", "DBP",  "w2hRatio")
categRecipPreTxVars <- c("ethnicity_coded","smoking_coded")
binaryRecipPreTxVars <- c("Male", "graft_type_coded", "pretx_MACE_any")


# Outcomes
binaryOutcomeVars <- c('PTDMStatus_v1','survStatus', 'AcuteReject', "DGF_Status")

# bloodwork features
numericBwVars <- c('HDL', "LDL","NonHDLchol", "TG", "Uprot24h", "FBS", "RBS", "ApoA1", "ApoB", 
                     "BA1ratio", "sCr", "GFRmdrd",
                     "CRP","MACR","UricAcid", "PTH", "VitD25")

# medication use
binaryDrugVars <- c("Statin", "ACEI", "ARB", "OH", "Prednisone", "Tacrolimus")


var_label(df_plus) <- list(# Recipient variable names
                           AgeAtTx = "Age at transplant (years)", 
                           AgeAtVitD = "Age at bloodwork (years)", 
                           Male = "Male sex",
                           TimeFollowed = "Mean years follow-up",
                           ethnicity_coded = "Race/Ethnicity",
                           #dialysisDuration = "Dialysis vintage (years)", 
                           BMI = "BMI (kg/m^2)", 
                           SBP = "Systolic BP (mmHg)", 
                           DBP = "Diastolic BP (mmHg)", 
                           #recipMAP_pretx = "MAP (mmHg)", 
                           w2hRatio = "Waist-to-hip ratio",
                           graft_type_coded = "Live donor",
                           smoking_coded = "Smoking status",
                           pretx_MACE_any = "Pre-transplant MACE",
                           
                           # Outcome variable names 'PTDMStatus_v1','survStatus',
                           PTDMStatus_v1 = "Post-transplant DM",
                           survStatus = "Post-transplant MACE",
                           AcuteReject = "Acute rejection",
                           DGF_Status = "Delayed graft function",

                           # bloodwork features
                           VitD25 = "Vitamin D-25 (ng/mL)", 
                           PTH = "Parathyroid hormone (ng/mL)", 
                           HDL = "HDL (mmol/L)", 
                           LDL = "LDL (mmol/L)",
                           TG = "Triglycerides (mmol/L)", 
                           FBS = "Fasting blood sugar(mmol/L)",
                           RBS = "Random blood sugar (mmol/L)", 
                           ApoA1 = "Apolipoprotein A1",
                           ApoB = "Apolipoprotein B", 
                           BA1ratio = "Apo-B/Apo-A1 ratio",   
                           sCr = "serum creatinine  (mmol/L)",
                           GFRmdrd = "GFR (MDRD formula)", 
                           NonHDLchol = "Non-HDL cholesterol",
                           CRP = "C-reactive protein", 
                           UricAcid = 'Uric acid (mmol/L)',
                           MACR = "Albumin to creatinine ratio",   
                           Uprot24h = "24-hour urine protein",
                           
                           # Medications and Fluids binaryDrugVars <- c("Statin", "ACEI", "ARB", "OH", "Insulin", "Prednisone")
                           Statin = "Using statin", 
                           ACEI = "Using ACE-inhibitor", 
                           ARB = "Using ARB",
                           OH = "Using oral hypoglycemic", 
                           Prednisone = "Using prednisone",
                           Tacrolimus = "Tacrolimus (vs. cyclosporine)"
                           )



tableOne <- CreateTableOne(data=df_plus, 
                          vars = c(
                                   # Recipient variables
                                   "AgeAtTx", "AgeAtVitD", "TimeFollowed", "ethnicity_coded",
                                      # 4 + 9 (categorical) + 1 + 3 (categorical)
                                   
                                   # recip characteristics variables
                                   "SBP", "DBP",  "BMI", "w2hRatio", "smoking_coded",       # 8
                                   "Male", "graft_type_coded", "pretx_MACE_any",
                                   
                                   # recip outcomes
                                   'PTDMStatus_v1','survStatus', "AcuteReject", "DGF_Status", # 4 
                                   
                                   # Hemodynamics 
                                   'HDL', "LDL", "NonHDLchol", "TG", "Uprot24h",
                                   "FBS", "RBS", "ApoA1", "ApoB", 
                                   "BA1ratio", "sCr", "GFRmdrd", 
                                   "CRP","MACR","UricAcid", "PTH", "VitD25",     # 18
                                   "Statin", "ACEI", "ARB", "OH", "Prednisone", "Tacrolimus"       # 6 + 6 +3 + 18 + 6
                                     ), 
                          
                          factorVars = c(categRecipPreTxVars, binaryRecipPreTxVars,
                                         binaryOutcomeVars, binaryDrugVars),
                          strata=c('VitDmed','NCEPATPIII'),
                          test=TRUE)
p <- print(tableOne, 
           quote = FALSE, 
           noSpaces = TRUE,
           explain=FALSE,
           showAllLevels=FALSE,
           varLabels = T,
           test=TRUE) 
           # cramVars = "sex", quote = TRUE))
p = subset(p, select = -c(test) )
colnames(p) <- c("below med.", "above med.", "below med.", "above med.", "p")



kbl(p, booktabs = TRUE, format = "latex") %>% 
  kable_styling(latex_options = c("scale_down")) %>% 
  #kable_styling(latex_options = "striped", stripe_index = c(1,2, 5:6)) %>%  
  kable_styling(position = "center") %>%
  group_rows("Patient demographics", 2, 10) %>%
  group_rows("Patient characteristics", 11, 18) %>%
  group_rows("Patient outcomes in followup", 19, 22) %>%
  group_rows("3-month post-tx. bloodwork", 23, 39) %>%
  group_rows("Medications at 3 months", 40, 44) %>%
  add_header_above(c(" " = 1, "No metabolic syndrome" = 2, "Metabolic syndrome" = 2, " " = 1))



## keep this in mind for later: package tableone also has a ShowRegTable function
# It shows the regression result in the HR [95% CI] p-value format,
# which is usually the form used in medical research papers.
#ShowRegTable(
#  model,
#  exp = TRUE,
#  digits = 2,
#  pDigits = 3,
#  printToggle = TRUE,
#  quote = FALSE,
#  ciFun = confint
#)

