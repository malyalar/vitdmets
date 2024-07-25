library(tab)
library(survival)
library(forestmodel)
library(patchwork)
library(clipr)
library(survminer)

# Read the clipboard data into a data frame
#new_data <- read.table(text = read_clip(), header = TRUE, sep = "\t")
#data_PTDM <- merge(txdata_bw1_rv1[txdata_bw1_rv1$pretx_DM_v2 == 0,], new_data, by = c("PatientID", "TxID"), all.x = TRUE)

pretty_data_crit_PTDM <- data_PTDM %>%
  transmute(
    survTimePTDM_v3,
    PTDMStatus_v3,
    'Waist size criterion' = waist_crit,
    'Triglycerides criterion' = tg_crit,
    'Blood pressure criterion' = bp_crit,
    'HDL criterion' = hdl_crit,
    'Fasting blood gluc. ' = gluc_crit,
    'Male sex' = Male
  )

p <- forest_model(coxph(Surv(survTimePTDM_v3, PTDMStatus_v3) ~ ., pretty_data_crit_PTDM))
#p$theme$title = "Multivariate Cox regression; criteria of metabolic syndrome"

pretty_data_PTDM <- data_PTDM %>%
  transmute(
    survTimePTDM_v3,
    PTDMStatus_v3,
    'Waist size (10 cm)' = WaistSize/10,
    'Triglycerides (mmol/L)' = TG,
    'SBP (10 mmHg)' = SBP/10,
    'HDL (mmol/L)' = HDL,
    'FBG (5 mmol/L) ' = FBS/5,
    'Male sex' = Male
  )

q <- forest_model(coxph(Surv(survTimePTDM_v3, PTDMStatus_v3) ~ ., pretty_data_PTDM))
#q$theme$title = "Multivariate Cox regression; continuous metabolic syndrome parameters"



dm_splots <- list()

dm_splots[[1]] <- ggsurvplot(survfit(Surv(survTimePTDM_v3, PTDMStatus_v3)~waist_crit,
                                     # set exclusion criteria in subsetting function
                                     # version selection in initial Surv() (i.e. survTimePTDM_v1/2/3) will also make a difference
                                     data=data_PTDM),
                             pval = T,
                             ggtheme = theme_pubr() + theme(text = element_text(size = 10)),
                             palette=c("black", "black"),
                             #legend.labs=c("Criterion positive", "Criterion negative"),
                             linetype=c("solid","dashed"),
                             test.for.trend = F,
                             conf.int = TRUE,  
                             conf.int.alpha = 0.2,
                             break.time.by=300, censor=F, 
                             # change titles and x/y axis labels
                             xlab="Days post-transplant",
                             ylab="PTDM-free survival",
                             legend = "none",
                             size = 0.5,
                             xlim=c(0,1800), ylim=c(0.72,1), pval.coord=c(10, 0.73))

dm_splots[[2]] <- ggsurvplot(survfit(Surv(survTimePTDM_v3, PTDMStatus_v3)~hdl_crit,
                                     # set exclusion criteria in subsetting function
                                     # version selection in initial Surv() (i.e. survTimePTDM_v1/2/3) will also make a difference
                                     data=data_PTDM),
                             pval = T,
                             ggtheme = theme_pubr() + theme(text = element_text(size = 10)),
                             palette=c("black", "black"),
                             #legend.labs=c("Criterion positive", "Criterion negative"),
                             linetype=c("solid","dashed"),
                             test.for.trend = F,
                             conf.int = TRUE,  
                             conf.int.alpha = 0.2,
                             break.time.by=300, censor=F, 
                             # change titles and x/y axis labels
                             xlab="Days post-transplant",
                             ylab="PTDM-free survival",
                             title="HDL ratio criterion", 
                             legend = "none",
                             size = 0.5,
                             xlim=c(0,1800), ylim=c(0.72,1), pval.coord=c(10, 0.73))


dm_splots[[3]] <- ggsurvplot(survfit(Surv(survTimePTDM_v3, PTDMStatus_v3)~tg_crit,
                                     # set exclusion criteria in subsetting function
                                     # version selection in initial Surv() (i.e. survTimePTDM_v1/2/3) will also make a difference
                                     data=data_PTDM),
                             pval = T,
                             ggtheme = theme_pubr() + theme(text = element_text(size = 10)),
                             palette=c("black", "black"),
                             #legend.labs=c("Criterion positive", "Criterion negative"),
                             linetype=c("solid","dashed"),
                             test.for.trend = F,
                             conf.int = TRUE,  
                             conf.int.alpha = 0.2,
                             break.time.by=300, censor=F, 
                             # change titles and x/y axis labels
                             xlab="Days post-transplant",
                             ylab="PTDM-free survival",
                             title="Triglycerides criterion", 
                             legend = "none",
                             size = 0.5,
                             xlim=c(0,1800), ylim=c(0.72,1), pval.coord=c(10, 0.73))

dm_splots[[4]] <- ggsurvplot(survfit(Surv(survTimePTDM_v3, PTDMStatus_v3)~bp_crit,
                                     # set exclusion criteria in subsetting function
                                     # version selection in initial Surv() (i.e. survTimePTDM_v1/2/3) will also make a difference
                                     data=data_PTDM),
                             pval = T,
                             ggtheme = theme_pubr() + theme(text = element_text(size = 10)),
                             palette=c("black", "black"),
                             #legend.labs=c("Criterion positive", "Criterion negative"),
                             linetype=c("solid","dashed"),
                             test.for.trend = F,
                             conf.int = TRUE,  
                             conf.int.alpha = 0.2,
                             break.time.by=300, censor=F, 
                             # change titles and x/y axis labels
                             xlab="Days post-transplant",
                             ylab="PTDM-free survival",
                             title="Blood pressure criterion", 
                             legend = "none",
                             size = 0.5,
                             xlim=c(0,1800), ylim=c(0.72,1), pval.coord=c(10, 0.73))

dm_splots[[5]] <- ggsurvplot(survfit(Surv(survTimePTDM_v3, PTDMStatus_v3)~gluc_crit,
                                     # set exclusion criteria in subsetting function
                                     # version selection in initial Surv() (i.e. survTimePTDM_v1/2/3) will also make a difference
                                     data=data_PTDM),
                             pval = T,
                             ggtheme = theme_pubr() + theme(text = element_text(size = 10)),
                             palette=c("black", "black"),
                             #legend.labs=c("Criterion positive", "Criterion negative"),
                             linetype=c("solid","dashed"),
                             test.for.trend = F,
                             conf.int = TRUE,  
                             conf.int.alpha = 0.2,
                             break.time.by=300, censor=F, 
                             # change titles and x/y axis labels
                             xlab="Days post-transplant",
                             ylab="PTDM-free survival",
                             title="Fasting glucose criterion", 
                             legend = "none",
                             size = 0.5,
                             xlim=c(0,1800), ylim=c(0.5,1), pval.coord=c(10, 0.52))

dm_splots[[6]] <- ggsurvplot(survfit(Surv(survTimePTDM_v3, PTDMStatus_v3)~Male,
                                     # set exclusion criteria in subsetting function
                                     # version selection in initial Surv() (i.e. survTimePTDM_v1/2/3) will also make a difference
                                     data=data_PTDM),
                             pval = T,
                             ggtheme = theme_pubr() + theme(text = element_text(size = 10)),
                             palette=c("black", "black"),
                             #legend.labs=c("Criterion positive", "Criterion negative"),
                             linetype=c("solid","dashed"),
                             test.for.trend = F,
                             conf.int = TRUE,  
                             conf.int.alpha = 0.2,
                             break.time.by=300, censor=F, 
                             # change titles and x/y axis labels
                             xlab="Days post-transplant",
                             ylab="PTDM-free survival",
                             title="Male sex", 
                             legend = "none",
                             size = 0.5,
                             xlim=c(0,1800), ylim=c(0.72,1), pval.coord=c(10, 0.73))

survplots <- arrange_ggsurvplots(dm_splots, print = TRUE, ncol = 3, nrow = 2, labels=c("A","B","C","D","E", "F"))





p / q /survplots + plot_layout(heights = c(2, 2, 6))
ggsave(filename = "/Users/malyalar/Dropbox/Renal Transplant Research/vitd_mets_study/figures/png/forestPlots_metscriteria.png", width = 9, height = 10, device='png', dpi=900)





















library(survival)
library(broom)

# Perform univariate Cox regressions
univariate_models <- lapply(names(pretty_data_PTDM)[-c(1:2)], function(var) {
  fit <- coxph(Surv(survTimePTDM_v3, PTDMStatus_v3) ~ ., data = pretty_data_PTDM[, c("survTimePTDM_v3", "PTDMStatus_v3", var)])
  tidy(fit)
})

# Combine the results into a table
univariate_table <- bind_rows(univariate_models, .id = "Variable")

# Print the table
print(univariate_table)


                  
