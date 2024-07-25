library(plyr)
library(readr)
require(ggiraph)
require(ggiraphExtra)
library(ggeffects)
require(broom) # for tidy()
require(knitr)
require(ggplot2)
library(pixiedust)
library(ggh4x)
library(patchwork)
library(gridExtra)
library(tablesgg)
library(tab)
library(survival)
library(forestmodel)
library(tableone)

# Convert the plot to grayscale
theme_set(theme_bw())

# Define line styles for each factor
line_styles <- c("solid",  "longdash", "twodash", "dashed", "dotdash", "dotted" )


# Create the new column NCEP_crit_no2
#df_outcomes$NCEP_crit_no2 <- ifelse(df_outcomes$NCEP_crit_no > 0 & 
#                                      ((df_outcomes$Male == 1 & df_outcomes$WaistSize >= 40*2.54) |
#                                         (df_outcomes$Male == 0 & df_outcomes$WaistSize >= 35*2.54)),
#                                    df_outcomes$NCEP_crit_no - 1,
#                                    df_outcomes$NCEP_crit_no)

# Create temporary columns for each condition
df_outcomes$Condition1 <- ifelse((df_outcomes$WaistSize > (40*2.54) & df_outcomes$Male == 1) | (df_outcomes$WaistSize > (35*2.54) & df_outcomes$Male == 0), 1, 0)
df_outcomes$Condition2 <- ifelse(df_outcomes$SBP > 130 | df_outcomes$DBP > 85, 1, 0)
df_outcomes$Condition3 <- ifelse(df_outcomes$TG > 3.90, 1, 0)
df_outcomes$Condition4 <- ifelse((df_outcomes$HDL < 1.03 & df_outcomes$Male == 1) | (df_outcomes$HDL < 1.30 & df_outcomes$Male == 0), 1, 0)
df_outcomes$Condition5 <- ifelse(df_outcomes$FBS > 5.5, 1, 0)

# Sum up the temporary columns to get the count of criteria met
df_outcomes$NCEP_crit_no3 <- rowSums(df_outcomes[, c("Condition1","Condition2","Condition3", "Condition4", "Condition5")])

# Remove temporary columns
df_outcomes <- df_outcomes[, !(names(df_outcomes) %in% c("Condition1","Condition2","Condition3", "Condition4", "Condition5"))]


# Fit the Cox PH model
model <- coxph(Surv(survTimePTDM_v3, PTDMStatus_v3) ~ VitD25+NCEP_crit_no3 +  Male + AgeAtVitD  + HbA1c, data = df_outcomes)

# Generate predicted values for the marginal effects
marginal_effects <- ggpredict(model = model,
                              terms = c("VitD25", "NCEP_crit_no3"),
                              type = "re")

# Manually create the plot with grayscale and different line styles
p <- ggplot(marginal_effects, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  geom_line(aes(group = group, linetype = group), size = 0.6) +
  scale_color_grey() +
  scale_fill_grey() +
  scale_linetype_manual(values = line_styles) +
  labs(x = "Vitamin D-25 (nmol/L)", y = "Cox PH model\nMarginal effects on HR") + 
  coord_cartesian(xlim = c(0, 155), ylim=c(0,5))


#p <- p + theme(legend.position="bottom")

p$labels$group <- "NCEPATPIII\ncriteria met"
p$labels$fill <- "NCEPATPIII\ncriteria met"
p$labels$linetype <- "NCEPATPIII\ncriteria met"

#p <- p+guides(fill=guide_legend(ncol=2,byrow=TRUE))


df_outcomes$VitD25_10 = df_outcomes$VitD25/10
df_outcomes$AgeAtVitD_10 = df_outcomes$AgeAtVitD/10
df_outcomes$PTH_10 = df_outcomes$PTH/10


model_div <- coxph(Surv(survTimePTDM_v3, PTDMStatus_v3) ~ VitD25_10+NCEP_crit_no3  + Male + AgeAtVitD_10 + FBS + HbA1c, data = df_outcomes)

regTable <- ShowRegTable(
  model_div,
  exp = TRUE,
  digits = 2,
  pDigits = 3,
  printToggle = TRUE,
  quote = FALSE,
  ciFun = confint
)

rownames(regTable) <- c("Vitamin D-25 (10 nmol/L)", "NCEPATPIII criteria met", "Male sex","Age (10 years)", "Fasting sugar (mmol/L)", "HbA1c (%)" ) #"NCEP-ATP-III crtieria * Vit-D25 level"
colnames(regTable) <- c("HR [95% conf. int.]", "p-value")
regTable[, "p-value"] <- sprintf("%15s", regTable[, "p-value"])


table <- textTable(regTable, text_format = c("bold"), font_size = 15)
q <- plot(table, scale=0.9)

p + q + plot_layout(ncol=1, heights=unit(c(3,2),c('null','null')))
ggsave(filename = "/Users/malyalar/Dropbox/Renal Transplant Research/vitd_mets_study/figures/png/marginalEffectsPlot.png", width = 5, height = 4, device='png', dpi=900)



