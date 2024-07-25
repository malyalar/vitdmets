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

# Convert the plot to grayscale
theme_set(theme_bw())

# Define line styles for each factor
line_styles <- c("solid",  "longdash", "twodash", "dashed", "dotdash", "dotted" )

# Fit the Cox PH model
model <- coxph(Surv(survTimePTDM_v3, PTDMStatus_v3) ~ VitD25*hdl_crit +AgeAtVitD+Male , data = data_PTDM)

# Generate predicted values for the marginal effects
marginal_effects <- ggpredict(model = model,
                              terms = c("VitD25", "hdl_crit"),
                              type = "re")

# Manually create the plot with grayscale and different line styles
p <- ggplot(marginal_effects, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  geom_line(aes(group = group, linetype = group), size = 0.6) +
  scale_color_grey() +
  scale_fill_grey() +
  ylim(0, 4.5) + 
  theme(legend.background = element_rect(fill = "transparent")) +
  scale_linetype_manual(values = line_styles) +
  labs(x = "Vitamin D-25 (nmol/L)", y = "Marginal effects on HR")

p$labels$group <- "HDL criteria met"
p$labels$fill <- "HDL criteria met"
p$labels$linetype <- "HDL criteria met"

pvals_list <- summary(model)[['coefficients']][,'Pr(>|z|)']
p <- p + annotate("text", x = 0, y = 3.65 +  0, hjust=0, label = paste0("Vit.D-25: ", round(pvals_list[1], 3)), vjust = 0, size = 3)
p <- p + annotate("text", x = 0.05, y = 3.65 +  0.2, hjust=0, label = paste0("Criterion: ", round(pvals_list[2], 3)), vjust = 0, size = 3)
p <- p + annotate("text", x = 0, y = 3.65 +  0.4, hjust=0, label = paste0("Interaction: ", round(pvals_list[3], 3)), vjust = 0, size = 3)
p <- p + annotate("text", x = 0, y = 3.65 +  0.65, hjust=0, label = expression(underline("P-values:")), vjust = 0, size = 3)




model <- coxph(Surv(survTimePTDM_v3, PTDMStatus_v3) ~ VitD25*tg_crit +AgeAtVitD+Male, data = data_PTDM)

# Generate predicted values for the marginal effects
marginal_effects <- ggpredict(model = model,
                              terms = c("VitD25", "tg_crit"),
                              type = "re")

# Manually create the plot with grayscale and different line styles
q <- ggplot(marginal_effects, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  geom_line(aes(group = group, linetype = group), size = 0.6) +
  scale_color_grey() +
  scale_fill_grey() +
  ylim(0, 4.5) + 
  theme(legend.background = element_rect(fill = "transparent")) +
  scale_linetype_manual(values = line_styles) +
  labs(x = "Vitamin D-25 (nmol/L)", y = "Marginal effects on HR")

q$labels$group <- "TG criteria met"
q$labels$fill <- "TG criteria met"
q$labels$linetype <- "TG criteria met"

pvals_list <- summary(model)[['coefficients']][,'Pr(>|z|)']
q <- q + annotate("text", x = 0, y = 3.65 +  0, hjust=0, label = paste0("Vit.D-25: ", round(pvals_list[1], 3)), vjust = 0, size = 3)
q <- q + annotate("text", x = 0.05, y = 3.65 +  0.2, hjust=0, label = paste0("Criterion: ", round(pvals_list[2], 3)), vjust = 0, size = 3)
q <- q + annotate("text", x = 0, y = 3.65 +  0.4, hjust=0, label = paste0("Interaction: ", round(pvals_list[3], 3)), vjust = 0, size = 3)
q <- q + annotate("text", x = 0, y = 3.65 +  0.65, hjust=0, label = expression(underline("P-values:")), vjust = 0, size = 3)



model <- coxph(Surv(survTimePTDM_v3, PTDMStatus_v3) ~ VitD25*bp_crit +AgeAtVitD+Male, data = data_PTDM)

# Generate predicted values for the marginal effects
marginal_effects <- ggpredict(model = model,
                              terms = c("VitD25", "bp_crit"),
                              type = "re")

# Manually create the plot with grayscale and different line styles
r <- ggplot(marginal_effects, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  geom_line(aes(group = group, linetype = group), size = 0.6) +
  scale_color_grey() +
  scale_fill_grey() +
  ylim(0, 4.5) + 
  theme(legend.background = element_rect(fill = "transparent")) +
  scale_linetype_manual(values = line_styles) +
  labs(x = "Vitamin D-25 (nmol/L)", y = "Marginal effects on HR")

r$labels$group <- "BP criteria met"
r$labels$fill <- "BP criteria met"
r$labels$linetype <- "BP criteria met"

pvals_list <- summary(model)[['coefficients']][,'Pr(>|z|)']
r <- r + annotate("text", x = 0, y = 3.65 +  0, hjust=0, label = paste0("Vit.D-25: ", round(pvals_list[1], 3)), vjust = 0, size = 3)
r <- r + annotate("text", x = 0.05, y = 3.65 +  0.2, hjust=0, label = paste0("Criterion: ", round(pvals_list[2], 3)), vjust = 0, size = 3)
r <- r + annotate("text", x = 0, y = 3.65 +  0.4, hjust=0, label = paste0("Interaction: ", round(pvals_list[3], 3)), vjust = 0, size = 3)
r <- r + annotate("text", x = 0, y = 3.65 +  0.65, hjust=0, label = expression(underline("P-values:")), vjust = 0, size = 3)




model <- coxph(Surv(survTimePTDM_v3, PTDMStatus_v3) ~ VitD25*waist_crit +AgeAtVitD+Male, data = data_PTDM)

# Generate predicted values for the marginal effects
marginal_effects <- ggpredict(model = model,
                              terms = c("VitD25", "waist_crit"),
                              type = "re")

# Manually create the plot with grayscale and different line styles
s <- ggplot(marginal_effects, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  geom_line(aes(group = group, linetype = group), size = 0.6) +
  scale_color_grey() +
  scale_fill_grey() + 
  ylim(0, 4.5) + 
  theme(legend.background = element_rect(fill = "transparent")) +
  scale_linetype_manual(values = line_styles) +
  labs(x = "Vitamin D-25 (nmol/L)", y = "Marginal effects on HR") 

s$labels$group <- "Waist-to-hip\ncriteria met"
s$labels$fill <- "Waist-to-hip\ncriteria met"
s$labels$linetype <- "Waist-to-hip\ncriteria met"

pvals_list <- summary(model)[['coefficients']][,'Pr(>|z|)']
s <- s + annotate("text", x = 0, y = 3.65 +  0, hjust=0, label = paste0("Vit.D-25: ", round(pvals_list[1], 3)), vjust = 0, size = 3)
s <- s + annotate("text", x = 0.05, y = 3.65 +  0.2, hjust=0, label = paste0("Criterion: ", round(pvals_list[2], 3)), vjust = 0, size = 3)
s <- s + annotate("text", x = 0, y = 3.65 +  0.4, hjust=0, label = paste0("Interaction: ", round(pvals_list[3], 3)), vjust = 0, size = 3)
s <- s + annotate("text", x = 0, y = 3.65 +  0.65, hjust=0, label = expression(underline("P-values:")), vjust = 0, size = 3)





model <- coxph(Surv(survTimePTDM_v3, PTDMStatus_v3) ~ VitD25*gluc_crit +AgeAtVitD+Male, data = data_PTDM)

# Generate predicted values for the marginal effects
marginal_effects <- ggpredict(model = model,
                              terms = c("VitD25", "gluc_crit"),
                              type = "re")

# Manually create the plot with grayscale and different line styles
t <- ggplot(marginal_effects, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  geom_line(aes(group = group, linetype = group), size = 0.6) +
  scale_color_grey() +
  scale_fill_grey() +
  ylim(0, 10) + 
  theme(legend.background = element_rect(fill = "transparent")) +
  scale_linetype_manual(values = line_styles) +
  labs(x = "Vitamin D-25 (nmol/L)", y = "Marginal effects on HR")

t$labels$group <- "Fasting glucose\ncriteria met"
t$labels$fill <- "Fasting glucose\ncriteria met"
t$labels$linetype <- "Fasting glucose\ncriteria met"

pvals_list <- summary(model)[['coefficients']][,'Pr(>|z|)']
t <- t + annotate("text", x = 0, y = 8, hjust=0, label = paste0("Vit.D-25: ", round(pvals_list[1], 3)), vjust = 0, size = 3)
t <- t + annotate("text", x = 0.05, y = 8.5, hjust=0, label = paste0("Criterion: ", round(pvals_list[2], 3)), vjust = 0, size = 3)
t <- t + annotate("text", x = 0, y = 9, hjust=0, label = paste0("Interaction: ", round(pvals_list[3], 3)), vjust = 0, size = 3)
t <- t + annotate("text", x = 0, y = 9.5, hjust=0, label = expression(underline("P-values:")), vjust = 0, size = 3)



model <- coxph(Surv(survTimePTDM_v3, PTDMStatus_v3) ~ VitD25*Male, data = data_PTDM)

# Generate predicted values for the marginal effects
marginal_effects <- ggpredict(model = model,
                              terms = c("VitD25", "Male"),
                              type = "re")

# Manually create the plot with grayscale and different line styles
u <- ggplot(marginal_effects, aes(x = x, y = predicted)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high, fill = group), alpha = 0.2) +
  geom_line(aes(group = group, linetype = group), size = 0.6) +
  scale_color_grey() +
  scale_fill_grey() +
  ylim(0, 4.5) + 
  theme(legend.background = element_rect(fill = "transparent")) +
  scale_linetype_manual(values = line_styles) +
  labs(x = "Vitamin D-25 (nmol/L)", y = "Marginal effects on HR")

u$labels$group <- "Male sex"
u$labels$fill <- "Male sex"
u$labels$linetype <- "Male sex"

pvals_list <- summary(model)[['coefficients']][,'Pr(>|z|)']
u <- u + annotate("text", x = 0, y = 3.65 +  0, hjust=0, label = paste0("Vit.D-25: ", round(pvals_list[1], 3)), vjust = 0, size = 3)
u <- u + annotate("text", x = 0.05, y = 3.65 +  0.2, hjust=0, label = paste0("Criterion: ", round(pvals_list[2], 3)), vjust = 0, size = 3)
u <- u + annotate("text", x = 0, y = 3.65 +  0.4, hjust=0, label = paste0("Interaction: ", round(pvals_list[3], 3)), vjust = 0, size = 3)
u <- u + annotate("text", x = 0, y = 3.65 +  0.65, hjust=0, label = expression(underline("P-values:")), vjust = 0, size = 3)



# Create a list of plots
plots <- list(p, q, r, s, t, u)

# Iterate through the list and modify the legend position

p <- p + theme(legend.position = c(0.99,0.99),legend.justification = c(1, 1))
q <- q + theme(legend.position = c(0.99,0.99),legend.justification = c(1, 1))
r <- r + theme(legend.position = c(0.99,0.99),legend.justification = c(1, 1))
s <- s + theme(legend.position = c(0.99,0.99),legend.justification = c(1, 1))
t <- t + theme(legend.position = c(0.99,0.99),legend.justification = c(1, 1))
u <- u + theme(legend.position = c(0.99,0.99),legend.justification = c(1, 1))




(p + q + r) / (s + t + u) + plot_layout(heights = c(2.5, 2.5))
ggsave(filename = "/Users/malyalar/Dropbox/Renal Transplant Research/vitd_mets_study/figures/png/serialinteractionplot.png", width = 10, height = 6, device='png', dpi=900)








