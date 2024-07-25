library(ggplot2)
library(gridExtra)
library(patchwork)

## DATA

# Data for PTDM plot (p)
pdata <- data.frame(
  Quartile = c(1, 2, 3, 4),
  Followup_PTDM_No_MetS = c(618.312328767123, 641.153424657534, 1015.76164383562, 1413.22191780822),
  Events_PTDM_No_MetS = c(21, 19, 20, 25),
  Events_per_100_PTDM_No_MetS = c(3.39634178763227, 2.96340926668974, 1.96896586136861, 1.76900737845708),
  Followup_PTDM_With_MetS = c(945.26301369863, 1144.1698630137, 1298.85479452055, 1683.91506849315),
  Events_PTDM_With_MetS = c(40, 38, 47, 26),
  Events_per_100_PTDM_With_MetS = c(4.23162648070697, 3.32118518660415, 3.61857231449411, 1.54402086461914)
)

pval_PTDM_noMetS <- prop.trend.test(pdata$Events_PTDM_No_MetS, pdata$Followup_PTDM_No_MetS, score = seq_along(pdata$Events_PTDM_No_MetS))
pval_PTDM_withMetS <- prop.trend.test(pdata$Events_PTDM_With_MetS, pdata$Followup_PTDM_With_MetS, score = seq_along(pdata$Events_PTDM_With_MetS))

# Convert data to long format
pdata_long <- tidyr::pivot_longer(pdata, 
                                  cols = c(Events_per_100_PTDM_No_MetS, Events_per_100_PTDM_With_MetS), 
                                  names_to = "Group",
                                  values_to = "Events_per_100")
# Add "followup" column
pdata_long$Followup <- ifelse(grepl("No_MetS", pdata_long$Group), pdata$Followup_PTDM_No_MetS, pdata$Followup_PTDM_With_MetS)
pdata_long$Events <- ifelse(grepl("No_MetS", pdata_long$Group), pdata$Events_PTDM_No_MetS, pdata$Events_PTDM_With_MetS)

# Data for MACE plot (q)
qdata <- data.frame(
  Count = c(0, 1, 2, 3, 4, 5),
  Followup_PTDM = c(900.572602739726, 2310.8602739726, 2590.81917808219, 1894.4, 874.630136986301, 189.369863013699),
  Events_PTDM = c(9, 28, 74, 62, 49, 14),
  Events_per_100_PTDM = c(0.999364179258858, 1.2116699705026, 2.85623947151639, 3.27280405405405, 5.6023681242952, 7.3929398148148)
)

pval_PTDM <- prop.trend.test(qdata$Events_PTDM, qdata$Followup_PTDM, score = seq_along(qdata$Events_PTDM))

# Convert data to long format
qdata_long <- tidyr::pivot_longer(qdata, 
                                  cols = c(Events_per_100_PTDM), 
                                  names_to = "Group",
                                  values_to = "Events_per_100")

# Add "followup" column
qdata_long$Followup <- ifelse(grepl("MACE", qdata_long$Group), qdata$Followup_PTDM)
qdata_long$Events <- ifelse(grepl("MACE", qdata_long$Group), qdata$Events_PTDM)

## PLOTS

# Create barplot using ggplot2
p <- ggplot(pdata_long, aes(x = factor(Quartile), y = Events_per_100, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Quartile",
       y = "Events per 100 patient-years",
       fill = NULL) +
  scale_fill_manual(values = c("gray50", "gray80"),
                    labels = c("Without MetS", "With MetS")) +
  theme_minimal() +
  theme(legend.position = "none") + 
  geom_text(aes(label = paste(round(Followup, 0), "y\n", Events, "cs.\n", round(Events_per_100, 1), "")),
            position = position_dodge(width = 1),
            vjust = -0.2, lineheight = 0.9, size=2.5) +
  ylim(0, 6) +
  ggtitle("Plot of PTDM incidence proportions\nby vitamin D-25 quartile") +
  xlab("Vitamin D-25 quartile (range, nmol/L)") + ylab("New diagnoses per 100 patient-years") +
  geom_segment(aes(x = 1, xend = 4, y = 5.3, yend = 5.3), color = "black", linetype="solid") +
  geom_segment(aes(x = 1, xend = 1, y = 5.3, yend = 5.15), color = "black", linetype="solid") +
  geom_segment(aes(x = 4, xend = 4, y = 5.3, yend = 5.15), color = "black", linetype="solid") +
  geom_text(aes(x = 1, y = 5.9, hjust=0, label = paste("Test for trend in proportions:")),
            size = 3, color = "black") +
  geom_text(aes(x = 1, y = 5.5, hjust=0, label = paste("(No met. syndrome) p-value:", round(pval_PTDM_noMetS$p.value, 5))),
            size = 3, color = "grey50") +
  geom_text(aes(x = 1, y = 5.7, hjust=0, label = paste("(With met. syndrome) p-value:", round(pval_PTDM_withMetS$p.value, 5))),
            size = 3, color = "grey80")

p <- p + scale_x_discrete(labels=c('Q1 (7-32)', 'Q2 (32-48)', 'Q3 (48-68)', 'Q4 (68-172)'))

# Code for MetS criteria plot (q)
q <- ggplot(qdata_long, aes(x = factor(Count), y = Events_per_100, fill = Group)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Count",
       y = "Events per 100 patient-years",
       fill = NULL) +
  scale_fill_manual(values = c("gray50")) +
  theme_minimal() +
  theme(legend.position = "none") +
  geom_text(aes(label = paste(round(Followup_PTDM, 0), "y\n", Events_PTDM, "cs.\n", round(Events_per_100, 1), "")),
            position = position_dodge(width = 1),
            vjust = -0.2, lineheight = 0.9, size = 2.5) +
  ylim(0, 9) +
  ggtitle("Plot of PTDM incidence proportion\nby number of NCEP-ATP III criteria") +
  xlab("Number of NCEP-ATP III\ncriteria met (out of 5)") +
  ylab("New diagnoses per 100 patient-years") +
  geom_segment(aes(x = 1, xend = 6, y = 8.5, yend = 8.5), color = "black", linetype = "solid") +
  geom_segment(aes(x = 1, xend = 1, y = 8.5, yend = 8.35), color = "black", linetype = "solid") +
  geom_segment(aes(x = 6, xend = 6, y = 8.5, yend = 8.35), color = "black", linetype = "solid") +
  geom_text(aes(x = 1, y = 8.9, hjust=0, label = paste("Test for trend in proportions:\np-value:", round(pval_PTDM$p.value, 20))),
            size = 3, color = "grey50") 

# Combine plots side by side using patchwork
final_plot <- p + q
final_plot

ggsave("/Users/malyalar/Dropbox/Renal Transplant Research/vitd_mets_study/figures/png/2_barplots.png", final_plot, width = 7, height = 6, dpi = 900)
ggsave("/Users/malyalar/Dropbox/Renal Transplant Research/vitd_mets_study/figures/pdf/2_barplots.pdf", final_plot, width = 7, height = 6, dpi = 900)
