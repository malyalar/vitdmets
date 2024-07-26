using<-function(...) {
  libs<-unlist(list(...))
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  if(length(need)>0){ 
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
}

using("survminer","survMisc","ggpubr", "gridtext", "patchwork")

dplots <- list()

dplots[[1]] <- ggsurvplot(survfit(Surv(survTimePTDM_v3, PTDMStatus_v3)~VitDmed,
                                  # set exclusion criteria in subsetting function
                                  # version selection in initial Surv() (i.e. survTimePTDM_v1/2/3) will also make a difference
                                  data=txdata_bw1_rv1[txdata_bw1_rv1$pretx_DM_v2 == 0 & txdata_bw1_rv1$NCEPATPnoRx=='no',]),
                          pval = T,
                          ggtheme = theme_pubr() + theme(text = element_text(size = 10)),
                          # set color palette, legend labels, and linetypes
                          palette=c("black", "black"),
                          legend.labs=c("below\nmed", "above\nmed"),
                          linetype=c("solid","dashed"),
                          # set test.for.trend true if wanting to consider ordinal variables
                          test.for.trend = F,
                          risk.table = T, cumevents = T, 
                          break.time.by=300, censor=F, 
                          # change titles and x/y axis labels
                          xlab="Time in days",
                          ylab="PTDM-free survival",
                          title="Time to post-transplant PTDM", 
                          subtitle="In patients without NCEP-ATP-III\nmetabolic syndrome.",
                          legend.title = "25[OH]-D\nmed = 48 nM",
                          size = 0.5,
                          # shift position of the p-value text box
                          xlim=c(0,1800), ylim=c(0.72,1), pval.coord=c(10, 0.73), 
                          conf.int=T, conf.int.alpha=0.1, 
                          # risk table parameters. See for more options: http://rpkgs.datanovia.com/survminer/reference/ggsurvplot.html#legend-title-labels-and-position 
                          tables.theme = theme_minimal(),
                          risk.table.height=0.2,
                          risk.table.fontsize=3,
                          cumevents.height=0.2,
                          tables.y.text=TRUE)

dplots[[2]] <- ggsurvplot(survfit(Surv(survTimePTDM_v3, PTDMStatus_v3)~VitDmed,
                                  # set exclusion criteria in subsetting function
                                  # version selection in initial Surv() (i.e. survTimePTDM_v1/2/3) will also make a difference
                                  data=txdata_bw1_rv1[txdata_bw1_rv1$pretx_DM_v2 == 0 & txdata_bw1_rv1$NCEPATPnoRx=='yes',]),
                          pval = T,
                          ggtheme = theme_pubr() + theme(text = element_text(size = 10)),
                          # set color palette, legend labels, and linetypes
                          palette=c("black", "black"),
                          legend.labs=c("below\nmed", "above\nmed"),
                          linetype=c("solid","dashed"),
                          # set test.for.trend true if wanting to consider ordinal variables
                          test.for.trend = F,
                          risk.table = T, cumevents = T, 
                          break.time.by=300, censor=F, 
                          # change titles and x/y axis labels
                          xlab="Time in days",
                          ylab="PTDM-free survival",
                          title="Time to post-transplant PTDM", 
                          subtitle="In patients with NCEP-ATP-III\nmetabolic syndrome.",
                          legend.title = "25[OH]-D\nmed = 48 nM",
                          size = 0.5,
                          # shift position of the p-value text box
                          xlim=c(0,1800), ylim=c(0.72,1), pval.coord=c(10, 0.73), 
                          conf.int=T, conf.int.alpha=0.1, 
                          # risk table parameters. See for more options: http://rpkgs.datanovia.com/survminer/reference/ggsurvplot.html#legend-title-labels-and-position 
                          tables.theme = theme_minimal(),
                          risk.table.height=0.2,
                          risk.table.fontsize=3,
                          cumevents.height=0.2,
                          tables.y.text=TRUE)



dplots[[1]] $cumevents$layers[[1]]$aes_params$size <- 3
dplots[[2]] $cumevents$layers[[1]]$aes_params$size <- 3

survplots <- arrange_ggsurvplots(dplots, print = TRUE, ncol = 2, nrow = 1, risk.table.height = 0.2, labels=c("A","B"))
ggsave("PTDM_KM_curves_med.pdf", device="pdf", path="figures/pdf", plot=survplots, units="in", width=8, height=7, dpi=900)
ggsave("PTDM_KM_curves_med.png", device="png", path="figures/png", plot=survplots, units="in", width=8, height=7, dpi=900)

