# ------- publication bias ---------------
library(metafor)
tiff("cognitive_funnel_plots.tif", height=550, width=850)
dcex <- 2
par(mfrow=c(2, 2), mar = c(5, 5, 2, 2) + 0.1)
# social_cog
funnel(rma_SMD_scog <- rma(SMD_post ~ 1, vi = vSMD_post,
                           data = subset(cdata, SG1 == "social_cog")), 
       xlab = "Social cognitions: SMD", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMD_scog, estimator = "L0")
funnel(rma_SMCR_scog <- rma(SMCR ~ 1, vi = vSMCR,
                            data = subset(cdata, SG1 == "social_cog")), 
       xlab = "Social cognitions: SMCR", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMCR_scog, estimator = "L0")
# general_cog
funnel(rma_SMD_gcog <- rma(SMD_post ~ 1, vi = vSMD_post,
                           data = subset(cdata, SG1 == "general_cog")), 
       xlab = "General cognitions: SMD", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMD_gcog, estimator = "L0")
funnel(rma_SMCR_gcog <- rma(SMCR ~ 1, vi = vSMCR,
                            data = subset(cdata, SG1 == "general_cog")), 
       xlab = "General cognitions: SMCR", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMCR_gcog, estimator = "L0")
par(mfrow=c(1,1), mar = c(5, 4, 4, 2) + 0.1)
dev.off()
