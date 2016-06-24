# ------- publication bias ---------------
library(metafor)
tiff("cognitive_funnel_plots.tif", height=1100, width=850)
dcex <- 2
par(mfrow=c(4, 2), mar = c(5, 5, 2, 2) + 0.1)
# general_cog
funnel(rma_SMD_gcog <- rma(SMD_post ~ 1, vi = vSMD_post,
                           data = subset(cdata, SG1 == "general_cog")), 
       xlab = "General cognitions: SMD", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMD_gcog, estimator = "L0")
funnel(rma_SMCR_gcog <- rma(SMCR ~ 1, vi = vSMCR,
                            data = subset(cdata, SG1 == "general_cog")), 
       xlab = "General cognitions: SCMR", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMCR_gcog, estimator = "L0")
# social_cog
funnel(rma_SMD_scog <- rma(SMD_post ~ 1, vi = vSMD_post,
                           data = subset(cdata, SG1 == "social_cog")), 
       xlab = "Social cognitions: SMD", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMD_scog, estimator = "L0")
funnel(rma_SMCR_scog <- rma(SMCR ~ 1, vi = vSMCR,
                            data = subset(cdata, SG1 == "social_cog")), 
       xlab = "Social cognitions: SCMR", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMCR_scog, estimator = "L0")
# social_func
funnel(rma_SMD_sfunc <- rma(SMD_post ~ 1, vi = vSMD_post,
                            data = subset(cdata, SG1 == "social_func")), 
       xlab = "Social functions: SMD", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMD_sfunc, estimator = "L0")
funnel(rma_SMCR_sfunc <- rma(SMCR ~ 1, vi = vSMCR,
                             data = subset(cdata, SG1 == "social_func")), 
       xlab = "Social functions: SCMR", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMCR_sfunc, estimator = "L0")
# symptoms
funnel(rma_SMD_symp <- rma(SMD_post ~ 1, vi = vSMD_post,
                           data = subset(cdata, SG1 == "symptoms")), 
       xlab = "Symptoms: SMD", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMD_symp, estimator = "L0")
funnel(rma_SMCR_symp <- rma(SMCR ~ 1, vi = vSMCR,
                            data = subset(cdata, SG1 == "symptoms")), 
       xlab = "Symptoms: SCMR", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMCR_symp, estimator = "L0")
par(mfrow=c(1,1), mar = c(5, 4, 4, 2) + 0.1)
dev.off()