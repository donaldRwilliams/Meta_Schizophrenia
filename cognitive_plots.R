# ------- forest plots ---------
cex <- 1.4
cex.lab <- 2

## social cognition
study_names_social <- ifelse(duplicated(scdata$study), "", 
                             as.character(scdata$study))
tiff("forest_SMD_social.tif", height=1600, width=1000)
rma_SMD_social <- rma(SMD_post ~ 1, vi = vSMD_post, data = scdata)
forest(rma_SMD_social, addfit = FALSE, xlab = "Social Cognition: SMD", 
       cex.lab = cex.lab, cex.axis = cex, cex = cex,
       slab = study_names_social)
text(-6.3, 73, "Authors, Year", cex = cex)
text(4.6, 73, "SMD", cex = cex)
text(5.6, 73, "[95%-CI]", cex = cex)
dev.off()

## general cognition
study_names_general <- ifelse(duplicated(gcdata$study), "", 
                              as.character(gcdata$study))
tiff("forest_SMD_general.tif", height=400, width=1000)
rma_SMD_general <- rma(SMD_post ~ 1, vi = vSMD_post, data = gcdata)
forest(rma_SMD_general, addfit = FALSE, xlab = "General Cognition: SMD", 
       cex.lab = cex.lab, cex.axis = cex, cex = cex, alim = c(-2, 2),
       slab = study_names_general)
text(-2.75, 13, "Authors, Year", cex = cex)
text(2.35, 13, "SMD", cex = cex)
text(2.85, 13, "[95%-CI]", cex = cex)
dev.off()


# ------- publication bias ---------------
library(metafor)
tiff("cognitive_funnel_plots.tif", height=550, width=850)
dcex <- 2
par(mfrow=c(2, 2), mar = c(5, 5, 2, 2) + 0.1)
# social_cog
funnel(rma_SMD_social <- rma(SMD_post ~ 1, vi = vSMD_post, data = scdata), 
       xlab = "Social cognitions: SMD", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMD_social, estimator = "L0")
funnel(rma_SMCR_social <- rma(SMCR ~ 1, vi = vSMCR, data = scdata), 
       xlab = "Social cognitions: SCMR", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMCR_social, estimator = "L0")
# general_cog
funnel(rma_SMD_general <- rma(SMD_post ~ 1, vi = vSMD_post, data = gcdata), 
       xlab = "General cognitions: SMD", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMD_general, estimator = "L0")
funnel(rma_SMCR_general <- rma(SMCR ~ 1, vi = vSMCR, data = gcdata), 
       xlab = "General cognitions: SCMR", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMCR_general, estimator = "L0")
par(mfrow=c(1,1), mar = c(5, 4, 4, 2) + 0.1)
dev.off()