# ------- metafor models used in the plots -------
library(metafor)
rma_SMD_social <- rma(SMD_post ~ 1, vi = vSMD_post, data = scdata)
rma_SMCR_social <- rma(SMCR ~ 1, vi = vSMCR, data = scdata)
rma_SMD_general <- rma(SMD_post ~ 1, vi = vSMD_post, data = gcdata)
rma_SMCR_general <- rma(SMCR ~ 1, vi = vSMCR, data = gcdata)

# ------- forest plots ---------
cex <- 1.4
cex.lab <- 2

## social cognition
study_names_social <- ifelse(duplicated(scdata$study), "", 
                             as.character(scdata$study))
tiff("forest_SMD_social.tif", height=1600, width=1000)
forest(rma_SMD_social, addfit = FALSE, xlab = "SMD", 
       cex.lab = cex.lab, cex.axis = cex, cex = 1.2,
       slab = study_names_social)
text(-6.3, 74, "Authors (Year)", cex = cex)
text(5.0, 74, "SMD", cex = cex)
text(6.0, 74, "[95%-CI]", cex = cex)
dev.off()

## general cognition
study_names_general <- ifelse(duplicated(gcdata$study), "", 
                              as.character(gcdata$study))
tiff("forest_SMD_general.tif", height=400, width=1000)
forest(rma_SMD_general, addfit = FALSE, xlab = "SMD", 
       cex.lab = cex.lab, cex.axis = cex, cex = cex, alim = c(-2, 2),
       slab = study_names_general)
text(-2.5, 13, "Authors (Year)", cex = cex)
text(2, 13, "SMD", cex = cex)
text(2.45, 13, "[95%-CI]", cex = cex)
dev.off()


# ------- publication bias ---------------
tiff("cognitive_funnel_plots.tif", height=550, width=850)
dcex <- 2
par(mfrow=c(2, 2), mar = c(5, 5, 2, 2) + 0.1)
# social_cog
funnel(rma_SMD_social, xlab = "Social cognitions: SMD", 
       cex = dcex, cex.axis = dcex, cex.lab = dcex)
regtest(rma_SMD_social, model = "lm", predictor = "sei")
trimfill(rma_SMD_social, estimator = "R0")
funnel(rma_SMCR_social, xlab = "Social cognitions: SCMR", 
       cex = dcex, cex.axis = dcex, cex.lab = dcex)
regtest(rma_SMCR_social, model = "lm", predictor = "sei")
trimfill(rma_SMCR_social, estimator = "R0")
# general_cog
funnel(rma_SMD_general, xlab = "General cognitions: SMD", 
       cex = dcex, cex.axis = dcex, cex.lab = dcex)
regtest(rma_SMD_general, model = "lm", predictor = "sei")
trimfill(rma_SMD_general, estimator = "R0")
funnel(rma_SMCR_general, xlab = "General cognitions: SCMR", 
       cex = dcex, cex.axis = dcex, cex.lab = dcex)
regtest(rma_SMCR_General, model = "lm", predictor = "sei")
trimfill(rma_SMCR_general, estimator = "R0")
par(mfrow=c(1,1), mar = c(5, 4, 4, 2) + 0.1)
dev.off()