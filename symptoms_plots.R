library(metafor)

# ---------------- compute sub MAs with metafor -------------
rma_SMD_pos <- rma(SMD_post ~ 1, vi = vSMD_post, data = pos_data,
                   slab = pos_data$study)
rma_SMD_neg <- rma(SMD_post ~ 1, vi = vSMD_post, data = neg_data,
                   slab = neg_data$study)
rma_SMD_gen <- rma(SMD_post ~ 1, vi = vSMD_post, data = gen_data,
                   slab = gen_data$study)
rma_SMD_ove <- rma(SMD_post ~ 1, vi = vSMD_post, data = ove_data,
                   slab = ove_data$study)

rma_SMCR_pos <- rma(SMCR ~ 1, vi = vSMCR, data = pos_data,
                    slab = pos_data$study)
rma_SMCR_neg <- rma(SMCR ~ 1, vi = vSMCR, data = neg_data,
                    slab = neg_data$study)
rma_SMCR_gen <- rma(SMCR ~ 1, vi = vSMCR, data = gen_data,
                    slab = gen_data$study)
rma_SMCR_ove <- rma(SMCR ~ 1, vi = vSMCR, data = ove_data,
                    slab = ove_data$study)

# ---------------- forest plots --------------
cex <- 1.4
cex.lab <- 2
## SMD
tiff("forest_SMD_post.tif", height=1500, width=1000)
# layout(matrix(1:4), heights = c(6, 8, 5, 7))
par(mfrow = c(4, 1), cex = cex, mar = c(4, 2, 0, 2) + 0.1)
forest(rma_SMD_pos, addfit = FALSE, xlab = "Positive Symptoms", 
       cex.lab = cex.lab)
text(-4.8, 7.5, "Authors, Year", cex = cex)
text(2.8, 7.5, "SMD", cex = cex)
text(4.0, 7.5, "[95%-CI]", cex = cex)
forest(rma_SMD_neg, addfit = FALSE, xlab = "Negative symptoms",
       cex.lab = cex.lab)
text(-3.8, 9.7, "Authors, Year", cex = cex)
text(3.0, 9.7, "SMD", cex = cex)
text(4, 9.7, "[95%-CI]", cex = cex)
forest(rma_SMD_gen, addfit = FALSE, xlab = "General psychopathodology",
       cex.lab = cex.lab)
text(-4, 6.5, "Authors, Year", cex = cex)
text(2.3, 6.5, "SMD", cex = cex)
text(3.3, 6.5, "[95%-CI]", cex = cex)
forest(rma_SMD_ove, addfit = FALSE, xlab = "Overall symptoms",
       cex.lab = cex.lab)
text(-4.9, 8.6, "Authors, Year", cex = cex)
text(3.4, 8.6, "SMD", cex = cex)
text(4.7, 8.6, "[95%-CI]", cex = cex)
dev.off()

## SMCR
tiff("forest_SMCR.tif", height=1500, width=1000)
# layout(matrix(1:4), heights = c(6, 8, 5, 7))
par(mfrow = c(4, 1), cex = cex, mar = c(4, 2, 0, 2) + 0.1)
forest(rma_SMCR_pos, addfit = FALSE, xlab = "Positive Symptoms", 
       cex.lab = cex.lab)
text(-4.1, 7.5, "Authors, Year", cex = cex)
text(2.6, 7.5, "SMCR", cex = cex)
text(3.7, 7.5, "[95%-CI]", cex = cex)
forest(rma_SMCR_neg, addfit = FALSE, xlab = "Negative symptoms",
       cex.lab = cex.lab)
text(-3.9, 9.7, "Authors, Year", cex = cex)
text(2.2, 9.7, "SMCR", cex = cex)
text(3.3, 9.7, "[95%-CI]", cex = cex)
forest(rma_SMCR_gen, addfit = FALSE, xlab = "General psychopathodology",
       cex.lab = cex.lab)
text(-4, 6.5, "Authors, Year", cex = cex)
text(2.8, 6.5, "SMCR", cex = cex)
text(3.9, 6.5, "[95%-CI]", cex = cex)
forest(rma_SMCR_ove, addfit = FALSE, xlab = "Overall symptoms",
       cex.lab = cex.lab)
text(-4.0, 8.6, "Authors, Year", cex = cex)
text(2.5, 8.6, "SMCR", cex = cex)
text(3.7, 8.6, "[95%-CI]", cex = cex)
dev.off()


# ---------------- publication bias ----------------
tiff("symptoms_funnel_plots.tif", height=1000, width=800)
dcex <- 2
par(mfrow=c(4, 2), mar = c(5, 5, 2, 2) + 0.1)
# positive
funnel(rma_SMD_pos, xlab = "Positive symptoms: SMD", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMD_pos, estimator = "L0")
funnel(rma_SMCR_pos, xlab = "Positive symptoms: SCMR", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMCR_pos, estimator = "L0")
# negative
funnel(rma_SMD_neg, xlab = "Negative symptoms: SMD", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMD_neg, estimator = "L0")
funnel(rma_SMCR_neg, xlab = "Negative symptoms: SCMR", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMCR_neg, estimator = "L0")
# general
funnel(rma_SMD_gen, xlab = "General symptoms: SMD", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMD_gen, estimator = "L0")
funnel(rma_SMCR_gen, xlab = "General symptoms: SCMR", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMCR_gen, estimator = "L0")
# overall
funnel(rma_SMD_ove, xlab = "Overall symptoms: SMD", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMD_ove, estimator = "L0")
funnel(rma_SMCR_ove, xlab = "General symptoms: SCMR", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMCR_ove, estimator = "L0")
par(mfrow=c(1,1), mar = c(5, 4, 4, 2) + 0.1)
dev.off()
