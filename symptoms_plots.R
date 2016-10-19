library(metafor)

# ---------------- new forest plots --------------
library(forestplot)
## SMCR
forest_data_symp <- data.frame(
  mean = c(NA, NA, NA, neg_data$SMCR, NA, -0.06, 
           NA, NA, pos_data$SMCR, NA, -0.16,
           NA, NA, gen_data$SMCR, NA, -0.24,
           NA, NA, ove_data$SMCR, NA, -0.19),
  # using 0 instead of NA in lower and upper avoids a strange bug
  lower = c(0, 0, 0, neg_data$SMCR - 1.96 * sqrt(neg_data$vSMCR), 0, -0.34,
            0, 0, pos_data$SMCR - 1.96 * sqrt(pos_data$vSMCR), 0, -0.49,
            0, 0, gen_data$SMCR - 1.96 * sqrt(gen_data$vSMCR), 0, -0.59,
            0, 0, ove_data$SMCR - 1.96 * sqrt(ove_data$vSMCR), 0, -0.55),
  higher = c(0, 0, 0, neg_data$SMCR + 1.96 * sqrt(neg_data$vSMCR), 0, 0.22,
             0, 0, pos_data$SMCR + 1.96 * sqrt(pos_data$vSMCR), 0, 0.19,
             0, 0, gen_data$SMCR + 1.96 * sqrt(gen_data$vSMCR), 0, 0.14,
             0, 0, ove_data$SMCR + 1.96 * sqrt(ove_data$vSMCR), 0, 0.18)
)

label_text_symp <- cbind(
  c("Authors, Year", 
    NA, NA, as.character(neg_data$study), NA, "Summary negative symptoms",
    NA, NA, as.character(pos_data$study), NA, "Summary positive symptoms",
    NA, NA, as.character(gen_data$study), NA, "Summary general psychopathology",
    NA, NA, as.character(ove_data$study), NA, "Summary overall symptoms"),
  c("SMCR", 
    NA, NA, format(round(neg_data$SMCR, 2), nsmall = 2), NA, "-0.06",
    NA, NA, format(round(pos_data$SMCR, 2), nsmall = 2), NA, "-0.16",
    NA, NA, format(round(gen_data$SMCR, 2), nsmall = 2), NA, "-0.24",
    NA, NA, format(round(ove_data$SMCR, 2), nsmall = 2), NA, "-0.19")
)
is_summary <- c(TRUE, rep(FALSE, nrow(neg_data) + 3), TRUE, 
                rep(FALSE, nrow(pos_data) + 3), TRUE,
                rep(FALSE, nrow(gen_data) + 3), TRUE,
                rep(FALSE, nrow(ove_data) + 3), TRUE)

tiff("symptoms_forest_SMCR.tif", height=1650, width=1200)
forestplot(label_text_symp, forest_data_symp,
           align = c("l", "l", "r"), is.summary = is_summary, 
           txt_gp = fpTxtGp(label = gpar(cex = 2),
                            ticks = gpar(cex = 1.8)),
           ci.vertices = TRUE, ci.vertices.height = .25,
           hrzl_lines = list("3" = gpar(lty=2), 
                             "13" = gpar(lwd=1, columns=1:3, col = "#000044"),
                             "23" = gpar(lwd=1, columns=1:3, col = "#000044"),
                             "32" = gpar(lwd=1, columns=1:3, col = "#000044"),
                             "43" = gpar(lwd=1, columns=1:3, col = "#000044")),
           col = fpColors(box = "black", line = "darkblue", 
                          summary = "black"))
dev.off()

## SMD
forest_data_symp <- data.frame(
  mean = c(NA, NA, NA, neg_data$SMD_post, NA, -0.02, 
           NA, NA, pos_data$SMD_post, NA, -0.23,
           NA, NA, gen_data$SMD_post, NA, -0.19,
           NA, NA, ove_data$SMD_post, NA, -0.13),
  # using 0 instead of NA in lower and upper avoids a strange bug
  lower = c(0, 0, 0, neg_data$SMD_post - 1.96 * sqrt(neg_data$vSMD_post), 0, -0.32,
            0, 0, pos_data$SMD_post - 1.96 * sqrt(pos_data$vSMD_post), 0, -0.71,
            0, 0, gen_data$SMD_post - 1.96 * sqrt(gen_data$vSMD_post), 0, -0.53,
            0, 0, ove_data$SMD_post - 1.96 * sqrt(ove_data$vSMD_post), 0, -0.53),
  higher = c(0, 0, 0, neg_data$SMD_post + 1.96 * sqrt(neg_data$vSMD_post), 0, 0.28,
             0, 0, pos_data$SMD_post + 1.96 * sqrt(pos_data$vSMD_post), 0, 0.27,
             0, 0, gen_data$SMD_post + 1.96 * sqrt(gen_data$vSMD_post), 0, 0.17,
             0, 0, ove_data$SMD_post + 1.96 * sqrt(ove_data$vSMD_post), 0, 0.30)
)

label_text_symp <- cbind(
  c("Authors, Year", 
    NA, NA, as.character(neg_data$study), NA, "Summary negative symptoms",
    NA, NA, as.character(pos_data$study), NA, "Summary positive symptoms",
    NA, NA, as.character(gen_data$study), NA, "Summary general psychopathology",
    NA, NA, as.character(ove_data$study), NA, "Summary overall symptoms"),
  c("SMD", 
    NA, NA, format(round(neg_data$SMD_post, 2), nsmall = 2), NA, "-0.02",
    NA, NA, format(round(pos_data$SMD_post, 2), nsmall = 2), NA, "-0.23",
    NA, NA, format(round(gen_data$SMD_post, 2), nsmall = 2), NA, "-0.19",
    NA, NA, format(round(ove_data$SMD_post, 2), nsmall = 2), NA, "-0.13")
)
is_summary <- c(TRUE, rep(FALSE, nrow(neg_data) + 3), TRUE, 
                rep(FALSE, nrow(pos_data) + 3), TRUE,
                rep(FALSE, nrow(gen_data) + 3), TRUE,
                rep(FALSE, nrow(ove_data) + 3), TRUE)

tiff("symptoms_forest_SMD.tif", height=1650, width=1200)
forestplot(label_text_symp, forest_data_symp,
           align = c("l", "l", "r"), is.summary = is_summary, 
           txt_gp = fpTxtGp(label = gpar(cex = 2),
                            ticks = gpar(cex = 1.8)),
           ci.vertices = TRUE, ci.vertices.height = .25,
           hrzl_lines = list("3" = gpar(lty=2), 
                             "13" = gpar(lwd=1, columns=1:3, col = "#000044"),
                             "23" = gpar(lwd=1, columns=1:3, col = "#000044"),
                             "32" = gpar(lwd=1, columns=1:3, col = "#000044"),
                             "43" = gpar(lwd=1, columns=1:3, col = "#000044")),
           col = fpColors(box = "black", line = "darkblue", 
                          summary = "black"))
dev.off()


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


# ----------- old forest plots -----------
cex <- 1.4
cex.lab <- 2
## SMD
tiff("symptoms_forest_SMD.tif", height=1500, width=1000)
# layout(matrix(1:4), heights = c(6, 8, 5, 7))
par(mfrow = c(4, 1), cex = cex, mar = c(4, 2, 0, 2) + 0.1)
forest(rma_SMD_neg, addfit = FALSE, xlab = "Negative symptoms",
       cex.lab = cex.lab)
text(-3.8, 9.7, "Authors, Year", cex = cex)
text(3.0, 9.7, "SMD", cex = cex)
text(4, 9.7, "[95%-CI]", cex = cex)
forest(rma_SMD_pos, addfit = FALSE, xlab = "Positive symptoms", 
       cex.lab = cex.lab)
text(-4.8, 7.5, "Authors, Year", cex = cex)
text(2.8, 7.5, "SMD", cex = cex)
text(4.0, 7.5, "[95%-CI]", cex = cex)
forest(rma_SMD_gen, addfit = FALSE, xlab = "General psychopathology",
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
tiff("symptoms_forest_SMCR.tif", height=1500, width=1000)
# layout(matrix(1:4), heights = c(6, 8, 5, 7))
par(mfrow = c(4, 1), cex = cex, mar = c(4, 2, 0, 2) + 0.1)
forest(rma_SMCR_neg, addfit = FALSE, xlab = "Negative symptoms",
       cex.lab = cex.lab)
text(-3.9, 9.7, "Authors, Year", cex = cex)
text(2.2, 9.7, "SMCR", cex = cex)
text(3.3, 9.7, "[95%-CI]", cex = cex)
forest(rma_SMCR_pos, addfit = FALSE, xlab = "Positive symptoms", 
       cex.lab = cex.lab)
text(-4.1, 7.5, "Authors, Year", cex = cex)
text(2.6, 7.5, "SMCR", cex = cex)
text(3.7, 7.5, "[95%-CI]", cex = cex)
forest(rma_SMCR_gen, addfit = FALSE, xlab = "General psychopathology",
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
## SMCR
tiff("symptoms_funnel_SMCR.tif", height=500, width=800)
dcex <- 2
par(mfrow=c(2, 2), mar = c(5, 5, 2, 2) + 0.1)
# negative
funnel(rma_SMCR_neg, xlab = "Negative symptoms", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMCR_neg, estimator = "L0")
# positive
funnel(rma_SMCR_pos, xlab = "Positive symptoms", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMCR_pos, estimator = "L0")
# general
funnel(rma_SMCR_gen, xlab = "General psychopathology", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMCR_gen, estimator = "L0")
# overall
funnel(rma_SMCR_ove, xlab = "Overall symptoms", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMCR_ove, estimator = "L0")
par(mfrow=c(1,1), mar = c(5, 4, 4, 2) + 0.1)
dev.off()

## SMD
tiff("symptoms_funnel_SMD.tif", height=500, width=800)
dcex <- 2
par(mfrow=c(2, 2), mar = c(5, 5, 2, 2) + 0.1)
# negative
funnel(rma_SMD_neg, xlab = "Negative symptoms", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMD_neg, estimator = "L0")
# positive
funnel(rma_SMD_pos, xlab = "Positive symptoms", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMD_pos, estimator = "L0")
# general
funnel(rma_SMD_gen, xlab = "General psychopathology", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMD_gen, estimator = "L0")
# overall
funnel(rma_SMD_ove, xlab = "Overall symptoms", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMD_ove, estimator = "L0")
par(mfrow=c(1,1), mar = c(5, 4, 4, 2) + 0.1)
dev.off()