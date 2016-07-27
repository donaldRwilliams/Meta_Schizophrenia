# ------- metafor models used in the plots -------
library(metafor)
rma_SMD_social <- rma(SMD ~ 1, vi = vSMD, data = scdata)
rma_SMCR_social <- rma(SMCR ~ 1, vi = vSMCR, data = scdata)
rma_SMD_neuro <- rma(SMD ~ 1, vi = vSMD, data = ncdata)
rma_SMCR_neuro <- rma(SMCR ~ 1, vi = vSMCR, data = ncdata)

# -------- new forest plots --------
## social cognition complete
forest_data_social <- data.frame(
  mean = c(NA, NA, NA, scdata$SMD, NA, 0.04),
  # using 0 instead of NA in lower and upper avoids a strange bug
  lower = c(0, 0, 0, scdata$SMD - 1.96 * sqrt(scdata$vSMD), 0, -0.09),
  upper = c(0, 0, 0, scdata$SMD + 1.96 * sqrt(scdata$vSMD), 0, 0.15)
)
study_names_social <- ifelse(duplicated(scdata$study), "", 
                             as.character(scdata$study))
label_text_social <- cbind(
  c("Authors (year)", NA, NA, study_names_social, NA, "Summary"),
  c("Outcome", NA, NA, as.character(scdata$outcome), NA, NA),
  c("SMD", NA, NA, format(round(scdata$SMD, 2), nsmall = 2), NA, "0.04")
)
is_summary <- c(TRUE, rep(FALSE, nrow(scdata) + 3), TRUE, TRUE)

tiff("forest_SMD_social.tif", height=1650, width=1200)
forestplot(label_text_social, forest_data_social,
           align = c("l", "l", "r"), is.summary = is_summary, 
           txt_gp = fpTxtGp(label = gpar(cex = 1.5),
                            ticks = gpar(cex = 2)),
           ci.vertices = TRUE, ci.vertices.height = .25,
           hrzl_lines = list("3" = gpar(lty=2), 
                             "91" = gpar(lwd=1, columns=1:3, col = "#000044")),
           col = fpColors(box = "black", line = "darkblue", 
                          summary = "black"))
dev.off()

## neurocognition complete
forest_data_neuro <- data.frame(
  mean = c(NA, NA, NA, ncdata$SMD, NA, 0.09),
  # using 0 instead of NA in lower and upper avoids a strange bug
  lower = c(0, 0, 0, ncdata$SMD - 1.96 * sqrt(ncdata$vSMD), 0, -0.15),
  upper = c(0, 0, 0, ncdata$SMD + 1.96 * sqrt(ncdata$vSMD), 0, 0.33)
)
study_names_neuro <- ifelse(duplicated(ncdata$study), "", 
                             as.character(ncdata$study))
label_text_neuro <- cbind(
  c("Authors (year)", NA, NA, study_names_neuro, NA, "Summary"),
  c("Outcome", NA, NA, as.character(ncdata$outcome), NA, NA),
  c("SMD", NA, NA, format(round(ncdata$SMD, 2), nsmall = 2), NA, "0.09")
)
is_summary <- c(TRUE, rep(FALSE, nrow(ncdata) + 3), TRUE, TRUE)

tiff("forest_SMD_neuro.tif", height=400, width=1200)
forestplot(label_text_neuro, forest_data_neuro,
           align = c("l", "l", "r"), is.summary = is_summary, 
           txt_gp = fpTxtGp(label = gpar(cex = 1.5),
                            ticks = gpar(cex = 2)),
           ci.vertices = TRUE, ci.vertices.height = .25,
           hrzl_lines = list("3" = gpar(lty=2), 
                             "16" = gpar(lwd=1, columns=1:3, col = "#000044")),
           col = fpColors(box = "black", line = "darkblue", 
                          summary = "black"))
dev.off()

## social cognition level High vs. low 
scdata_low <- subset(scdata, level == "Low")
scdata_high <- subset(scdata, level == "High")
forest_data_slevel <- data.frame(
  mean = c(NA, NA, NA, scdata_low$SMD, NA, -0.03, 
           NA, NA, scdata_high$SMD, NA, 0.20),
  # using 0 instead of NA in lower and upper avoids a strange bug
  lower = c(0, 0, 0, scdata_low$SMD - 1.96 * sqrt(scdata_low$vSMD), 0, -0.15,
            0, 0, scdata_high$SMD - 1.96 * sqrt(scdata_high$vSMD), 0, 0.05),
  higher = c(0, 0, 0, scdata_low$SMD + 1.96 * sqrt(scdata_low$vSMD), 0, 0.08,
             0, 0, scdata_high$SMD + 1.96 * sqrt(scdata_high$vSMD), 0, 0.34)
)
study_names_low <- ifelse(duplicated(scdata_low$study), "", 
                          as.character(scdata_low$study))
study_names_high <- ifelse(duplicated(scdata_high$study), "", 
                           as.character(scdata_high$study))
label_text_slevel <- cbind(
  c("Authors (year)", NA, NA, study_names_low, NA, "Summary cognition level: Low",
    NA, NA, study_names_high, NA, "Summary cognition level: High"),
  c("Outcome", NA, NA, as.character(scdata_low$outcome), NA, NA, 
    NA, NA, as.character(scdata_high$outcome), NA, NA),
  c("SMD", NA, NA, format(round(scdata_low$SMD, 2), nsmall = 2), NA, "-0.03",
    NA, NA, format(round(scdata_high$SMD, 2), nsmall = 2), NA, "0.20")
)
nlow <- sum(scdata$level == "Low", na.rm = TRUE)
nhigh <- sum(scdata$level == "High", na.rm = TRUE)
is_summary <- c(TRUE, rep(FALSE, nlow + 3), TRUE, 
                rep(FALSE, nhigh + 3), TRUE)

tiff("forest_SMD_slevel.tif", height=1650, width=1200)
forestplot(label_text_slevel, forest_data_slevel,
           align = c("l", "l", "r"), is.summary = is_summary, 
           txt_gp = fpTxtGp(label = gpar(cex = 1.5),
                            ticks = gpar(cex = 2)),
           ci.vertices = TRUE, ci.vertices.height = .25,
           hrzl_lines = list("3" = gpar(lty=2), 
                             "53" = gpar(lwd=1, columns=1:3, col = "#000044"),
                             "84" = gpar(lwd=1, columns=1:3, col = "#000044")),
           col = fpColors(box = "black", line = "darkblue", 
                          summary = "black"))
dev.off()

## social cognition type (SG2) 
scdata_emr <- subset(scdata, SG2 == "emotionRec")
scdata_tom <- subset(scdata, SG2 == "theoryOfMind")
scdata_hob <- subset(scdata, SG2 == "hostileBias")
forest_data_stype <- data.frame(
  mean = c(NA, NA, NA, scdata_emr$SMD, NA, 0.02, 
           NA, NA, scdata_tom$SMD, NA, 0.12,
           NA, NA, scdata_hob$SMD, NA, -0.15),
  # using 0 instead of NA in lower and upper avoids a strange bug
  lower = c(0, 0, 0, scdata_emr$SMD - 1.96 * sqrt(scdata_emr$vSMD), 0, -0.12,
            0, 0, scdata_tom$SMD - 1.96 * sqrt(scdata_tom$vSMD), 0, -0.04,
            0, 0, scdata_hob$SMD - 1.96 * sqrt(scdata_hob$vSMD), 0, -0.46),
  higher = c(0, 0, 0, scdata_emr$SMD + 1.96 * sqrt(scdata_emr$vSMD), 0, 0.16,
             0, 0, scdata_tom$SMD + 1.96 * sqrt(scdata_tom$vSMD), 0, 0.25,
             0, 0, scdata_hob$SMD + 1.96 * sqrt(scdata_hob$vSMD), 0, 0.16)
)
study_names_emr <- ifelse(duplicated(scdata_emr$study), "", 
                          as.character(scdata_emr$study))
study_names_tom <- ifelse(duplicated(scdata_tom$study), "", 
                           as.character(scdata_tom$study))
study_names_hob <- ifelse(duplicated(scdata_hob$study), "", 
                          as.character(scdata_hob$study))
label_text_stype <- cbind(
  c("Authors (year)", NA, NA, study_names_emr, NA, "Summary emotion recognition",
    NA, NA, study_names_tom, NA, "Summary theory of mind",
    NA, NA, study_names_hob, NA, "Summary hostile bias"),
  c("Outcome", NA, NA, as.character(scdata_emr$outcome), NA, NA, 
    NA, NA, as.character(scdata_tom$outcome), NA, NA,
    NA, NA, as.character(scdata_hob$outcome), NA, NA),
  c("SMD", NA, NA, format(round(scdata_emr$SMD, 2), nsmall = 2), NA, "0.02",
    NA, NA, format(round(scdata_tom$SMD, 2), nsmall = 2), NA, "0.12",
    NA, NA, format(round(scdata_hob$SMD, 2), nsmall = 2), NA, "-0.15")
)
nemr <- sum(scdata$SG2 == "emotionRec", na.rm = TRUE)
ntom <- sum(scdata$SG2 == "theoryOfMind", na.rm = TRUE)
nhob <- sum(scdata$SG2 == "hostileBias", na.rm = TRUE)
is_summary <- c(TRUE, rep(FALSE, nemr + 3), TRUE, 
                rep(FALSE, ntom + 3), TRUE,
                rep(FALSE, nhob + 3), TRUE)

tiff("forest_SMD_stype.tif", height=1650, width=1200)
forestplot(label_text_stype, forest_data_stype,
           align = c("l", "l", "r"), is.summary = is_summary, 
           txt_gp = fpTxtGp(label = gpar(cex = 1.5),
                            ticks = gpar(cex = 2)),
           ci.vertices = TRUE, ci.vertices.height = .25,
           hrzl_lines = list("3" = gpar(lty=2), 
                             "41" = gpar(lwd=1, columns=1:3, col = "#000044"),
                             "82" = gpar(lwd=1, columns=1:3, col = "#000044"),
                             "92" = gpar(lwd=1, columns=1:3, col = "#000044")),
           col = fpColors(box = "black", line = "darkblue", 
                          summary = "black"))
dev.off()


# ------- old forest plots ---------
cex <- 1.4
cex.lab <- 2

## social cognition
study_names_social <- ifelse(duplicated(scdata$study), "", 
                             as.character(scdata$study))
tiff("forest_SMD_social_old.tif", height=1600, width=1000)
forest(rma_SMD_social, addfit = FALSE, xlab = "SMD", 
       cex.lab = cex.lab, cex.axis = cex, cex = 1.2,
       slab = study_names_social, xlim = c(-7, 4),
       ilab = as.character(scdata$outcome), ilab.xpos = -4.1,
       ilab.pos = 4)
text(-6.3, 74, "Authors (Year)", cex = cex)
text(-4.1, 74, "Outcome", cex = cex, pos = 4)
text(2.6, 74, "SMD", cex = cex)
text(3.4, 74, "[95%-CI]", cex = cex)
dev.off()

## neurocognition
study_names_neuro <- ifelse(duplicated(ncdata$study), "", 
                              as.character(ncdata$study))
tiff("forest_SMD_neuro_old.tif", height=400, width=1000)
forest(rma_SMD_neuro, addfit = FALSE, xlab = "SMD", 
       cex.lab = cex.lab, cex.axis = cex, cex = cex, 
       xlim = c(-3.3, 1.9), slab = study_names_neuro,
       ilab = as.character(ncdata$outcome), ilab.xpos = -2.1,
       ilab.pos = 4)
text(-2.97, 13, "Authors (Year)", cex = cex)
text(-2.1, 13, "Outcome", cex = cex, pos = 4)
text(1.17, 13, "SMD", cex = cex)
text(1.59, 13, "[95%-CI]", cex = cex)
dev.off()


# ------- publication bias ---------------
tiff("cognitive_funnel_plots.tif", height=550, width=850)
dcex <- 2
par(mfrow=c(2, 2), mar = c(5, 5, 2, 2) + 0.1)
# social_cog
funnel(rma_SMD_social, xlab = "Social cognition: SMD", 
       cex = dcex, cex.axis = dcex, cex.lab = dcex)
regtest(rma_SMD_social, model = "lm", predictor = "sei")
trimfill(rma_SMD_social, estimator = "R0")
funnel(rma_SMCR_social, xlab = "Social cognition: SCMR", 
       cex = dcex, cex.axis = dcex, cex.lab = dcex)
regtest(rma_SMCR_social, model = "lm", predictor = "sei")
trimfill(rma_SMCR_social, estimator = "R0")
# neuro_cog
funnel(rma_SMD_neuro, xlab = "Neurocognition: SMD", 
       cex = dcex, cex.axis = dcex, cex.lab = dcex)
regtest(rma_SMD_neuro, model = "lm", predictor = "sei")
trimfill(rma_SMD_neuro, estimator = "R0")
funnel(rma_SMCR_neuro, xlab = "Neurocognition: SCMR", 
       cex = dcex, cex.axis = dcex, cex.lab = dcex)
regtest(rma_SMCR_neuro, model = "lm", predictor = "sei")
trimfill(rma_SMCR_neuro, estimator = "R0")
par(mfrow=c(1,1), mar = c(5, 4, 4, 2) + 0.1)
dev.off()


# ------- moderator plots -------
mod_plot <- function(x, effect, weights = 1, xlab = NULL, 
                     ylab = NULL, ylim = NULL, bw = TRUE,
                     ...) {
  library(brms)
  stopifnot(is(x, "brmsfit"))
  stopifnot(length(effect) == 1L)
  me <- marginal_effects(x, effects = effect)
  attr(me[[1]], "points")$.WEIGHTS <- weights
  effects <- attributes(me[[1]])$effects
  if (!is.null(xlab)) xlab <- xlab(xlab)
  if (!is.null(ylab)) ylab <- ylab(ylab)
  if (!is.null(ylim)) ylim <- ylim(ylim)
  out <- plot(me, do_plot = FALSE, ...)[[1]]
  if (bw && grepl("geom_smooth", capture.output(out$layers[[1]])[1])) {
    # make sure that plots are black and white
    out <- out + geom_smooth(stat = "identity", colour = "black")
  }
  out + geom_point(aes_string(x = effects[1], y = ".RESP", size = ".WEIGHTS"), 
                   shape = 1, data = attr(me[[1]], "points"), 
                   inherit.aes = FALSE, show.legend = FALSE) +
    xlab + ylab + ylim
}

theme_set(theme_bw())

vSMD_level <- scdata[!is.na(scdata$level), "vSMD"]
weights <- 1/(0.18^2 + 0.05^2 + vSMD_level)
(pl_level <- mod_plot(fit_SMD_level, "level", weights = weights, 
                      xlab = "Cognition level", ylab = "SMD"))
tiff("cognition_level.tif", height=400, width=400)
print(pl_level)
dev.off()
