# ------- metafor models used in the plots -------
library(metafor)
rma_SMD_social <- rma(SMD ~ 1, vi = vSMD, data = scdata)
rma_SMCR_social <- rma(SMCR ~ 1, vi = vSMCR, data = scdata)
rma_SMD_neuro <- rma(SMD ~ 1, vi = vSMD, data = ncdata)
rma_SMCR_neuro <- rma(SMCR ~ 1, vi = vSMCR, data = ncdata)

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

## neuro cognition
study_names_neuro <- ifelse(duplicated(ncdata$study), "", 
                              as.character(ncdata$study))
tiff("forest_SMD_neuro.tif", height=400, width=1000)
forest(rma_SMD_neuro, addfit = FALSE, xlab = "SMD", 
       cex.lab = cex.lab, cex.axis = cex, cex = cex, alim = c(-2, 2),
       slab = study_names_neuro)
text(-2.5, 13, "Authors (Year)", cex = cex)
text(2, 13, "SMD", cex = cex)
text(2.45, 13, "[95%-CI]", cex = cex)
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
