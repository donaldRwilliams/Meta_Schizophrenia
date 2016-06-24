library(brms)
prior <- c(set_prior("normal(0,0.5)", class = "b", 
                     coef = "SG1general_cog"),
           set_prior("normal(0,0.5)", class = "b", 
                     coef = "SG1social_cog"),
           set_prior("cauchy(0,0.3)", class = "sd"))
prior_sg2a3 <- c(set_prior("normal(0,0.5)"),
                 set_prior("cauchy(0,0.3)", class = "sd"))
iter <- 5000
options(mc.cores = 2)
control <- list(adapt_delta = 0.95)
theme_set(theme_bw())

# ---------- primary meta-analysis ----------------
sgs1 <- levels(cdata$SG1)
## SMD
fit_SMD_sg1 <- brm(SMD_post | se(sqrt(vSMD_post)) ~ 
                     0 + SG1 + (1|study) + (1|obs), 
                   data = cdata, prior = prior, sample_prior = TRUE,
                   iter = iter, control = control)
fit_SMD_sg1
plot(fit_SMD_sg1, ask = FALSE)
(hyp_SMD_sg1 <- hypothesis(fit_SMD_sg1, paste("SG1", sgs1, " = 0")))
plot(hyp_SMD_sg1, chars = NULL, ask = FALSE)
marginal_effects(fit_SMD_sg1)

## SMCR
fit_SMCR_sg1 <- brm(SMCR | se(sqrt(vSMCR)) ~ 
                      0 + SG1 + (1|study) + (1|obs), 
                    data = cdata, prior = prior, sample_prior = TRUE,
                    iter = iter, control = control)
fit_SMCR_sg1
plot(fit_SMCR_sg1, ask = FALSE)
(hyp_SMCR_sg1 <- hypothesis(fit_SMCR_sg1, paste("SG1", sgs1, " = 0")))
plot(hyp_SMCR_sg1, chars = NULL, ask = FALSE)
marginal_effects(fit_SMCR_sg1)


# ------- moderator analysis ------
sgs2 <- levels(cdata$SG2)
sgs3 <- levels(cdata$SG3)

## SMD
fit_SMD_sg2 <- brm(SMD_post | se(sqrt(vSMD_post)) ~ 
                     0 + SG2 + (1|study) + (1|obs), 
                   data = cdata, prior = prior_sg2a3, 
                   sample_prior = TRUE,
                   iter = iter, control = control)
fit_SMD_sg2
plot(fit_SMD_sg2, ask = FALSE)
(hyp_SMD_sg2 <- hypothesis(fit_SMD_sg2, paste("SG2", sgs2, " = 0")))
plot(hyp_SMD_sg2, chars = NULL, ask = FALSE)
marginal_effects(fit_SMD_sg2)

fit_SMD_sg3 <- brm(SMD_post | se(sqrt(vSMD_post)) ~ 
                     0 + SG3 + (1|study) + (1|obs), 
                   data = cdata, prior = prior_sg2a3, 
                   sample_prior = TRUE,
                   iter = iter, control = control)
fit_SMD_sg3
plot(fit_SMD_sg3, ask = FALSE)
(hyp_SMD_sg3 <- hypothesis(fit_SMD_sg3, paste("SG2", sgs3, " = 0")))
plot(hyp_SMD_sg3, chars = NULL, ask = FALSE)
marginal_effects(fit_SMD_sg3)

### other moderators
fit_SMD_oxyAge <- brm(SMD_post | se(sqrt(vSMD_post)) ~ 
                        0 + SG1 + SG1:oxyAge + (1|study) + (1|obs), 
                      data = cdata, prior = prior, sample_prior = TRUE,
                      iter = iter, control = control)
print(fit_SMD_oxyAge, 4)
plot(fit_SMD_oxyAge)
plot(marginal_effects(fit_SMD_oxyAge), points = TRUE)

fit_SMD_plaAge <- update(fit_SMD_oxyAge, 
                         formula. = ~ . - SG1:oxyAge + SG1:plaAge,
                         newdata = cdata, control = control)
print(fit_SMD_plaAge, 4)
plot(marginal_effects(fit_SMD_oxyAge), points = TRUE)

fit_SMD_oxyMale <- update(fit_SMD_oxyAge, 
                          formula. = ~ . - SG1:oxyAge + SG1:oxyMale,
                          newdata = cdata, control = control)
fit_SMD_oxyMale

fit_SMD_plaMale <- update(fit_SMD_oxyAge, 
                          formula. = ~ . - SG1:oxyAge + SG1:plaMale,
                          newdata = cdata, control = control)
fit_SMD_plaMale

fit_SMD_training <- update(fit_SMD_oxyAge, 
                          formula. = ~ . - SG1:oxyAge + SG1:training,
                          newdata = cdata, control = control)
fit_SMD_training

fit_SMD_duration <- update(fit_SMD_oxyAge, 
                           formula. = ~ . - SG1:oxyAge + SG1:duration,
                           newdata = cdata, control = control)
print(fit_SMD_duration, 4)
plot(marginal_effects(fit_SMD_duration), points = TRUE)

fit_SMD_dailyDose <- update(fit_SMD_oxyAge, 
                            formula. = ~ . - SG1:oxyAge + SG1:dailyDose,
                            newdata = cdata, control = control)
fit_SMD_dailyDose

fit_SMD_eachDose <- update(fit_SMD_oxyAge, 
                           formula. = ~ . - SG1:oxyAge + SG1:eachDose,
                           newdata = cdata, control = control)
fit_SMD_eachDose

fit_SMD_admin_int <- update(fit_SMD_oxyAge, 
                            formula. = ~ . - SG1:oxyAge + SG1:admin_int,
                            newdata = cdata, control = control)
fit_SMD_admin_int


## SMCR
fit_SMCR_sg2 <- brm(SMCR | se(sqrt(vSMCR)) ~ 
                     0 + SG2 + (1|study) + (1|obs), 
                   data = cdata, prior = prior_sg2a3, 
                   sample_prior = TRUE,
                   iter = iter, control = control)
fit_SMCR_sg2
plot(fit_SMCR_sg2, ask = FALSE)
(hyp_SMCR_sg2 <- hypothesis(fit_SMCR_sg2, paste("SG1", sgs2, " = 0")))
plot(hyp_SMCR_sg2, chars = NULL, ask = FALSE)
marginal_effects(fit_SMCR_sg2)

fit_SMCR_sg3 <- brm(SMCR | se(sqrt(vSMCR)) ~ 
                     0 + SG3 + (1|study) + (1|obs), 
                   data = cdata, prior = prior_sg2a3, 
                   sample_prior = TRUE,
                   iter = iter, control = control)
fit_SMCR_sg3
plot(fit_SMCR_sg3, ask = FALSE)
(hyp_SMCR_sg3 <- hypothesis(fit_SMCR_sg3, paste("SG1", sgs3, " = 0")))
plot(hyp_SMCR_sg3, chars = NULL, ask = FALSE)
marginal_effects(fit_SMCR_sg3)

### other moderators
fit_SMCR_oxyAge <- brm(SMCR | se(sqrt(vSMCR)) ~ 
                        0 + SG1 + SG1:oxyAge + (1|study) + (1|obs), 
                      data = cdata, prior = prior, sample_prior = TRUE,
                      iter = iter, control = control)
print(fit_SMCR_oxyAge, 4)
plot(fit_SMCR_oxyAge)
plot(marginal_effects(fit_SMCR_oxyAge), points = TRUE)

fit_SMCR_plaAge <- update(fit_SMCR_oxyAge, 
                         formula. = ~ . - SG1:oxyAge + SG1:plaAge,
                         newdata = cdata, control = control)
print(fit_SMCR_plaAge, 4)
plot(marginal_effects(fit_SMCR_oxyAge), points = TRUE)

fit_SMCR_oxyMale <- update(fit_SMCR_oxyAge, 
                          formula. = ~ . - SG1:oxyAge + SG1:oxyMale,
                          newdata = cdata, control = control)
fit_SMCR_oxyMale

fit_SMCR_plaMale <- update(fit_SMCR_oxyAge, 
                          formula. = ~ . - SG1:oxyAge + SG1:plaMale,
                          newdata = cdata, control = control)
fit_SMCR_plaMale

fit_SMCR_training <- update(fit_SMCR_oxyAge, 
                           formula. = ~ . - SG1:oxyAge + SG1:training,
                           newdata = cdata, control = control)
fit_SMCR_training

fit_SMCR_duration <- update(fit_SMCR_oxyAge, 
                           formula. = ~ . - SG1:oxyAge + SG1:duration,
                           newdata = cdata, control = control)
print(fit_SMCR_duration, 4)
plot(marginal_effects(fit_SMCR_duration), points = TRUE)

fit_SMCR_dailyDose <- update(fit_SMCR_oxyAge, 
                            formula. = ~ . - SG1:oxyAge + SG1:dailyDose,
                            newdata = cdata, control = control)
fit_SMCR_dailyDose

fit_SMCR_eachDose <- update(fit_SMCR_oxyAge, 
                           formula. = ~ . - SG1:oxyAge + SG1:eachDose,
                           newdata = cdata, control = control)
fit_SMCR_eachDose

fit_SMCR_admin_int <- update(fit_SMCR_oxyAge, 
                            formula. = ~ . - SG1:oxyAge + SG1:admin_int,
                            newdata = cdata, control = control)
fit_SMCR_admin_int


# ---------------- leave one out analysis ----------------
study_names <- levels(cdata$study)
fits_SMD_post <- fits_SMCR <-
  setNames(vector("list", length(study_names)), study_names)
for (i in seq_along(study_names)) {
  print(study_names[i])
  subdata <- droplevels(subset(cdata, study != study_names[i]))
  fits_SMD_post[[i]] <- update(fit_SMD_post, newdata = subdata,
                               control = control)
  fits_SMCR[[i]] <- update(fit_SMCR, newdata = subdata,
                           control = control)
}
fits_SMD_post
fits_SMCR


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
