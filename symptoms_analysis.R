library(brms)
prior <- c(set_prior("cauchy(0,0.3)", class = "sd"),
           set_prior("normal(0,0.5)"))
iter <- 5000
options(mc.cores = 2)
control <- list(adapt_delta = 0.95)
symp_type <- unique(sdata$sympType)
conditions <- data.frame(study = unique(sdata$study))
rownames(conditions) <- unique(sdata$study)
theme_set(theme_bw())


# ---------------- primary (multivariate) meta-analysis ----------------
## SMD
fit_SMD_post <- brm(SMD_post ~ 0 + sympType + (0+sympType|study), 
                    data = sdata, autocor = cor_fixed(V_SMD_post), 
                    prior = prior, sample_prior = TRUE, iter = iter,
                    control = control)
fit_SMD_post
plot(fit_SMD_post, ask = FALSE)
(hyp_SMD_post <- hypothesis(fit_SMD_post, paste("sympType", symp_type, " = 0")))
plot(hyp_SMD_post)
marginal_effects(fit_SMD_post)
plot(marginal_effects(fit_SMD_post, conditions = conditions, 
                      re_formula = NULL), points = TRUE, ncol = 4)

## SMCR
fit_SMCR <- brm(SMCR ~ 0 + sympType + (0+sympType|study), data = sdata,
                autocor = cor_fixed(V_SMCR), prior = prior,
                sample_prior = TRUE, iter = iter, control = control)
fit_SMCR
plot(fit_SMCR, ask = FALSE)
(hyp_SMCR <- hypothesis(fit_SMCR, paste("sympType", symp_type, " = 0")))
plot(hyp_SMCR)
marginal_effects(fit_SMCR)
plot(marginal_effects(fit_SMCR, conditions = conditions, 
                      re_formula = NULL), points = TRUE, ncol = 4)


# ---------------- analysis of overall symptoms ---------------- 
fit_SMD_post_ove <- brm(SMD_post | se(sqrt(vSMD_post)) ~ 0 + intercept + (1|study), 
                        data = tdata, prior = prior, sample_prior = TRUE,
                        iter = iter, control = control)
fit_SMD_post_ove
(hyp_SMD_post_ove <- hypothesis(fit_SMD_post_ove, "intercept = 0"))
plot(hyp_SMD_post_ove)

fit_SMCR_ove <- brm(SMCR | se(sqrt(vSMCR)) ~ 0 + intercept  + (1|study), 
                    data = tdata, prior = prior, sample_prior = TRUE,
                    iter = iter, control = control)
fit_SMCR_ove
(hyp_SMCR_ove <- hypothesis(fit_SMCR_ove, "intercept = 0"))
plot(hyp_SMCR_ove)


# ---------------- moderator analyses ---------------- 
## assuming the same effects across symptom types
## SMD
fit_SMD_oxyAge <- brm(SMD_post ~ 0 + sympType + oxyAge + (0+sympType|study), 
                      data = sdata, autocor = cor_fixed(V_SMD_post), 
                      prior = prior, sample_prior = TRUE, iter = iter,
                      control = control)
fit_SMD_oxyAge

fit_SMD_plaAge <- update(fit_SMD_oxyAge, formula. = ~ . + plaAge - oxyAge,
                         newdata = sdata, control = control)
fit_SMD_plaAge

fit_SMD_oxyMale <- update(fit_SMD_oxyAge, formula. = ~ . + oxyMale - oxyAge,
                          newdata = sdata, control = control)
fit_SMD_oxyMale

fit_SMD_plaMale <- update(fit_SMD_oxyAge, formula. = ~ . + plaMale - oxyAge,
                          newdata = sdata, control = control)
fit_SMD_plaMale

fit_SMD_training <- update(fit_SMD_oxyAge, formula. = ~ . + training - oxyAge,
                           newdata = sdata, control = control)
fit_SMD_training

fit_SMD_duration <- update(fit_SMD_oxyAge, formula. = ~ . + duration - oxyAge,
                           newdata = sdata, control = control)
fit_SMD_duration

fit_SMD_dailyDose <- update(fit_SMD_oxyAge, formula. = ~ . + dailyDose - oxyAge,
                            newdata = sdata, control = control)
fit_SMD_dailyDose

fit_SMD_eachDose <- update(fit_SMD_oxyAge, formula. = ~ . + eachDose - oxyAge,
                           newdata = sdata, control = control)
fit_SMD_eachDose

fit_SMD_admin_int <- update(fit_SMD_oxyAge, formula. = ~ . + admin_int - oxyAge,
                            newdata = sdata, control = control)
fit_SMD_admin_int

# SMCR
fit_SMCR_oxyAge <- brm(SMCR ~ 0 + sympType + oxyAge + (0+sympType|study), 
                      data = sdata, autocor = cor_fixed(V_SMCR), 
                      prior = prior, sample_prior = TRUE, iter = iter,
                      control = control)
fit_SMCR_oxyAge

fit_SMCR_plaAge <- update(fit_SMCR_oxyAge, formula. = ~ . + plaAge - oxyAge,
                          newdata = sdata, control = control)
fit_SMCR_plaAge

fit_SMCR_oxyMale <- update(fit_SMCR_oxyAge, formula. = ~ . + oxyMale - oxyAge,
                           newdata = sdata, control = control)
fit_SMCR_oxyMale

fit_SMCR_plaMale <- update(fit_SMCR_oxyAge, formula. = ~ . + plaMale - oxyAge,
                           newdata = sdata, control = control)
fit_SMCR_plaMale

fit_SMCR_training <- update(fit_SMCR_oxyAge, formula. = ~ . + training - oxyAge,
                            newdata = sdata, control = control)
fit_SMCR_training

fit_SMCR_duration <- update(fit_SMCR_oxyAge, formula. = ~ . + duration - oxyAge,
                            newdata = sdata, control = control)
fit_SMCR_duration

fit_SMCR_dailyDose <- update(fit_SMCR_oxyAge, formula. = ~ . + dailyDose - oxyAge,
                             newdata = sdata, control = control)
fit_SMCR_dailyDose

fit_SMCR_eachDose <- update(fit_SMCR_oxyAge, formula. = ~ . + eachDose - oxyAge,
                            newdata = sdata, control = control)
fit_SMCR_eachDose

fit_SMCR_admin_int <- update(fit_SMCR_oxyAge, formula. = ~ . + admin_int - oxyAge,
                             newdata = sdata, control = control)
fit_SMCR_admin_int


# ---------------- leave one out analysis ----------------
study_names <- levels(sdata$study)
fits_SMD_post <- fits_SMCR <-
  setNames(vector("list", length(study_names)), study_names)
for (i in seq_along(study_names)) {
  print(study_names[i])
  subdata <- droplevels(subset(sdata, study != study_names[i]))
  sub_V_SMD_post <- cov_matrix(study_id = subdata$study, 
                               out_id = subdata$sympType,
                               v = subdata$vSMD_post, R = cor_mat)
  sub_V_SMCR <- cov_matrix(study_id = subdata$study, 
                           out_id = subdata$sympType,
                           v = subdata$vSMCR, R = cor_mat)
  fits_SMD_post[[i]] <- update(fit_SMD_post, newdata = subdata,
                               autocor = cor_fixed(sub_V_SMD_post), 
                               control = control)
  fits_SMCR[[i]] <- update(fit_SMCR, newdata = subdata,
                           autocor = cor_fixed(sub_V_SMCR),
                           control = control)
}

# ---------------- publication bias ----------------
library(metafor)
tiff("symptoms_funnel_plots.tif", height=1100, width=850)
dcex <- 2
par(mfrow=c(4, 2), mar = c(5, 5, 2, 2) + 0.1)
# positive
funnel(rma_SMD_pos <- rma(SMD_post ~ 1, vi = vSMD_post,
                          data = subset(sdata, sympType == "positive")), 
       xlab = "Positive symptoms: SMD", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMD_pos, estimator = "L0")
funnel(rma_SMCR_pos <- rma(SMCR ~ 1, vi = vSMCR,
                           data = subset(sdata, sympType == "positive")), 
       xlab = "Positive symptoms: SCMR", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMCR_pos, estimator = "L0")
# negative
funnel(rma_SMD_neg <- rma(SMD_post ~ 1, vi = vSMD_post,
                          data = subset(sdata, sympType == "negative")), 
       xlab = "Negative symptoms: SMD", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMD_neg, estimator = "L0")
funnel(rma_SMCR_neg <- rma(SMCR ~ 1, vi = vSMCR,
                           data = subset(sdata, sympType == "negative")), 
       xlab = "Negative symptoms: SCMR", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMCR_neg, estimator = "L0")
# general
funnel(rma_SMD_gen <- rma(SMD_post ~ 1, vi = vSMD_post,
                          data = subset(sdata, sympType == "general")), 
       xlab = "General symptoms: SMD", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMD_gen, estimator = "L0")
funnel(rma_SMCR_gen <- rma(SMCR ~ 1, vi = vSMCR,
                           data = subset(sdata, sympType == "general")), 
       xlab = "General symptoms: SCMR", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMCR_gen, estimator = "L0")
# overall
funnel(rma_SMD_ove <- rma(SMD_post ~ 1, vi = vSMD_post, data = tdata), 
       xlab = "Overall symptoms: SMD", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMD_ove, estimator = "L0")
funnel(rma_SMCR_ove <- rma(SMCR ~ 1, vi = vSMCR, data = tdata), 
       xlab = "General symptoms: SCMR", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMCR_ove, estimator = "L0")
par(mfrow=c(1,1), mar = c(5, 4, 4, 2) + 0.1)
dev.off()
