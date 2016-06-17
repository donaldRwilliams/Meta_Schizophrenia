library(brms)
prior_eff <- set_prior("normal(0,0.5)", class = "b")
prior_tau <- set_prior("cauchy(0,0.3)", class = "sd")
prior <- c(prior_eff, prior_tau)
iter <- 5000
options(mc.cores = 2)
control <- list(adapt_delta = 0.95)
theme_set(theme_bw())

# ---------- primary meta-analysis ----------------
## SMD
fit_SMD_post <- brm(SMD_post ~ 0 + intercept + (1|obs), 
                    data = cdata, autocor = cor_fixed(V_SMD_post), 
                    prior = prior, sample_prior = TRUE,
                    iter = iter, control = control)
fit_SMD_post
plot(fit_SMD_post, ask = FALSE)
(hyp_SMD_post <- hypothesis(fit_SMD_post, "intercept = 0"))
plot(hyp_SMD_post)

## SMCR
fit_SMCR <- brm(SMCR ~ 0 + intercept + (1|obs), 
                data = cdata, autocor = cor_fixed(V_SMCR), 
                prior = prior, sample_prior = TRUE,
                iter = iter, control = control)
fit_SMCR
plot(fit_SMCR, ask = FALSE)
(hyp_SMCR <- hypothesis(fit_SMCR, "intercept = 0"))
plot(hyp_SMCR)


# ------- moderator analysis ------
subgroups1 <- levels(cdata$subgroup_1)
subgroups2 <- levels(cdata$subgroup_2)
subgroups3 <- levels(cdata$subgroup_3)

## SMD
### subgroups
fit_SMD_sg1 <- brm(SMD_post ~ 0 + subgroup_1 + (1|obs), 
                   data = cdata, autocor = cor_fixed(V_SMD_post), 
                   prior = prior, sample_prior = TRUE,
                   iter = iter, control = control)
fit_SMD_sg1
plot(fit_SMD_sg1, ask = FALSE)
(hyp_SMD_sg1 <- hypothesis(fit_SMD_sg1, paste("subgroup_1", subgroups1, " = 0")))
plot(hyp_SMD_sg1, chars = NULL, ask = FALSE)
marginal_effects(fit_SMD_sg1)

fit_SMD_sg2 <- brm(SMD_post ~ 0 + subgroup_2 + (1|obs), 
                   data = cdata, autocor = cor_fixed(V_SMD_post), 
                   prior = prior, sample_prior = TRUE,
                   iter = iter, control = control)
fit_SMD_sg2
plot(fit_SMD_sg2, ask = FALSE)
(hyp_SMD_sg2 <- hypothesis(fit_SMD_sg2, paste("subgroup_2", subgroups2, " = 0")))
plot(hyp_SMD_sg2, chars = NULL, ask = FALSE, N = 6)
marginal_effects(fit_SMD_sg2)

fit_SMD_sg3 <- brm(SMD_post ~ 0 + subgroup_3 + (1|obs), 
                   data = cdata, autocor = cor_fixed(V_SMD_post), 
                   prior = prior, sample_prior = TRUE,
                   iter = iter, control = control)
fit_SMD_sg3
plot(fit_SMD_sg3, ask = FALSE)
(hyp_SMD_sg3 <- hypothesis(fit_SMD_sg3, paste("subgroup_3", subgroups3, " = 0")))
plot(hyp_SMD_sg3, chars = NULL, ask = FALSE, N = 6)
marginal_effects(fit_SMD_sg3)

### other moderators
fit_SMD_oxyAge <- brm(SMD_post ~ oxyAge + (1|obs), 
                      data = cdata, autocor = cor_fixed(V_SMD_post), 
                      prior = prior_tau, sample_prior = TRUE,
                      iter = iter, control = control)
fit_SMD_oxyAge

fit_SMD_plaAge <- update(fit_SMD_oxyAge, formula. = ~ . - oxyAge + plaAge,
                         newdata = cdata, control = control)
fit_SMD_plaAge

fit_SMD_oxyMale <- update(fit_SMD_oxyAge, formula. = ~ . - oxyAge + oxyMale,
                         newdata = cdata, control = control)
fit_SMD_oxyMale

fit_SMD_plaMale <- update(fit_SMD_oxyAge, formula. = ~ . - oxyAge + plaMale,
                         newdata = cdata, control = control)
fit_SMD_plaMale

fit_SMD_training <- update(fit_SMD_oxyAge, formula. = ~ . - oxyAge + training,
                         newdata = cdata, control = control)
fit_SMD_training

fit_SMD_duration <- update(fit_SMD_oxyAge, formula. = ~ . - oxyAge + duration,
                         newdata = cdata, control = control)
fit_SMD_duration

fit_SMD_dailyDose <- update(fit_SMD_oxyAge, formula. = ~ . - oxyAge + dailyDose,
                         newdata = cdata, control = control)
fit_SMD_dailyDose

fit_SMD_eachDose <- update(fit_SMD_oxyAge, formula. = ~ . - oxyAge + eachDose,
                         newdata = cdata, control = control)
fit_SMD_eachDose

fit_SMD_admin_int <- update(fit_SMD_oxyAge, formula. = ~ . - oxyAge + admin_int,
                         newdata = cdata, control = control)
fit_SMD_admin_int

## SMCR
### subgroups
fit_SMCR_sg1 <- brm(SMCR ~ 0 + subgroup_1 + (1|obs), 
                    data = cdata, autocor = cor_fixed(V_SMCR), 
                    prior = prior, sample_prior = TRUE,
                    iter = iter, control = control)
fit_SMCR_sg1
plot(fit_SMCR_sg1, ask = FALSE)
(hyp_SMCR_sg1 <- hypothesis(fit_SMCR_sg1, paste("subgroup_1", subgroups1, " = 0")))
plot(hyp_SMCR_sg1, chars = NULL, ask = FALSE)
marginal_effects(fit_SMCR_sg1)

fit_SMCR_sg2 <- brm(SMCR ~ 0 + subgroup_2 + (1|obs), 
                    data = cdata, autocor = cor_fixed(V_SMCR), 
                    prior = prior, sample_prior = TRUE,
                    iter = iter, control = control)
fit_SMCR_sg2
plot(fit_SMCR_sg2, ask = FALSE)
(hyp_SMCR_sg2 <- hypothesis(fit_SMCR_sg2, paste("subgroup_2", subgroups2, " = 0")))
plot(hyp_SMCR_sg2, chars = NULL, ask = FALSE, N = 6)
marginal_effects(fit_SMCR_sg2)

fit_SMCR_sg3 <- brm(SMCR ~ 0 + subgroup_3 + (1|obs), 
                    data = cdata, autocor = cor_fixed(V_SMCR), 
                    prior = prior, sample_prior = TRUE,
                    iter = iter, control = control)
fit_SMCR_sg3
plot(fit_SMCR_sg3, ask = FALSE)
(hyp_SMCR_sg3 <- hypothesis(fit_SMCR_sg3, paste("subgroup_3", subgroups3, " = 0")))
plot(hyp_SMCR_sg3, chars = NULL, ask = FALSE, N = 6)
marginal_effects(fit_SMCR_sg3)

### other moderators
fit_SMCR_oxyAge <- brm(SMCR ~ oxyAge + (1|obs), 
                       data = cdata, autocor = cor_fixed(V_SMCR), 
                       prior = prior_tau, sample_prior = TRUE,
                       iter = iter, control = control)
fit_SMCR_oxyAge

fit_SMCR_plaAge <- update(fit_SMCR_oxyAge, formula. = ~ . - oxyAge + plaAge,
                          newdata = cdata, control = control)
fit_SMCR_plaAge

fit_SMCR_oxyMale <- update(fit_SMCR_oxyAge, formula. = ~ . - oxyAge + oxyMale,
                           newdata = cdata, control = control)
fit_SMCR_oxyMale

fit_SMCR_plaMale <- update(fit_SMCR_oxyAge, formula. = ~ . - oxyAge + plaMale,
                          newdata = cdata, control = control)
fit_SMCR_plaMale

fit_SMCR_training <- update(fit_SMCR_oxyAge, formula. = ~ . - oxyAge + training,
                           newdata = cdata, control = control)
fit_SMCR_training

fit_SMCR_duration <- update(fit_SMCR_oxyAge, formula. = ~ . - oxyAge + duration,
                           newdata = cdata, control = control)
fit_SMCR_duration

fit_SMCR_dailyDose <- update(fit_SMCR_oxyAge, formula. = ~ . - oxyAge + dailyDose,
                            newdata = cdata, control = control)
fit_SMCR_dailyDose

fit_SMCR_eachDose <- update(fit_SMCR_oxyAge, formula. = ~ . - oxyAge + eachDose,
                           newdata = cdata, control = control)
fit_SMCR_eachDose

fit_SMCR_admin_int <- update(fit_SMCR_oxyAge, formula. = ~ . - oxyAge + admin_int,
                            newdata = cdata, control = control)
fit_SMCR_admin_int


# ---------------- leave one out analysis ----------------
study_names <- levels(cdata$study)
fits_SMD_post <- fits_SMCR <-
  setNames(vector("list", length(study_names)), study_names)
for (i in seq_along(study_names)) {
  print(study_names[i])
  subdata <- droplevels(subset(cdata, study != study_names[i]))
  sub_V_SMD_post <- cov_matrix2(study_id = subdata$study, 
                                v = subdata$vSMD_post, r = 0.7)
  sub_V_SMCR <- cov_matrix2(study_id = subdata$study, 
                            v = subdata$vSMCR, r = 0.7)
  fits_SMD_post[[i]] <- update(fit_SMD_post, newdata = subdata,
                               autocor = cor_fixed(sub_V_SMD_post),
                               control = control)
  fits_SMCR[[i]] <- update(fit_SMCR, newdata = subdata,
                           autocor = cor_fixed(sub_V_SMCR), 
                           control = control)
}
fits_SMD_post
fits_SMCR


# ---------- vary between-outcome correlation -------------
rs <- c(0.1, 0.3, 0.5, 0.7, 0.9)
fits_SMD_post_r <- fits_SMCR_r <-  
  setNames(vector("list", length(rs)), rs)
for (i in seq_along(rs)) {
  V_SMD_post_r <- cov_matrix2(study_id = cdata$study, 
                              v = cdata$vSMD_post, r = rs[i])
  V_SMCR_r <- cov_matrix2(study_id = cdata$study,
                          v = cdata$vSMCR, r = rs[i])
  fits_SMD_post_r[[i]] <- update(fit_SMD_post, 
                                 autocor = cor_fixed(V_SMD_post_r),
                                 control = control)
  fits_SMCR_r[[i]] <- update(fit_SMCR, autocor = cor_fixed(V_SMCR_r),
                             control = control)
}
fits_SMD_post_r
fits_SMCR_r


# ------- publication bias ---------------
library(metafor)
tiff("cognitive_funnel_plots.tif", height=1100, width=850)
dcex <- 2
par(mfrow=c(4, 2), mar = c(5, 5, 2, 2) + 0.1)
# general_cog
funnel(rma_SMD_gcog <- rma(SMD_post ~ 1, vi = vSMD_post,
                          data = subset(cdata, subgroup_1 == "general_cog")), 
       xlab = "General cognitions: SMD", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMD_gcog, estimator = "L0")
funnel(rma_SMCR_gcog <- rma(SMCR ~ 1, vi = vSMCR,
                           data = subset(cdata, subgroup_1 == "general_cog")), 
       xlab = "General cognitions: SCMR", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMCR_gcog, estimator = "L0")
# social_cog
funnel(rma_SMD_scog <- rma(SMD_post ~ 1, vi = vSMD_post,
                          data = subset(cdata, subgroup_1 == "social_cog")), 
       xlab = "Social cognitions: SMD", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMD_scog, estimator = "L0")
funnel(rma_SMCR_scog <- rma(SMCR ~ 1, vi = vSMCR,
                            data = subset(cdata, subgroup_1 == "social_cog")), 
       xlab = "Social cognitions: SCMR", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMCR_scog, estimator = "L0")
# social_func
funnel(rma_SMD_sfunc <- rma(SMD_post ~ 1, vi = vSMD_post,
                          data = subset(cdata, subgroup_1 == "social_func")), 
       xlab = "Social functions: SMD", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMD_sfunc, estimator = "L0")
funnel(rma_SMCR_sfunc <- rma(SMCR ~ 1, vi = vSMCR,
                           data = subset(cdata, subgroup_1 == "social_func")), 
       xlab = "Social functions: SCMR", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMCR_sfunc, estimator = "L0")
# symptoms
funnel(rma_SMD_symp <- rma(SMD_post ~ 1, vi = vSMD_post,
                          data = subset(cdata, subgroup_1 == "symptoms")), 
       xlab = "Symptoms: SMD", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMD_symp, estimator = "L0")
funnel(rma_SMCR_symp <- rma(SMCR ~ 1, vi = vSMCR,
                           data = subset(cdata, subgroup_1 == "symptoms")), 
       xlab = "Symptoms: SCMR", cex = dcex, 
       cex.axis = dcex, cex.lab = dcex)
trimfill(rma_SMCR_symp, estimator = "L0")
par(mfrow=c(1,1), mar = c(5, 4, 4, 2) + 0.1)
dev.off()
