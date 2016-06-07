library(brms)
prior <- c(set_prior("normal(0,0.3)", class = "sd"),
           set_prior("normal(0,0.5)"))
symp_type <- unique(sdata$sympType)
conditions <- data.frame(study = unique(sdata$study))
rownames(conditions) <- unique(sdata$study)
theme_set(theme_bw())

# analyse hedges' g estimates
fit_SMD_post <- brm(SMD_post ~ 0 + sympType + (0+sympType|study), 
                    data = sdata, autocor = cor_fixed(V_SMD_post), 
                    prior = prior, sample_prior = TRUE)
fit_SMD_post
plot(fit_SMD_post, ask = FALSE)
(hyp_SMD_post <- hypothesis(fit_SMD_post, paste("sympType", symp_type, " = 0")))
plot(hyp_SMD_post)
marginal_effects(fit_SMD_post)
plot(marginal_effects(fit_SMD_post, conditions = conditions, 
                      re_formula = NULL), points = TRUE, ncol = 4)

# analyse SMCR estimates
fit_SMCR <- brm(SMCR ~ 0 + sympType + (0+sympType|study), data = sdata,
                autocor = cor_fixed(V_SMCR), prior = prior,
                sample_prior = TRUE)
fit_SMCR
plot(fit_SMCR, ask = FALSE)
(hyp_SMCR <- hypothesis(fit_SMCR, paste("sympType", symp_type, " = 0")))
plot(hyp_SMCR)
marginal_effects(fit_SMCR)
plot(marginal_effects(fit_SMCR, conditions = conditions, 
                      re_formula = NULL), points = TRUE, ncol = 4)

# moderator analyses assuming the same effects across symptom types
# hedges' g estimates
fit_SMD_oxyAge <- brm(SMD_post ~ 0 + sympType + oxyAge + (0+sympType|study), 
                    data = sdata, autocor = cor_fixed(V_SMD_post), 
                    prior = prior, sample_prior = TRUE)
fit_SMD_oxyAge

fit_SMD_plaAge <- update(fit_SMD_oxyAge, formula. = ~ . + plaAge - oxyAge,
                         newdata = sdata)
fit_SMD_plaAge

fit_SMD_oxyMale <- update(fit_SMD_oxyAge, formula. = ~ . + oxyMale - oxyAge,
                         newdata = sdata)
fit_SMD_oxyMale

fit_SMD_plaMale <- update(fit_SMD_oxyAge, formula. = ~ . + plaMale - oxyAge,
                          newdata = sdata)
fit_SMD_plaMale

fit_SMD_training <- update(fit_SMD_oxyAge, formula. = ~ . + training - oxyAge,
                          newdata = sdata)
fit_SMD_training

fit_SMD_duration <- update(fit_SMD_oxyAge, formula. = ~ . + duration - oxyAge,
                           newdata = sdata)
fit_SMD_duration

fit_SMD_dailyDose <- update(fit_SMD_oxyAge, formula. = ~ . + dailyDose - oxyAge,
                           newdata = sdata)
fit_SMD_dailyDose

fit_SMD_eachDose <- update(fit_SMD_oxyAge, formula. = ~ . + eachDose - oxyAge,
                           newdata = sdata)
fit_SMD_eachDose

fit_SMD_admin_int <- update(fit_SMD_oxyAge, formula. = ~ . + admin_int - oxyAge,
                           newdata = sdata)
fit_SMD_admin_int

# SMCR estimates
fit_SMCR_oxyAge <- brm(SMCR ~ 0 + sympType + oxyAge + (0+sympType|study), 
                      data = sdata, autocor = cor_fixed(V_SMCR), 
                      prior = prior, sample_prior = TRUE)
fit_SMCR_oxyAge

fit_SMCR_plaAge <- update(fit_SMCR_oxyAge, formula. = ~ . + plaAge - oxyAge,
                         newdata = sdata)
fit_SMCR_plaAge

fit_SMCR_oxyMale <- update(fit_SMCR_oxyAge, formula. = ~ . + oxyMale - oxyAge,
                          newdata = sdata)
fit_SMCR_oxyMale

fit_SMCR_plaMale <- update(fit_SMCR_oxyAge, formula. = ~ . + plaMale - oxyAge,
                          newdata = sdata)
fit_SMCR_plaMale

fit_SMCR_training <- update(fit_SMCR_oxyAge, formula. = ~ . + training - oxyAge,
                           newdata = sdata)
fit_SMCR_training

fit_SMCR_duration <- update(fit_SMCR_oxyAge, formula. = ~ . + duration - oxyAge,
                           newdata = sdata)
fit_SMCR_duration

fit_SMCR_dailyDose <- update(fit_SMCR_oxyAge, formula. = ~ . + dailyDose - oxyAge,
                            newdata = sdata)
fit_SMCR_dailyDose

fit_SMCR_eachDose <- update(fit_SMCR_oxyAge, formula. = ~ . + eachDose - oxyAge,
                           newdata = sdata)
fit_SMCR_eachDose

fit_SMCR_admin_int <- update(fit_SMCR_oxyAge, formula. = ~ . + admin_int - oxyAge,
                            newdata = sdata)
fit_SMCR_admin_int
