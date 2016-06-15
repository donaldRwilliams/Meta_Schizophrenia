library(brms)
prior <- c(set_prior("cauchy(0,0.3)", class = "sd"),
           set_prior("normal(0,0.5)"))
theme_set(theme_bw())

# ---------- primary meta-analysis ----------------
## SMD
fit_SMD_post <- brm(SMD_post ~ 0 + intercept + (1|obs), 
                    data = cdata, autocor = cor_fixed(V_SMD_post), 
                    prior = prior, sample_prior = TRUE)
fit_SMD_post
plot(fit_SMD_post, ask = FALSE)
(hyp_SMD_post <- hypothesis(fit_SMD_post, "intercept = 0"))
plot(hyp_SMD_post)

## SMCR
fit_SMCR <- brm(SMCR ~ 0 + intercept + (1|obs), 
                data = cdata, autocor = cor_fixed(V_SMCR), 
                prior = prior, sample_prior = TRUE,
                inits = 0)
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
                   prior = prior, sample_prior = TRUE)
fit_SMD_sg1
plot(fit_SMD_sg1, ask = FALSE)
(hyp_SMD_sg1 <- hypothesis(fit_SMD_sg1, paste("subgroup_1", subgroups1, " = 0")))
plot(hyp_SMD_sg1, chars = NULL, ask = FALSE)
marginal_effects(fit_SMD_sg1)

fit_SMD_sg2 <- brm(SMD_post ~ 0 + subgroup_2 + (1|obs), 
                   data = cdata, autocor = cor_fixed(V_SMD_post), 
                   prior = prior, sample_prior = TRUE)
fit_SMD_sg2
plot(fit_SMD_sg2, ask = FALSE)
(hyp_SMD_sg2 <- hypothesis(fit_SMD_sg2, paste("subgroup_2", subgroups2, " = 0")))
plot(hyp_SMD_sg2, chars = NULL, ask = FALSE, N = 6)
marginal_effects(fit_SMD_sg2)

fit_SMD_sg3 <- brm(SMD_post ~ 0 + subgroup_3 + (1|obs), 
                        data = cdata, autocor = cor_fixed(V_SMD_post), 
                        prior = prior, sample_prior = TRUE)
fit_SMD_sg3
plot(fit_SMD_sg3, ask = FALSE)
(hyp_SMD_sg3 <- hypothesis(fit_SMD_sg3, paste("subgroup_3", subgroups3, " = 0")))
plot(hyp_SMD_sg3, chars = NULL, ask = FALSE, N = 6)
marginal_effects(fit_SMD_sg3)

### other moderators
fit_SMD_oxyAge <- brm(SMD_post ~ oxyAge + (1|obs), 
                      data = cdata, autocor = cor_fixed(V_SMD_post), 
                      prior = prior, sample_prior = TRUE)
fit_SMD_oxyAge

fit_SMD_plaAge <- update(fit_SMD_oxyAge, formula. = ~ . - oxyAge + plaAge,
                         newdata = cdata)
fit_SMD_plaAge

fit_SMD_oxyMale <- update(fit_SMD_oxyAge, formula. = ~ . - oxyAge + oxyMale,
                         newdata = cdata)
fit_SMD_oxyMale

fit_SMD_plaMale <- update(fit_SMD_oxyAge, formula. = ~ . - oxyAge + plaMale,
                         newdata = cdata)
fit_SMD_plaMale

fit_SMD_training <- update(fit_SMD_oxyAge, formula. = ~ . - oxyAge + training,
                         newdata = cdata)
fit_SMD_training

fit_SMD_duration <- update(fit_SMD_oxyAge, formula. = ~ . - oxyAge + duration,
                         newdata = cdata)
fit_SMD_duration

fit_SMD_dailyDose <- update(fit_SMD_oxyAge, formula. = ~ . - oxyAge + dailyDose,
                         newdata = cdata)
fit_SMD_dailyDose

fit_SMD_eachDose <- update(fit_SMD_oxyAge, formula. = ~ . - oxyAge + eachDose,
                         newdata = cdata)
fit_SMD_eachDose

fit_SMD_admin_int <- update(fit_SMD_oxyAge, formula. = ~ . - oxyAge + admin_int,
                         newdata = cdata)
fit_SMD_admin_int

## SMCR
### subgroups
fit_SMCR_sg1 <- brm(SMCR ~ 0 + subgroup_1 + (1|obs), 
                        data = cdata, autocor = cor_fixed(V_SMCR), 
                        prior = prior, sample_prior = TRUE)
fit_SMCR_sg1
plot(fit_SMCR_sg1, ask = FALSE)
(hyp_SMCR_sg1 <- hypothesis(fit_SMCR_sg1, paste("subgroup_1", subgroups1, " = 0")))
plot(hyp_SMCR_sg1, chars = NULL, ask = FALSE)
marginal_effects(fit_SMCR_sg1)

fit_SMCR_sg2 <- brm(SMCR ~ 0 + subgroup_2 + (1|obs), 
                    data = cdata, autocor = cor_fixed(V_SMCR), 
                    prior = prior, sample_prior = TRUE)
fit_SMCR_sg2
plot(fit_SMCR_sg2, ask = FALSE)
(hyp_SMCR_sg2 <- hypothesis(fit_SMCR_sg2, paste("subgroup_2", subgroups2, " = 0")))
plot(hyp_SMCR_sg2, chars = NULL, ask = FALSE, N = 6)
marginal_effects(fit_SMCR_sg2)

fit_SMCR_sg3 <- brm(SMCR ~ 0 + subgroup_3 + (1|obs), 
                    data = cdata, autocor = cor_fixed(V_SMCR), 
                    prior = prior, sample_prior = TRUE)
fit_SMCR_sg3
plot(fit_SMCR_sg3, ask = FALSE)
(hyp_SMCR_sg3 <- hypothesis(fit_SMCR_sg3, paste("subgroup_3", subgroups3, " = 0")))
plot(hyp_SMCR_sg3, chars = NULL, ask = FALSE, N = 6)
marginal_effects(fit_SMCR_sg3)

### other moderators
fit_SMCR_oxyAge <- brm(SMCR ~ oxyAge + (1|obs), 
                      data = cdata, autocor = cor_fixed(V_SMCR), 
                      prior = prior, sample_prior = TRUE)
fit_SMCR_oxyAge

fit_SMCR_plaAge <- update(fit_SMCR_oxyAge, formula. = ~ . - oxyAge + plaAge,
                         newdata = cdata)
fit_SMCR_plaAge

fit_SMCR_oxyMale <- update(fit_SMCR_oxyAge, formula. = ~ . - oxyAge + oxyMale,
                          newdata = cdata)
fit_SMCR_oxyMale

fit_SMCR_plaMale <- update(fit_SMCR_oxyAge, formula. = ~ . - oxyAge + plaMale,
                          newdata = cdata)
fit_SMCR_plaMale

fit_SMCR_training <- update(fit_SMCR_oxyAge, formula. = ~ . - oxyAge + training,
                           newdata = cdata)
fit_SMCR_training

fit_SMCR_duration <- update(fit_SMCR_oxyAge, formula. = ~ . - oxyAge + duration,
                           newdata = cdata)
fit_SMCR_duration

fit_SMCR_dailyDose <- update(fit_SMCR_oxyAge, formula. = ~ . - oxyAge + dailyDose,
                            newdata = cdata)
fit_SMCR_dailyDose

fit_SMCR_eachDose <- update(fit_SMCR_oxyAge, formula. = ~ . - oxyAge + eachDose,
                           newdata = cdata)
fit_SMCR_eachDose

fit_SMCR_admin_int <- update(fit_SMCR_oxyAge, formula. = ~ . - oxyAge + admin_int,
                            newdata = cdata)
fit_SMCR_admin_int
