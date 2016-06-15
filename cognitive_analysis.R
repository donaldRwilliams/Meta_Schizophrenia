library(brms)
prior <- c(set_prior("cauchy(0,0.3)", class = "sd"),
           set_prior("normal(0,0.5)"))
theme_set(theme_bw())

# don't use multivariate analysis for now
# hedges' g estimates
fit_SMD_post <- brm(SMD_post ~ 0 + intercept + (1|obs), 
                    data = cdata, autocor = cor_fixed(V_SMD_post), 
                    prior = prior, sample_prior = TRUE)
fit_SMD_post
plot(fit_SMD_post, ask = FALSE)
(hyp_SMD_post <- hypothesis(fit_SMD_post, "intercept = 0"))
plot(hyp_SMD_post)

# SMCR estimates
fit_SMCR <- brm(SMCR ~ 0 + intercept + (1|obs), 
                data = cdata, autocor = cor_fixed(V_SMCR), 
                prior = prior, sample_prior = TRUE,
                inits = 0)
fit_SMCR
plot(fit_SMCR, ask = FALSE)
(hyp_SMCR <- hypothesis(fit_SMCR, "intercept = 0"))
plot(hyp_SMCR)


# ------- moderator analysis ------
# start with subgroups
subgroups1 <- levels(cdata$subgroup_1)
subgroups2 <- levels(cdata$subgroup_2)
subgroups3 <- levels(cdata$subgroup_3)

# Hedges' g
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

# SMCR
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
