library(brms)
prior <- c(set_prior("normal(0,0.25)", class = "sd"),
           set_prior("normal(0,0.5)"))
conditions <- data.frame(study = unique(sdata$study))
rownames(conditions) <- unique(sdata$study)

fit_SMD_post <- brm(SMD_post ~ 0 + sympType + (0+sympType|study), 
                    data = sdata, autocor = cor_fixed(V_SMD_post), 
                    prior = prior, sample_prior = TRUE)
fit_SMD_post
plot(fit_SMD_post, ask = FALSE)
(hyp_SMD_post <- hypothesis(fit_SMD_post, paste("sympType", c("pos", "neg", "gen"), " = 0")))
plot(hyp_SMD_post)
marginal_effects(fit_SMD_post)
plot(marginal_effects(fit_SMD_post, conditions = conditions, 
                      re_formula = NULL), points = TRUE, ncol = 4) + theme_bw()

fit_SMCR <- brm(SMCR ~ 0 + sympType + (0+sympType|study), data = sdata,
                autocor = cor_fixed(V_SMCR), prior = prior,
                sample_prior = TRUE)
fit_SMCR
plot(fit_SMCR, ask = FALSE)
(hyp_SMCR <- hypothesis(fit_SMCR, paste("sympType", c("pos", "neg", "gen"), " = 0")))
plot(hyp_SMCR)
marginal_effects(fit_SMCR)
plot(marginal_effects(fit_SMCR, conditions = conditions, 
                      re_formula = NULL), points = TRUE, ncol = 4)

# test SMCR model without random effects
fit_SMCR2 <- brm(SMCR ~ 0 + sympType, data = sdata,
                 autocor = cor_fixed(V_SMCR), prior = prior,
                 sample_prior = TRUE)
fit_SMCR2
plot(fit_SMCR2, ask = FALSE)
marginal_effects(fit_SMCR2)
LOO(fit_SMCR, fit_SMCR2)