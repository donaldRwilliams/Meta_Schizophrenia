library(brms)
prior <- c(set_prior("normal(0,0.3)", class = "sd"),
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
