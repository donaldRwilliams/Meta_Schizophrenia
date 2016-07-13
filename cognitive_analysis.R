p_value <- function(x, alternative = c("two.sided", "greater", "less")) {
  # compute p-values based on analyses done with brms
  # Args:
  #   x: Either a brmsfit of a brmshypothesis object
  #   alternative: specifies the type of alternative hypothesis
  if (is(x, "brmshypothesis")) {
    hyps <- x$hypothesis
  } else if (is(x, "brmsfit")) {
    alternative <- match.arg(alternative)
    fixef <- fixef(x)
    sign <- sign(fixef[, 1])
    fixef <- rownames(fixef)
    if (alternative == "greater") {
      hyps <- paste(fixef, "> 0")
    } else if (alternative == "less") {
      hyps <- paste(fixef, "< 0")
    } else {
      hyps <- paste(fixef, ifelse(sign < 0, "<", ">"), "0") 
    }
    hyps <- hypothesis(x, hyps)$hypothesis
  } else {
    stop("invalid 'x' argument")
  }
  odds <- hyps$Evid.Ratio
  mult <- ifelse(is(x, "brmsfit") && alternative == "two.sided", 2, 1)
  out <- (1 - odds / (1 + odds)) * mult
  names(out) <- rownames(hyps)
  out
} 

library(brms)
prior <- c(set_prior("normal(0,0.5)", coef = "intercept"),
           set_prior("cauchy(0,0.3)", class = "sd"))
prior_sg2a3 <- c(set_prior("normal(0,0.5)"),
                 set_prior("cauchy(0,0.2)", class = "sd"))
iter <- 5000
options(mc.cores = 2)
control <- list(adapt_delta = 0.95)
theme_set(theme_bw())

# ---------- primary meta-analysis ----------------
## social cognition
### SMD
fit_SMD_social <- brm(SMD_post | se(sqrt(vSMD_post)) ~ 
                        0 + intercept + (1|study) + (1|obs), 
                      data = scdata, prior = prior, sample_prior = TRUE,
                      iter = iter, control = control)
fit_SMD_social
plot(fit_SMD_social, ask = FALSE)
(hyp_SMD_social <- hypothesis(fit_SMD_social, "intercept = 0"))
plot(hyp_SMD_social, chars = NULL, ask = FALSE)
p_value(fit_SMD_social)

### SMCR
fit_SMCR_social <- brm(SMCR | se(sqrt(vSMCR)) ~ 
                         0 + intercept + (1|study) + (1|obs), 
                      data = scdata, prior = prior, sample_prior = TRUE,
                      iter = iter, control = control)
fit_SMCR_social
plot(fit_SMCR_social, ask = FALSE)
(hyp_SMCR_social <- hypothesis(fit_SMCR_social, "intercept = 0"))
plot(hyp_SMCR_social, chars = NULL, ask = FALSE)
p_value(fit_SMCR_social)

## general cognition
### SMD
fit_SMD_general <- brm(SMD_post | se(sqrt(vSMD_post)) ~ 
                        0 + intercept + (1|study) + (1|obs), 
                      data = gcdata, prior = prior, sample_prior = TRUE,
                      iter = iter, control = control)
fit_SMD_general
plot(fit_SMD_general, ask = FALSE)
(hyp_SMD_general <- hypothesis(fit_SMD_general, "intercept = 0"))
plot(hyp_SMD_general, chars = NULL, ask = FALSE)
p_value(fit_SMD_general)

### SMCR
fit_SMCR_general <- brm(SMCR | se(sqrt(vSMCR)) ~ 
                         0 + intercept + (1|study) + (1|obs), 
                       data = gcdata, prior = prior, sample_prior = TRUE,
                       iter = iter, control = control)
fit_SMCR_general
plot(fit_SMCR_general, ask = FALSE)
(hyp_SMCR_general <- hypothesis(fit_SMCR_general, "intercept = 0"))
plot(hyp_SMCR_general, chars = NULL, ask = FALSE)
p_value(fit_SMCR_general)

# ------- moderator analysis ------
# only for social cognition data
sgs2 <- levels(scdata$SG2)
sgs3 <- levels(scdata$SG3)

## SMD
fit_SMD_sg2 <- brm(SMD_post | se(sqrt(vSMD_post)) ~ 
                     0 + SG2 + (1|study) + (1|obs), 
                   data = scdata, prior = prior_sg2a3, 
                   sample_prior = TRUE,
                   iter = iter, control = control)
fit_SMD_sg2
plot(fit_SMD_sg2, ask = FALSE)
(hyp_SMD_sg2 <- hypothesis(fit_SMD_sg2, paste("SG2", sgs2, " = 0")))
plot(hyp_SMD_sg2, chars = NULL, ask = FALSE, N = 6)
p_value(fit_SMD_sg2)
marginal_effects(fit_SMD_sg2)

fit_SMD_sg3 <- brm(SMD_post | se(sqrt(vSMD_post)) ~ 
                     0 + SG3 + (1|study) + (1|obs), 
                   data = scdata, prior = prior_sg2a3, 
                   sample_prior = TRUE,
                   iter = iter, control = control)
fit_SMD_sg3
plot(fit_SMD_sg3, ask = FALSE)
(hyp_SMD_sg3 <- hypothesis(fit_SMD_sg3, paste("SG3", sgs3, " = 0")))
plot(hyp_SMD_sg3, chars = NULL, ask = FALSE)
p_value(fit_SMD_sg3)
marginal_effects(fit_SMD_sg3)

### other moderators
fit_SMD_oxyAge <- brm(SMD_post | se(sqrt(vSMD_post)) ~ 
                        0 + intercept + oxyAge + (1|study) + (1|obs), 
                      data = scdata, prior = prior, sample_prior = TRUE,
                      iter = iter, control = control)
print(fit_SMD_oxyAge, 4)
plot(fit_SMD_oxyAge)
p_value(fit_SMD_oxyAge)
plot(marginal_effects(fit_SMD_oxyAge), points = TRUE)

fit_SMD_plaAge <- update(fit_SMD_oxyAge, 
                         formula. = ~ . - oxyAge + plaAge,
                         newdata = scdata, control = control)
print(fit_SMD_plaAge, 4)
p_value(fit_SMD_plaAge)
plot(marginal_effects(fit_SMD_plaAge), points = TRUE)

fit_SMD_oxyMale <- update(fit_SMD_oxyAge, 
                          formula. = ~ . - oxyAge + oxyMale,
                          newdata = scdata, control = control)
fit_SMD_oxyMale
p_value(fit_SMD_oxyMale)

fit_SMD_plaMale <- update(fit_SMD_oxyAge, 
                          formula. = ~ . - oxyAge + plaMale,
                          newdata = scdata, control = control)
fit_SMD_plaMale
p_value(fit_SMD_plaMale)

fit_SMD_training <- update(fit_SMD_oxyAge, 
                          formula. = ~ . - oxyAge + training,
                          newdata = scdata, control = control)
fit_SMD_training
p_value(fit_SMD_training)

fit_SMD_duration <- update(fit_SMD_oxyAge, 
                           formula. = ~ . - oxyAge + duration,
                           newdata = scdata, control = control)
print(fit_SMD_duration, 4)
p_value(fit_SMD_duration)
plot(marginal_effects(fit_SMD_duration), points = TRUE)

fit_SMD_dailyDose <- update(fit_SMD_oxyAge, 
                            formula. = ~ . - oxyAge + dailyDose,
                            newdata = scdata, control = control)
fit_SMD_dailyDose
p_value(fit_SMD_dailyDose)

fit_SMD_eachDose <- update(fit_SMD_oxyAge, 
                           formula. = ~ . - oxyAge + eachDose,
                           newdata = scdata, control = control)
fit_SMD_eachDose
p_value(fit_SMD_eachDose)

fit_SMD_admin_int <- update(fit_SMD_oxyAge, 
                            formula. = ~ . - oxyAge + admin_int,
                            newdata = scdata, control = control)
fit_SMD_admin_int
p_value(fit_SMD_admin_int)

## SMCR
fit_SMCR_sg2 <- brm(SMCR | se(sqrt(vSMCR)) ~ 
                     0 + SG2 + (1|study) + (1|obs), 
                   data = scdata, prior = prior_sg2a3, 
                   sample_prior = TRUE,
                   iter = iter, control = control)
fit_SMCR_sg2
plot(fit_SMCR_sg2, ask = FALSE)
(hyp_SMCR_sg2 <- hypothesis(fit_SMCR_sg2, paste("SG2", sgs2, " = 0")))
plot(hyp_SMCR_sg2, chars = NULL, ask = FALSE)
p_value(fit_SMCR_sg2)
marginal_effects(fit_SMCR_sg2)

fit_SMCR_sg3 <- brm(SMCR | se(sqrt(vSMCR)) ~ 
                     0 + SG3 + (1|study) + (1|obs), 
                   data = scdata, prior = prior_sg2a3, 
                   sample_prior = TRUE,
                   iter = iter, control = control)
fit_SMCR_sg3
plot(fit_SMCR_sg3, ask = FALSE)
(hyp_SMCR_sg3 <- hypothesis(fit_SMCR_sg3, paste("SG3", sgs3, " = 0")))
plot(hyp_SMCR_sg3, chars = NULL, ask = FALSE)
p_value(fit_SMCR_sg3)
marginal_effects(fit_SMCR_sg3)

### other moderators
fit_SMCR_oxyAge <- brm(SMCR | se(sqrt(vSMCR)) ~ 
                        0 + intercept + oxyAge + (1|study) + (1|obs), 
                      data = scdata, prior = prior, sample_prior = TRUE,
                      iter = iter, control = control)
print(fit_SMCR_oxyAge, 4)
plot(fit_SMCR_oxyAge)
p_value(fit_SMCR_oxyAge)
plot(marginal_effects(fit_SMCR_oxyAge), points = TRUE)

fit_SMCR_plaAge <- update(fit_SMCR_oxyAge, 
                         formula. = ~ . - oxyAge + plaAge,
                         newdata = scdata, control = control)
print(fit_SMCR_plaAge, 4)
p_value(fit_SMCR_plaAge)
plot(marginal_effects(fit_SMCR_plaAge), points = TRUE)

fit_SMCR_oxyMale <- update(fit_SMCR_oxyAge, 
                          formula. = ~ . - oxyAge + oxyMale,
                          newdata = scdata, control = control)
fit_SMCR_oxyMale
p_value(fit_SMCR_oxyMale)

fit_SMCR_plaMale <- update(fit_SMCR_oxyAge, 
                          formula. = ~ . - oxyAge + plaMale,
                          newdata = scdata, control = control)
fit_SMCR_plaMale
p_value(fit_SMCR_plaMale)

fit_SMCR_training <- update(fit_SMCR_oxyAge, 
                           formula. = ~ . - oxyAge + training,
                           newdata = scdata, control = control)
fit_SMCR_training
p_value(fit_SMCR_training)

fit_SMCR_duration <- update(fit_SMCR_oxyAge, 
                           formula. = ~ . - oxyAge + duration,
                           newdata = scdata, control = control)
print(fit_SMCR_duration, 4)
p_value(fit_SMCR_duration)
plot(marginal_effects(fit_SMCR_duration), points = TRUE)

fit_SMCR_dailyDose <- update(fit_SMCR_oxyAge, 
                            formula. = ~ . - oxyAge + dailyDose,
                            newdata = scdata, control = control)
fit_SMCR_dailyDose
p_value(fit_SMCR_dailyDose)

fit_SMCR_eachDose <- update(fit_SMCR_oxyAge, 
                           formula. = ~ . - oxyAge + eachDose,
                           newdata = scdata, control = control)
fit_SMCR_eachDose
p_value(fit_SMCR_eachDose)

fit_SMCR_admin_int <- update(fit_SMCR_oxyAge, 
                            formula. = ~ . - oxyAge + admin_int,
                            newdata = scdata, control = control)
fit_SMCR_admin_int
p_value(fit_SMCR_admin_int)


# ---------------- leave one out analysis ----------------
## social cognition
study_names <- levels(scdata$study)
fits_SMD_social <- fits_SMCR_social <-
  setNames(vector("list", length(study_names)), study_names)
for (i in seq_along(study_names)) {
  print(study_names[i])
  subdata <- droplevels(subset(scdata, study != study_names[i]))
  fits_SMD_social[[i]] <- update(fit_SMD_social, newdata = subdata,
                               control = control)
  fits_SMCR_social[[i]] <- update(fit_SMCR_social, newdata = subdata,
                           control = control)
}
fits_SMD_social
fits_SMCR_social

## general cognition
study_names <- levels(gcdata$study)
fits_SMD_general <- fits_SMCR_general <-
  setNames(vector("list", length(study_names)), study_names)
for (i in seq_along(study_names)) {
  print(study_names[i])
  subdata <- droplevels(subset(gcdata, study != study_names[i]))
  fits_SMD_general[[i]] <- update(fit_SMD_general, newdata = subdata,
                                 control = control)
  fits_SMCR_general[[i]] <- update(fit_SMCR_general, newdata = subdata,
                                  control = control)
}
fits_SMD_general
fits_SMCR_general
