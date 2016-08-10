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
                 set_prior("cauchy(0,0.3)", class = "sd"))
iter <- 5000
options(mc.cores = 2)
control <- list(adapt_delta = 0.95)

# ---------- primary meta-analysis ----------------
## social cognition
### SMD
fit_SMD_social <- brm(SMD | se(sqrt(vSMD)) ~ 
                        0 + intercept + (1|sample) + (1|obs), 
                      data = scdata, prior = prior, sample_prior = TRUE,
                      iter = iter, control = control)
fit_SMD_social
p_value(fit_SMD_social)
(hyp_SMD_social <- hypothesis(fit_SMD_social, "intercept = 0"))
plot(fit_SMD_social, ask = FALSE)
plot(hyp_SMD_social, chars = NULL, ask = FALSE)


### SMCR
fit_SMCR_social <- brm(SMCR | se(sqrt(vSMCR)) ~ 
                         0 + intercept + (1|sample) + (1|obs), 
                      data = scdata, prior = prior, sample_prior = TRUE,
                      iter = iter, control = control)
fit_SMCR_social
p_value(fit_SMCR_social)
(hyp_SMCR_social <- hypothesis(fit_SMCR_social, "intercept = 0"))
plot(fit_SMCR_social, ask = FALSE)
plot(hyp_SMCR_social, chars = NULL, ask = FALSE)


## neurocognition
### SMD
fit_SMD_neuro <- brm(SMD | se(sqrt(vSMD)) ~ 
                        0 + intercept + (1|sample) + (1|obs), 
                      data = ncdata, prior = prior, sample_prior = TRUE,
                      iter = iter, control = control)
fit_SMD_neuro
p_value(fit_SMD_neuro)
(hyp_SMD_neuro <- hypothesis(fit_SMD_neuro, "intercept = 0"))
plot(fit_SMD_neuro, ask = FALSE)
plot(hyp_SMD_neuro, chars = NULL, ask = FALSE)


### SMCR
fit_SMCR_neuro <- brm(SMCR | se(sqrt(vSMCR)) ~ 
                         0 + intercept + (1|sample) + (1|obs), 
                       data = ncdata, prior = prior, sample_prior = TRUE,
                       iter = iter, control = control)
fit_SMCR_neuro
p_value(fit_SMCR_neuro)
(hyp_SMCR_neuro <- hypothesis(fit_SMCR_neuro, "intercept = 0"))
plot(fit_SMCR_neuro, ask = FALSE)
plot(hyp_SMCR_neuro, chars = NULL, ask = FALSE)



# ------- moderator analysis ------
# only for social cognition data
lSG2 <- levels(scdata$SG2)
lemotion <- levels(scdata$emotion)

## SMD
### cell mean coded moderators (not used anymore)
# fit_SMD_sg2 <- brm(SMD | se(sqrt(vSMD)) ~ 
#                      0 + SG2 + (1|sample) + (1|obs), 
#                    data = scdata, prior = prior_sg2a3, 
#                    sample_prior = TRUE,
#                    iter = iter, control = control)
# fit_SMD_sg2
# plot(fit_SMD_sg2, ask = FALSE)
# (hyp_SMD_sg2 <- hypothesis(fit_SMD_sg2, paste("SG2", lSG2, " = 0")))
# plot(hyp_SMD_sg2, chars = NULL, ask = FALSE, N = 6)
# p_value(fit_SMD_sg2)
# marginal_effects(fit_SMD_sg2)
# 
# fit_SMD_emotion <- brm(SMD | se(sqrt(vSMD)) ~ 
#                      0 + emotion + (1|sample) + (1|obs), 
#                    data = scdata, prior = prior_sg2a3, 
#                    sample_prior = TRUE,
#                    iter = iter, control = control)
# fit_SMD_emotion
# plot(fit_SMD_emotion, ask = FALSE)
# (hyp_SMD_emotion <- hypothesis(fit_SMD_emotion, 
#                                paste("emotion", lemotion, " = 0")))
# plot(hyp_SMD_emotion, chars = NULL, ask = FALSE)
# p_value(fit_SMD_emotion)
# marginal_effects(fit_SMD_emotion)

### other moderators 
fit_SMD_oxyAge <- brm(SMD | se(sqrt(vSMD)) ~ 
                        0 + intercept + oxyAge + (1|sample) + (1|obs), 
                      data = scdata, prior = prior, sample_prior = TRUE,
                      iter = iter, control = control)
print(fit_SMD_oxyAge, 3)
p_value(fit_SMD_oxyAge)
plot(fit_SMD_oxyAge)
plot(marginal_effects(fit_SMD_oxyAge), points = TRUE)

fit_SMD_plaAge <- update(fit_SMD_oxyAge, 
                         formula. = ~ . - oxyAge + plaAge,
                         newdata = scdata, control = control)
print(fit_SMD_plaAge, 3)
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

# not used anymore
# fit_SMD_duration <- update(fit_SMD_oxyAge, 
#                            formula. = ~ . - oxyAge + duration,
#                            newdata = scdata, control = control)
# print(fit_SMD_duration, 3)
# p_value(fit_SMD_duration)
# plot(marginal_effects(fit_SMD_duration), points = TRUE)

fit_SMD_eachDose <- update(fit_SMD_oxyAge, 
                           formula. = ~ . - oxyAge + eachDose,
                           newdata = scdata, control = control)
print(fit_SMD_eachDose, 3)
p_value(fit_SMD_eachDose)

fit_SMD_dailyDose <- update(fit_SMD_oxyAge, 
                            formula. = ~ . - oxyAge + dailyDose,
                            newdata = scdata, control = control)
print(fit_SMD_dailyDose, 3)
p_value(fit_SMD_dailyDose)

fit_SMD_admin_int <- update(fit_SMD_oxyAge, 
                            formula. = ~ . - oxyAge + admin_int,
                            newdata = scdata, control = control)
fit_SMD_admin_int
p_value(fit_SMD_admin_int)

fit_SMD_country <- update(fit_SMD_oxyAge, 
                          formula. = ~ . - oxyAge + country_simple,
                          newdata = scdata, control = control)
fit_SMD_country
p_value(fit_SMD_country)

fit_SMD_level <- update(fit_SMD_oxyAge, 
                        formula. = ~ . - oxyAge + level,
                        newdata = scdata, control = control)

fit_SMD_level
p_value(fit_SMD_level)
plot(marginal_effects(fit_SMD_level), points = TRUE)
hypothesis(fit_SMD_level, "intercept + levelHigh = 0")

# cognition level with priors
# add_prior <- set_prior("normal(0,0.2)", coef = "levelHigh")
# fit_SMD_level2 <- update(fit_SMD_level, prior = add_prior)
# (hyp <- hypothesis(fit_SMD_level2, "levelHigh = 0"))
# plot(hyp)

### special moderator analysis for emotion recognition of fear
fit_SMD_fear <- brm(SMD | se(sqrt(vSMD)) ~ 
                       0 + intercept + fear + (1|sample) + (1|obs), 
                     data = ercdata, prior = prior, sample_prior = TRUE,
                     iter = iter, control = control)
fit_SMD_fear
p_value(fit_SMD_fear)

## SMCR
### cell mean coded moderators
# fit_SMCR_sg2 <- brm(SMCR | se(sqrt(vSMCR)) ~ 
#                      0 + SG2 + (1|sample) + (1|obs), 
#                    data = scdata, prior = prior_sg2a3, 
#                    sample_prior = TRUE,
#                    iter = iter, control = control)
# fit_SMCR_sg2
# plot(fit_SMCR_sg2, ask = FALSE)
# (hyp_SMCR_sg2 <- hypothesis(fit_SMCR_sg2, paste("SG2", lSG2, " = 0")))
# plot(hyp_SMCR_sg2, chars = NULL, ask = FALSE)
# p_value(fit_SMCR_sg2)
# marginal_effects(fit_SMCR_sg2)
# 
# fit_SMCR_emotion <- brm(SMCR | se(sqrt(vSMCR)) ~ 
#                      0 + emotion + (1|sample) + (1|obs), 
#                    data = scdata, prior = prior_sg2a3, 
#                    sample_prior = TRUE,
#                    iter = iter, control = control)
# fit_SMCR_emotion
# plot(fit_SMCR_emotion, ask = FALSE)
# (hyp_SMCR_emotion <- hypothesis(fit_SMCR_emotion, 
#                                 paste("emotion", lemotion, " = 0")))
# plot(hyp_SMCR_emotion, chars = NULL, ask = FALSE)
# p_value(fit_SMCR_emotion)
# marginal_effects(fit_SMCR_emotion)
# 
# ### other moderators
# fit_SMCR_oxyAge <- brm(SMCR | se(sqrt(vSMCR)) ~ 
#                         0 + intercept + oxyAge + (1|sample) + (1|obs), 
#                       data = scdata, prior = prior, sample_prior = TRUE,
#                       iter = iter, control = control)
# print(fit_SMCR_oxyAge, 4)
# plot(fit_SMCR_oxyAge)
# p_value(fit_SMCR_oxyAge)
# plot(marginal_effects(fit_SMCR_oxyAge), points = TRUE)
# 
# fit_SMCR_plaAge <- update(fit_SMCR_oxyAge, 
#                          formula. = ~ . - oxyAge + plaAge,
#                          newdata = scdata, control = control)
# print(fit_SMCR_plaAge, 4)
# p_value(fit_SMCR_plaAge)
# plot(marginal_effects(fit_SMCR_plaAge), points = TRUE)
# 
# fit_SMCR_oxyMale <- update(fit_SMCR_oxyAge, 
#                           formula. = ~ . - oxyAge + oxyMale,
#                           newdata = scdata, control = control)
# fit_SMCR_oxyMale
# p_value(fit_SMCR_oxyMale)
# 
# fit_SMCR_plaMale <- update(fit_SMCR_oxyAge, 
#                           formula. = ~ . - oxyAge + plaMale,
#                           newdata = scdata, control = control)
# fit_SMCR_plaMale
# p_value(fit_SMCR_plaMale)
# 
# fit_SMCR_training <- update(fit_SMCR_oxyAge, 
#                            formula. = ~ . - oxyAge + training,
#                            newdata = scdata, control = control)
# fit_SMCR_training
# p_value(fit_SMCR_training)
# 
# fit_SMCR_duration <- update(fit_SMCR_oxyAge, 
#                            formula. = ~ . - oxyAge + duration,
#                            newdata = scdata, control = control)
# print(fit_SMCR_duration, 4)
# p_value(fit_SMCR_duration)
# plot(marginal_effects(fit_SMCR_duration), points = TRUE)
# 
# fit_SMCR_dailyDose <- update(fit_SMCR_oxyAge, 
#                             formula. = ~ . - oxyAge + dailyDose,
#                             newdata = scdata, control = control)
# fit_SMCR_dailyDose
# p_value(fit_SMCR_dailyDose)
# 
# fit_SMCR_eachDose <- update(fit_SMCR_oxyAge, 
#                            formula. = ~ . - oxyAge + eachDose,
#                            newdata = scdata, control = control)
# fit_SMCR_eachDose
# p_value(fit_SMCR_eachDose)
# 
# fit_SMCR_admin_int <- update(fit_SMCR_oxyAge, 
#                             formula. = ~ . - oxyAge + admin_int,
#                             newdata = scdata, control = control)
# fit_SMCR_admin_int
# p_value(fit_SMCR_admin_int)
# 
# fit_SMCR_country <- update(fit_SMCR_oxyAge, 
#                            formula. = ~ . - oxyAge + country_simple,
#                            newdata = scdata, control = control)
# fit_SMCR_country
# p_value(fit_SMCR_country)
# 
# fit_SMCR_level <- update(fit_SMCR_oxyAge, 
#                          formula. = ~ . - oxyAge + level,
#                          newdata = scdata, control = control)
# fit_SMCR_level
# p_value(fit_SMCR_level)
# 
# ### special moderator analysis for emotion recognition of fear
# fit_SMCR_fear <- brm(SMCR | se(sqrt(vSMCR)) ~ 
#                          0 + intercept + fear + (1|sample) + (1|obs), 
#                        data = ercdata, prior = prior, sample_prior = TRUE,
#                        iter = iter, control = control)
# fit_SMCR_fear
# p_value(fit_SMCR_fear)


# ---------------- leave one out analysis ----------------
## social cognition
study_names <- sort(unique(scdata$study))
fits_SMD_social <- fits_SMCR_social <- fits_SMD_level <-
  setNames(vector("list", length(study_names)), study_names)
for (i in seq_along(study_names)) {
  print(study_names[i])
  subdata <- droplevels(subset(scdata, study != study_names[i]))
  capture.output({
    fits_SMD_social[[i]] <- update(fit_SMD_social, newdata = subdata,
                                   control = control)
    fits_SMCR_social[[i]] <- update(fit_SMCR_social, newdata = subdata,
                                    control = control)
    fits_SMD_level[[i]] <- update(fit_SMD_level, newdata = subdata,
                                  control = control)
  })
}
fits_SMD_social
fits_SMCR_social
fits_SMD_level

## neurocognition
study_names <- sort(unique(ncdata$study))
fits_SMD_neuro <- fits_SMCR_neuro <-
  setNames(vector("list", length(study_names)), study_names)
for (i in seq_along(study_names)) {
  print(study_names[i])
  subdata <- droplevels(subset(ncdata, study != study_names[i]))
  capture.output({
    fits_SMD_neuro[[i]] <- update(fit_SMD_neuro, newdata = subdata,
                                  control = control)
    fits_SMCR_neuro[[i]] <- update(fit_SMCR_neuro, newdata = subdata,
                                   control = control)
  })
}
fits_SMD_neuro
fits_SMCR_neuro


# --------- study characteristics ---------------
ucdata <- cdata[!duplicated(cdata$sample), ]
# number of patients
N <- ifelse(ucdata$design == "crossover", ucdata$oxyN,
            ucdata$oxyN + ucdata$plaN)
range(N)  # Goldman et al. 2011 contains 2 separate samples
sum(N)
# number of samples
length(unique(ucdata$sample))
# range of daily dose
range(ucdata$dailyDose)
# number of studies with pre-treatment values
sum(!is.na(ucdata$SMCR))
## mean age
sum_age_basis <- sum(ucdata$oxyN * ucdata$oxyAge + ucdata$plaN * ucdata$plaAge)
sum_age_basis / sum(ucdata$oxyN + ucdata$plaN)
## mean % males
sum_male_basis <- sum(ucdata$oxyN * ucdata$oxyMale + ucdata$plaN * ucdata$plaMale)
sum_male_basis / sum(ucdata$oxyN + ucdata$plaN)

## prepare study characteristics table
table1 <- cdata
to_format <- c("oxyMean_pre", "oxyMean_post", "oxySd_pre", "oxySd_post",
               "plaMean_pre", "plaMean_post", "plaSd_pre", "plaSd_post")
table1[, to_format] <- format(table1[, to_format], nsmall = 2, trim = TRUE)
table1$N <- paste0("'", table1$oxyN, " / ", table1$plaN)
table1$mean_pre <- paste0("'", table1$oxyMean_pre, " / ", table1$plaMean_pre)
table1$SD_pre <- paste0("'", table1$oxySd_pre, " / ", table1$plaSd_pre)
table1$mean_post <- paste0("'", table1$oxyMean_post, " / ", table1$plaMean_post)
table1$SD_post <- paste0("'", table1$oxySd_post, " / ", table1$plaSd_post)
table1$male <- paste(round(table1$oxyMale * 100), "% /", round(table1$plaMale * 100), "%")
table1$age <- paste(round(table1$oxyAge, 1), "/", round(table1$plaAge, 1))
table1 <- table1[, c("SG1", "sample", "country", "N", "age", "male", "dailyDose",
                     "outcome", "mean_pre", "SD_pre", "mean_post", "SD_post")]
write.table(table1, file = "table1.csv", dec = ",", sep = ";",
            row.names = FALSE)
