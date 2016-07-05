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
prior_tau <- c(set_prior("cauchy(0,0.3)", class = "sd"))
prior_eff <- c(set_prior("normal(0,0.5)", coef = "sympTypegeneral"),
               set_prior("normal(0,0.5)", coef = "sympTypenegative"),
               set_prior("normal(0,0.5)", coef = "sympTypepositive"))
prior <- rbind(prior_eff, prior_tau)
iter <- 5000
options(mc.cores = 2)
control <- list(adapt_delta = 0.95)
symp_type <- unique(sdata$sympType)
conditions <- data.frame(study = unique(sdata$study))
rownames(conditions) <- unique(sdata$study)
theme_set(theme_bw())


# ---------------- primary (multivariate) meta-analysis ----------------
## SMD
fit_SMD_post <- brm(SMD_post ~ 0 + sympType + (0+sympType || study), 
                    data = sdata, autocor = cor_fixed(V_SMD_post), 
                    prior = prior, sample_prior = TRUE, iter = iter,
                    control = control)
fit_SMD_post
plot(fit_SMD_post, ask = FALSE)
(hyp_SMD_post <- hypothesis(fit_SMD_post, paste("sympType", symp_type, " = 0")))
plot(hyp_SMD_post)
p_value(fit_SMD_post)
marginal_effects(fit_SMD_post)
plot(marginal_effects(fit_SMD_post, conditions = conditions, 
                      re_formula = NULL), points = TRUE, ncol = 4)
### compare symptom types
hyp_SMD_post2 <- hypothesis(fit_SMD_post, c("sympTypenegative - sympTypepositive = 0",
                                            "sympTypenegative - sympTypegeneral = 0",
                                            "sympTypepositive - sympTypegeneral = 0"))
print(hyp_SMD_post2, chars = NULL)
plot(hyp_SMD_post2)

## SMCR
fit_SMCR <- brm(SMCR ~ 0 + sympType + (0+sympType || study), data = sdata,
                autocor = cor_fixed(V_SMCR), prior = prior,
                sample_prior = TRUE, iter = iter, control = control)
fit_SMCR
plot(fit_SMCR, ask = FALSE)
(hyp_SMCR <- hypothesis(fit_SMCR, paste("sympType", symp_type, " = 0")))
plot(hyp_SMCR)
p_value(fit_SMCR)
marginal_effects(fit_SMCR)
plot(marginal_effects(fit_SMCR, conditions = conditions, 
                      re_formula = NULL), points = TRUE, ncol = 4)
### compare symptom types
hyp_SMCR2 <- hypothesis(fit_SMCR, c("sympTypenegative - sympTypepositive = 0",
                                    "sympTypenegative - sympTypegeneral = 0",
                                    "sympTypepositive - sympTypegeneral = 0"))
print(hyp_SMCR2, chars = NULL)
plot(hyp_SMCR2)


# ---------------- analysis of overall symptoms ---------------- 
prior_ove <- rbind(c(set_prior("normal(0,0.5)", coef = "intercept")), 
                   prior_tau)
fit_SMD_post_ove <- brm(SMD_post | se(sqrt(vSMD_post)) ~ 0 + intercept + (1|study), 
                        data = ove_data, prior = prior_ove, sample_prior = TRUE,
                        iter = iter, control = control)
fit_SMD_post_ove
(hyp_SMD_post_ove <- hypothesis(fit_SMD_post_ove, "intercept = 0"))
plot(hyp_SMD_post_ove)
p_value(fit_SMD_post_ove)

fit_SMCR_ove <- brm(SMCR | se(sqrt(vSMCR)) ~ 0 + intercept  + (1|study), 
                    data = ove_data, prior = prior_ove, sample_prior = TRUE,
                    iter = iter, control = control)
fit_SMCR_ove
(hyp_SMCR_ove <- hypothesis(fit_SMCR_ove, "intercept = 0"))
plot(hyp_SMCR_ove)
p_value(fit_SMCR_ove)


# ---------------- moderator analyses ---------------- 
## assuming the same effects across symptom types
## SMD
fit_SMD_oxyAge <- brm(SMD_post ~ 0 + sympType + oxyAge + (0+sympType||study), 
                      data = sdata, autocor = cor_fixed(V_SMD_post), 
                      prior = prior, sample_prior = TRUE, iter = iter,
                      control = control)
fit_SMD_oxyAge
p_value(fit_SMD_oxyAge)

fit_SMD_plaAge <- update(fit_SMD_oxyAge, formula. = ~ . + plaAge - oxyAge,
                         newdata = sdata, control = control)
fit_SMD_plaAge
p_value(fit_SMD_plaAge)

fit_SMD_oxyMale <- update(fit_SMD_oxyAge, formula. = ~ . + oxyMale - oxyAge,
                          newdata = sdata, control = control)
fit_SMD_oxyMale
p_value(fit_SMD_oxyMale)

fit_SMD_plaMale <- update(fit_SMD_oxyAge, formula. = ~ . + plaMale - oxyAge,
                          newdata = sdata, control = control)
fit_SMD_plaMale
p_value(fit_SMD_plaMale)

fit_SMD_duration <- update(fit_SMD_oxyAge, formula. = ~ . + duration - oxyAge,
                           newdata = sdata, control = control)
fit_SMD_duration
p_value(fit_SMD_duration)

fit_SMD_eachDose <- update(fit_SMD_oxyAge, formula. = ~ . + eachDose - oxyAge,
                           newdata = sdata, control = control)
fit_SMD_eachDose
p_value(fit_SMD_eachDose)

fit_SMD_ <- update(fit_SMD_oxyAge, formula. = ~ . + dailyDose - oxyAge,
                   newdata = sdata, control = control)
fit_SMD_dailyDose
p_value(fit_SMD_dailyDose)

fit_SMD_admin_int <- update(fit_SMD_oxyAge, formula. = ~ . + admin_int - oxyAge,
                            newdata = sdata, control = control)
fit_SMD_admin_int
marginal_effects(fit_SMD_admin_int)

fit_SMD_training <- update(fit_SMD_oxyAge, formula. = ~ . + training - oxyAge,
                           newdata = sdata, control = control)
fit_SMD_training
p_value(fit_SMD_training)

fit_SMD_country <- update(fit_SMD_oxyAge, formula. = ~ . + country - oxyAge,
                          newdata = sdata, control = control)
fit_SMD_country
p_value(fit_SMD_country)

# SMCR
fit_SMCR_oxyAge <- brm(SMCR ~ 0 + sympType + oxyAge + (0+sympType||study), 
                      data = sdata, autocor = cor_fixed(V_SMCR), 
                      prior = prior, sample_prior = TRUE, iter = iter,
                      control = control)
fit_SMCR_oxyAge
p_value(fit_SMCR_oxyAge)

fit_SMCR_plaAge <- update(fit_SMCR_oxyAge, formula. = ~ . + plaAge - oxyAge,
                          newdata = sdata, control = control)
fit_SMCR_plaAge
p_value(fit_SMCR_plaAge)

fit_SMCR_oxyMale <- update(fit_SMCR_oxyAge, formula. = ~ . + oxyMale - oxyAge,
                           newdata = sdata, control = control)
fit_SMCR_oxyMale
p_value(fit_SMCR_oxyMale)

fit_SMCR_plaMale <- update(fit_SMCR_oxyAge, formula. = ~ . + plaMale - oxyAge,
                           newdata = sdata, control = control)
fit_SMCR_plaMale
p_value(fit_SMCR_plaMale)

fit_SMCR_duration <- update(fit_SMCR_oxyAge, formula. = ~ . + duration - oxyAge,
                            newdata = sdata, control = control)
fit_SMCR_duration
p_value(fit_SMCR_duration)

fit_SMCR_eachDose <- update(fit_SMCR_oxyAge, formula. = ~ . + eachDose - oxyAge,
                            newdata = sdata, control = control)
fit_SMCR_eachDose
p_value(fit_SMCR_eachDose)

fit_SMCR_dailyDose <- update(fit_SMCR_oxyAge, formula. = ~ . + dailyDose - oxyAge,
                             newdata = sdata, control = control)
fit_SMCR_dailyDose
p_value(fit_SMCR_dailyDose)

fit_SMCR_admin_int <- update(fit_SMCR_oxyAge, formula. = ~ . + admin_int - oxyAge,
                             newdata = sdata, control = control)
fit_SMCR_admin_int
p_value(fit_SMCR_admin_int)

fit_SMCR_training <- update(fit_SMCR_oxyAge, formula. = ~ . + training - oxyAge,
                            newdata = sdata, control = control)
fit_SMCR_training
p_value(fit_SMCR_training)

fit_SMCR_country <- update(fit_SMCR_oxyAge, formula. = ~ . + country - oxyAge,
                           newdata = sdata, control = control)
fit_SMCR_country
p_value(fit_SMCR_country)


# ---------------- leave one out analysis ----------------
study_names <- unique(sdata$study)
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


# ------------ study characteristics ----------
udata <- sdata[!duplicated(sdata$name), ]
summary(udata$plaN + udata$oxyN)
## mean age
sum_age_basis <- sum(udata$oxyN * udata$oxyAge + udata$plaN * udata$plaAge)
sum_age_basis / sum(udata$oxyN + udata$plaN)
## mean % males
sum_male_basis <- sum(udata$oxyN * udata$oxyMale + udata$plaN * udata$plaMale)
sum_male_basis / sum(udata$oxyN + udata$plaN)

## prepare study characteristics table
table1 <- adata[order(adata$name), ]
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
table1 <- table1[, c("name", "year", "N", "age", "male", "sympType", 
                    "mean_pre", "SD_pre", "mean_post", "SD_post")]
write.table(table1, file = "table1.csv", dec = ",", sep = ";",
            row.names = FALSE)
