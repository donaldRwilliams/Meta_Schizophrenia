fit_SMD_level <- brm(SMD | se(sqrt(vSMD)) ~ 
                       level + (1|sample) + (1|obs), 
                     data = scdata, prior = prior, sample_prior = TRUE,
                     iter = iter, control = control)
study_names <- sort(unique(scdata$study))

fits_SMD_level <-
  setNames(vector("list", length(study_names)), study_names)
for (i in seq_along(study_names)) {
  print(study_names[i])
  subdata <- droplevels(subset(scdata, study != study_names[i]))
  capture.output({
    
    fits_SMD_level[[i]] <- update(fit_SMD_level, newdata = subdata,
                                  control = control)
  })
}
fits_SMD_level
plot(marginal_effects(fits_SMD_level$`Averbeck et al. (2012)`), points = TRUE)
hypothesis(fits_SMD_level$`Averbeck et al. (2012)`, "Intercept + levelHigh = 0")

plot(marginal_effects(fits_SMD_level$`Cacciotti-Saija et al. (2015)`), points = TRUE)
  hypothesis(fits_SMD_level$`Cacciotti-Saija et al. (2015)`, "Intercept + levelHigh = 0")

plot(marginal_effects(fits_SMD_level$`Davis et al. (2013)`), points = TRUE)
hypothesis(fits_SMD_level$`Davis et al. (2013)`, "Intercept + levelHigh = 0")

plot(marginal_effects(fits_SMD_level$`de Macedo et al. (2014)`), points = TRUE)
hypothesis(fits_SMD_level$`de Macedo et al. (2014)`, "Intercept + levelHigh = 0")

plot(marginal_effects(fits_SMD_level$`Fischer-Shofty et al. (2013)`), points = TRUE)
hypothesis(fits_SMD_level$`Fischer-Shofty et al. (2013)`, "Intercept + levelHigh = 0")

plot(marginal_effects(fits_SMD_level$`Gibson et al. (2014)`), points = TRUE)
hypothesis(fits_SMD_level$`Gibson et al. (2014)`, "Intercept + levelHigh = 0")

plot(marginal_effects(fits_SMD_level$`Goldman et al. (2011)`), points = TRUE)
hypothesis(fits_SMD_level$`Goldman et al. (2011)`, "Intercept + levelHigh = 0")

plot(marginal_effects(fits_SMD_level$`Guastella et al. (2015)`), points = TRUE)
hypothesis(fits_SMD_level$`Guastella et al. (2015)`, "Intercept + levelHigh = 0")

plot(marginal_effects(fits_SMD_level$`Pederson et al. (2011)`), points = TRUE)
hypothesis(fits_SMD_level$`Pederson et al. (2011)`, "Intercept + levelHigh = 0")

plot(marginal_effects(fits_SMD_level$`Woolley et al. (2014)`), points = TRUE)
hypothesis(fits_SMD_level$`Woolley et al. (2014)`, "Intercept + levelHigh = 0")

