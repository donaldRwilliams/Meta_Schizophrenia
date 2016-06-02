*** analysis by Donny ***
require (brms)

## test analysis for sensitivity to sd and correlation 

## model with sd post used to compute effect size
sdPostR_25 <- brm(yi | se(sqrt(vi)) ~ 0 + factor(sympType) + (1|id), 
          data = sdPostR_25, seed = 1234, warmup = 100, thin = 2, iter = 1000)
sdPostR_50  <- brm(yi | se(sqrt(vi)) ~ 0 + factor(sympType) + (1|id), 
            data = sdPostR_50, seed = 1234, warmup = 100, thin = 2, iter = 1000)
sdPostR_75 <- brm(yi | se(sqrt(vi)) ~ 0 + factor(sympType) + (1|id), 
               data = sdPostR_75, seed = 1234, warmup = 100, thin = 2, iter = 1000)

## model with sd pre used to compute effect size
sdPreR_25 <- brm(yi | se(sqrt(vi)) ~ 0 + factor(sympType) + (1|id), 
                  data = sdPreR_25, seed = 1234, warmup = 100, thin = 2, iter = 1000)
sdPreR_50  <- brm(yi | se(sqrt(vi)) ~ 0 + factor(sympType) + (1|id), 
                   data = sdPreR_50, seed = 1234, warmup = 100, thin = 2, iter = 1000)
sdPreR_75 <- brm(yi | se(sqrt(vi)) ~ 0 + factor(sympType) + (1|id), 
                  data = sdPreR_75, seed = 1234, warmup = 100, thin = 2, iter = 1000)