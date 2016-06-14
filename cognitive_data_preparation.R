cdata <- read.csv2("data/cognitive.csv")
cdata$obs <- 1:nrow(cdata)

library(metafor)
# Hedges'g estimates
# for now treat crossover designs as having independent groups
cdata <- escalc(measure="SMD", m1i=oxyMean_pre, sd1i=oxySd_pre, 
                n1i=oxyN, m2i=plaMean_pre, sd2i=plaSd_pre, 
                n2i=plaN, data=cdata, var.names = c("SMD_pre", "vSMD_pre"))

cdata <- escalc(measure="SMD", m1i=oxyMean_post, sd1i=oxySd_post, 
                n1i=oxyN, m2i=plaMean_post, sd2i=plaSd_post, 
                n2i=plaN, data=cdata, var.names = c("SMD_post", "vSMD_post"))

# assumed pre-post correlation
cdata$ri <- 0.5

# SMCR estimates
# standardize based on pre-treatment SD
cdata <- escalc(measure="SMCR", m1i=oxyMean_post, m2i=oxyMean_pre,
                sd1i=oxySd_pre, ni=oxyN, ri = ri,
                data=cdata, var.names = c("oxySMCR", "voxySMCR"))

cdata <- escalc(measure="SMCR", m1i=plaMean_post, m2i=plaMean_pre,
                sd1i=plaSd_pre, ni=plaN, ri = ri,
                data=cdata, var.names = c("plaSMCR", "vplaSMCR"))

# for now treat crossover designs as having independent groups
cdata$SMCR <- cdata$oxySMCR - cdata$plaSMCR
cdata$vSMCR <- cdata$voxySMCR + cdata$vplaSMCR


# correct the direction of effects (positive = improvement)
cdata$SMD_pre <- cdata$SMD_pre * cdata$direction
cdata$SMD_post <- cdata$SMD_post * cdata$direction
cdata$SMCR <- cdata$SMCR * cdata$direction

# define factor contrast
sum_coding <- function(x, levels = levels(x)) {
  x <- factor(x, levels = levels)
  contrasts(x) <- contr.sum(length(levels))
  colnames(contrasts(x)) <- levels[-length(levels)]
  x
}
cdata$subgroup_1 <- sum_coding(subgroup_1)
cdata$subgroup_2 <- sum_coding(subgroup_2)
cdata$subgroup_3 <- sum_coding(subgroup_3)

# compute covariance matrix of the effect sizes
cov_matrix2 <- function(study_id, v, r, na.rm = FALSE) {
  # Args:
  #   study_id: a vector of study / sample IDs
  #   v: a vector containing the variances
  #   r: correlation between different outcomes 
  #      of the same study
  stopifnot(length(r) == 1L, r >= -1 && r <= 1,  
            length(study_id) == length(v))
  mat <- diag(v)
  se <- sqrt(v)
  se[is.na(se)] <- 0
  if (length(study_id) > 1L) {
    for (i in 2:nrow(mat)) {
      for (j in 1:(i-1)) {
        if (study_id[i] == study_id[j]) {
          mat[i, j] <- mat[j, i] <- r * se[i] * se[j] 
        }
      }
    }
  }
  dimnames(mat) <- list(1:nrow(mat), 1:ncol(mat))
  if (na.rm) {
    keep <- !is.na(diag(mat))
    mat <- mat[keep, keep]
  }
  mat
}

# use r = 0.7 for now
V_SMD_pre <- cov_matrix2(study_id = cdata$study, 
                         v = cdata$vSMD_pre, r = 0.7)

V_SMD_post <- cov_matrix2(study_id = cdata$study, 
                          v = cdata$vSMD_post, r = 0.7)

V_SMCR <- cov_matrix2(study_id = cdata$study,
                      v = cdata$vSMCR, r = 0.7)
