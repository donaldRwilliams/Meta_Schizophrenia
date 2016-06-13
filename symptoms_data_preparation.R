adata <- read.csv2("symptomsMod.csv")
adata$obs <- 1:nrow(adata)

library(metafor)
# Hedges'g estimates
adata <- escalc(measure="SMD", m1i=oxyMean_pre, sd1i=oxySd_pre, 
                n1i=oxyN, m2i=plaMean_pre, sd2i=plaSd_pre, 
                n2i=plaN, data=adata, var.names = c("SMD_pre", "vSMD_pre"))

adata <- escalc(measure="SMD", m1i=oxyMean_post, sd1i=oxySd_post, 
                n1i=oxyN, m2i=plaMean_post, sd2i=plaSd_post, 
                n2i=plaN, data=adata, var.names = c("SMD_post", "vSMD_post"))

# assumed pre-post correlation
adata$ri <- 0.5

# SMCR estimates
# standardize based on pre-treatment SD
adata <- escalc(measure="SMCR", m1i=oxyMean_post, m2i=oxyMean_pre,
                sd1i=oxySd_pre, ni=oxyN, ri = ri,
                data=adata, var.names = c("oxySMCR", "voxySMCR"))

adata <- escalc(measure="SMCR", m1i=plaMean_post, m2i=plaMean_pre,
                sd1i=plaSd_pre, ni=plaN, ri = ri,
                data=adata, var.names = c("plaSMCR", "vplaSMCR"))

adata$SMCR <- adata$oxySMCR - adata$plaSMCR
adata$vSMCR <- adata$voxySMCR + adata$vplaSMCR

# split up the data frame
sdata <- droplevels(subset(adata, sympType != "overall"))
tdata <- droplevels(subset(adata, sympType == "overall"))

# compute covariance matrix of the effect sizes
cov_matrix <- function(study_id, out_id, v, R, na.rm = FALSE) {
  # Args:
  #   study_id: a vector of study / sample IDs
  #   out_id: a factor defining outcome IDS
  #   v: a vector containing the variances
  #   R: a symmetric matrix containing correlations
  #      between different levels of out_id
  out_id <- as.character(out_id)
  stopifnot(isSymmetric(unname(R)), 
            nrow(R) == length(unique(out_id)),
            all(rownames(R) %in% unique(out_id)), 
            length(study_id) == length(out_id),
            length(study_id) == length(v))
  colnames(R) <- rownames(R)
  mat <- diag(v)
  se <- sqrt(v)
  se[is.na(se)] <- 0
  if (length(study_id) > 1L) {
    for (i in 2:nrow(mat)) {
      for (j in 1:(i-1)) {
        if (study_id[i] == study_id[j]) {
          mat[i, j] <- mat[j, i] <- R[out_id[i], out_id[j]] * se[i] * se[j] 
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

# correlations based on Peralta and Cuesta 1993
cor_mat <- matrix(c(1, 0.09, 0.20, 0.09, 1, 0.55, 0.20, 0.55, 1), 3, 3)
rownames(cor_mat) <- levels(sdata$sympType)
V_SMD_pre <- cov_matrix(study_id = sdata$study, out_id = sdata$sympType,
                        v = sdata$vSMD_pre, R = cor_mat)

V_SMD_post <- cov_matrix(study_id = sdata$study, out_id = sdata$sympType,
                         v = sdata$vSMD_post, R = cor_mat)

V_SMCR <- cov_matrix(study_id = sdata$study, out_id = sdata$sympType,
                     v = sdata$vSMCR, R = cor_mat)

