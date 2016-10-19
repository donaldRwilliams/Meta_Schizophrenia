compute_eff_size <- function(mean_oxy_post, mean_oxy_pre, mean_pla_post, mean_pla_pre,
                             sd_oxy_post, sd_pla_post, n_oxy, n_pla,
                             var.names = c("yi", "vi")) {
  # try to compute the effect size used by Hoffmann
  # currently unused
  change_oxy <- mean_oxy_post - mean_oxy_pre
  change_pla <- mean_pla_post - mean_pla_pre
  J <- 1 - 3 / (4 * (n_oxy + n_pla) - 9)
  g <- J * (change_oxy - change_pla) / 
    sqrt(((n_oxy - 1) * sd_pla_post^2 + (n_pla - 1) * sd_oxy_post^2) / 
           (n_oxy + n_pla - 2))
  vg <- J^2 * ((n_oxy + n_pla) / (n_oxy * n_pla) + g^2 / (2 * (n_oxy + n_pla)))
  structure(cbind(g, vg), names = list(NULL, var.names))
}

require(metafor)
adata <- read.csv("theData.csv")

adata$ri <- 0.7
adata <- escalc(measure="SMD", m1i=oxyMean_pre, sd1i=oxySd_pre, 
                n1i=oxyN, m2i=plaMean_pre, sd2i=plaSd_pre, 
                n2i=plaN, data=adata, var.names = c("SMD_pre", "vSMD_pre"))

adata <- escalc(measure="SMD", m1i=oxyMean_post, sd1i=oxySd_post, 
                n1i=oxyN, m2i=plaMean_post, sd2i=plaSd_post, 
                n2i=plaN, data=adata, var.names = c("SMD_post", "vSMD_post"))



adata <- escalc(measure="SMCR", m1i=oxyMean_post, m2i=oxyMean_pre,
                sd1i=oxySd_pre, ni=oxyN, ri = ri,
                data=adata, var.names = c("oxySMCR", "voxySMCR"))

adata <- escalc(measure="SMCR", m1i=plaMean_post, m2i=plaMean_pre,
                sd1i=plaSd_pre, ni=plaN, ri = ri,
                data=adata, var.names = c("plaSMCR", "vplaSMCR"))

adata$SMCR <- adata$oxySMCR - adata$plaSMCR
adata$SMCR <- adata$SMCR
adata$vSMCR <- adata$voxySMCR + adata$vplaSMCR

m.smcr_1 <- rma(SMCR, vi = vSMCR, subset = (incl_overall== 1), data = adata, slab = name)
m.smcr_1
forest(m.smcr_1)
m.smcr_0 <- rma(SMCR, vi = vSMCR, subset = (incl_overall== 0), data = adata, slab = name)
m.smcr_0
forest(m.smcr_0)
m.smd_1 <- rma(SMD_post, vi = vSMD_post, subset = (incl_overall== 1), data = adata, slab = name)
m.smd_1
forest(m.smd_1)
m.smd_0 <- rma(SMD_post, vi = vSMD_post, subset = (incl_overall== 0), data = adata, slab = name)
m.smd_0
forest(m.smd_0)