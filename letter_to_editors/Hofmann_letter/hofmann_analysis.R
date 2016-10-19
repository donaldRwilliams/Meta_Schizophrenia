require(metafor)
adata <- read.csv("dat_2.csv")
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

# enter in averbeck data by hand in excel and save (effect = - 0.51, v = 0.19)
write.csv(adata, "d_for_aver.csv")
adata <- read.csv("d_for_aver.csv")

# main analysis
m.smcr <- rma(SMCR * -1, vi = vSMCR,  subset = (overall== 1), data = adata, slab = name)
m.smcr

m.smd <- rma(SMD_post * -1, vi = vSMD_post,   subset = (overall == 1), data = adata, slab = name)
m.smd

# moderator for symptom types
m.smcr.mod <- rma(SMCR * -1, vi = vSMCR, mods = ~ disorder -1, data = adata, slab = name)
m.smcr.mod

m.smd.mod <- rma(SMD_post * -1, vi = vSMD_post, mods = ~ disorder -1, data = adata, slab = name)
m.smd.mod

# only total psychotic
m.smcr.mod <- rma(SMCR * -1, vi = vSMCR,subset = (psycho_tot== 1) , data = adata, slab = name)
m.smcr.mod

m.smd.mod <- rma(SMD_post * -1, vi = vSMD_post, subset = (psycho_tot== 1), data = adata, slab = name)
m.smd.mod


