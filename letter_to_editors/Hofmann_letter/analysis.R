require(metafor)
overall_data <- read.csv("dat.csv")

m.smcr <- rma(SMCR * -1, vi = vSMCR,  subset = (incl_overall== 1), data = overall_data, slab = name)
m.smcr
forest(m.smcr)
m.smd <- rma(SMD_post * -1, vi = vSMD_post,   subset = (incl_overall== 1), data = overall_data, slab = name)
m.smd
forest(m.smd)

m.smcr.mod <- rma(SMCR * -1, vi = vSMCR, subset = (in_hofmann== 1),mods = ~ disorder -1, data = overall_data, slab = name)
m.smcr.mod

m.smd.mod <- rma(SMD_post * -1, vi = vSMD_post, subset = (in_hofmann== 1),  mods = ~ disorder -1, data = overall_data, slab = name)
m.smd.mod

