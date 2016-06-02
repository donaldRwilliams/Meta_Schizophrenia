*** analysis by Donny Williams ***
withInOxy <- read.csv("withInOxy.csv")  

## with sd post  
  sdPostR_25 <- escalc(measure="SMCR", m1i=meanPostOxy, m2i=meanPreOxy, sd1i=sdPostOxy, ni=n, ri=r, data=withInOxy)
  sdPostR_50 <- escalc(measure="SMCR", m1i=meanPostOxy, m2i=meanPreOxy, sd1i=sdPostOxy, ni=n, ri=r.1, data=withInOxy)
  sdPostR_75 <- escalc(measure="SMCR", m1i=meanPostOxy, m2i=meanPreOxy, sd1i=sdPostOxy, ni=n, ri=r.2, data=withInOxy)
  
## with sd pre  
  sdPreR_25 <- escalc(measure="SMCR", m1i=meanPostOxy, m2i=meanPreOxy, sd1i=sdPreOxy, ni=n, ri=r, data=withInOxy)
  sdPreR_50 <- escalc(measure="SMCR", m1i=meanPostOxy, m2i=meanPreOxy, sd1i=sdPreOxy, ni=n, ri=r.1, data=withInOxy)
  sdPreR_75 <- escalc(measure="SMCR", m1i=meanPostOxy, m2i=meanPreOxy, sd1i=sdPreOxy, ni=n, ri=r.2, data=withInOxy)