library(mice)
library(VIM)
library(lavaan)
library(lme4)

abdn <- read.csv("abdn.csv")
abdn$matsm <- as.factor(abdn$matsm)
abdn$Quintile_SIMD_2006 <- as.factor(abdn$Quintile_SIMD_2006)

source("eval.R")

# Model will be used by all the regressions 
model <- ' trajectory =~ Z_CRL_our + Z_BWT_ICH + z_BPD_our '

curve <- function(db, model){
	growth(model, data = db)
}

regComplete <- curve(abdnComplete, model)

imp1 <- mice(abdn, print=FALSE)
# fit1 <- with(imp1, curve(imp1, model))
fit1 <- with(imp1, lmer(Z_CRL_our ~ Z_BWT_ICH + z_BPD_our))
regImp1 <- pool(fit1)

regImp1Test <- round(summary(pool(fit1)),3)



