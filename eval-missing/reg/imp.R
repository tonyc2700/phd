library(mice)
library(VIM)
library(lavaan)

abdn <- read.csv("abdn.csv")
abdn$matsm <- as.factor(abdn$matsm)
abdn$Quintile_SIMD_2006 <- as.factor(abdn$Quintile_SIMD_2006)

completeData <- function(db){
	db[rowSums(is.na(db))==0,]
}


abdnComplete <- completeData(abdn)

model <- ' trajectory =~ Z_CRL_our + Z_BWT_ICH + z_BPD_our '

curve <- growth(model, data = abdnComplete)


