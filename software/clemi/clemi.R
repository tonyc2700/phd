library(mice)
library(cluster)


# Calculated the percentage of missingness per column
colPerc <- function(db){
	nc <- ncol(db)
	colPerc <- numeric(nc)
	perc <- nrow(db)
	for(i in 1:nc){
		colPerc[i] <- length(which(is.na(db[i])))
	}
	# colPerc/sum(colPerc)
	colPerc <- colPerc/perc
}

# Returns only the complete records from a dataset
completeData <- function(db){
	db[rowSums(is.na(db))==0,]
}

# Given a dataset with missingness, extract the
# complete records from the dataset and make
# it mimic the missingness of the larger set.
## Note: this will not make a complete DB in one 
## with missingness and it copies the missingness
## from original (ie original has no missingness)
artMiss <- function(db){
	nc <- ncol(db)
	lperc <- colPerc(db) # list of percentages
	db <- completeData(db)
	# nr <- nrow(db)
	test <- db
	for (i in 1:nc){
		col <- db[[i]]
		perc <- as.integer(lperc[i]*(length(col)))
		col[sample(seq(col),perc)] <- NA
		test[[i]] <- col
	}
	return(test)
}

meanImpute <- function(df){
	# data.frame(x = 1:20, y = c(1:10,rep(NA,10)))
	df$y[is.na(df$y)] = mean(df$y, na.rm=TRUE)
	return(df)
}

meanImpute2 <- function(df){
	df <- transform(df, y <- ifelse(is.na(y), mean(y, na.rm=TRUE), y))
	return(df)
}

# Main evaluation of applying MICE to any dataset
# db is the dataset
# nDb is the number of artifically complete datasets
# nClust is the number of clusters for analysis
clemi <- function(db,nDb=4,nClust=3, ...){
	nDb <- 4
	nClust <- 3
	completeDB <- completeData(db)
	artDB <- artMiss(db)
	# Impute artDB and create artificially complete db
	mi <- mice(artDB, ...)
	#Impute artDB with mean imputation for reference
	completeMean <- meanImpute(artDB)
	# Create nDb amount of artificially complete data
	for (i in 1:nDb){
		assign(paste("artCompleteDB", i, sep = ""), complete(mi,i)) 
	}
	# Regression on dataset to compare them
	########
	# Cluster datasets so we can compare them
	completeClust <- clara(completeDB,nClust)
	# Cluster all imputed datasets
	for (i in 1:nDb){
		assign(paste("artCompleteClust", i, sep=""), clara(get(paste("artCompleteDB", i, sep="")),nClust))
	}
	# 
	compClustInfo <- list(completeClust$clusinfo, completeClust$silinfo$clus.avg.widths, completeClust$silinfo$avg.width ,completeClust$medoids)
	artCompClustInfo1 <- list(artCompleteClust1$clusinfo, artCompleteClust1$silinfo$clus.avg.widths, artCompleteClust1$silinfo$avg.width ,artCompleteClust1$medoids)
	artCompClustInfo2 <- list(artCompleteClust2$clusinfo, artCompleteClust2$silinfo$clus.avg.widths, artCompleteClust2$silinfo$avg.width ,artCompleteClust2$medoids)
	artCompClustInfo3 <- list(artCompleteClust3$clusinfo, artCompleteClust3$silinfo$clus.avg.widths, artCompleteClust3$silinfo$avg.width ,artCompleteClust3$medoids)
	artCompClustInfo4 <- list(artCompleteClust4$clusinfo, artCompleteClust4$silinfo$clus.avg.widths, artCompleteClust4$silinfo$avg.width ,artCompleteClust4$medoids)
	# Find distance from complete to averaged artificial 
	# completeClust$medoids
	# artCompleteClust1$medoids
	# artCompleteClust2$medoids
	# Return dbs for observation
	# list <- list(compClustInfo, artCompClustInfo1, artCompClustInfo2, artCompClustInfo3, artCompClustInfo4)
	list <- list(completeClust, artCompleteClust1, artCompleteClust2, artCompleteClust3, artCompleteClust4)
	return(list)
	#
}

library(foreign)
db = "abdn.sav"
dataset = read.spss(db, to.data.frame=TRUE)




# Read the csv file, ready for testing
# testData <- read.csv("abdn-testData.csv")
# Change maternal smoking and social to factors
# testData$matsm <- as.factor(testData$matsm)
# testData$Quintile_SIMD_2006 <- as.factor(testData$Quintile_SIMD_2006)
# # Calculate missingness percentages or original dataset
# # Used to compare missingness with artificially missing
# lperc <- colPerc(testData)
# # Get complete subset from the original dataset
# testDataComplete <- completeData(testData)
# # Generate artificially missing dataset from complete subset
# testDataArtMiss <- artMiss(testData)
# # Get missingness percentages from artificial data
# # Used to compare missingness with original dataset
# lpercArt <- colPerc(abdnArt)

# ## Testing stuff:
# testComplete <- completeData(testData)
# testArt1 <- artMiss(testData)
# testArt2 <- artMiss(testData)
# testMi1 <- mice(testArt1)
# testMi2 <- mice(testArt2)
# testImp11 <- complete(testMi1,1)
# testImp12 <- complete(testMi1,2)
# testImp13 <- complete(testMi1,3)
# testImp21 <- complete(testMi2,1)
# testImp22 <- complete(testMi2,2)
# testImp23 <- complete(testMi2,3)





























