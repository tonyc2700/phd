library(mice)
library(cluster)
library(hydroGOF)

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

missingPercEqual <- function(db){
	nc <- ncol(db) + 1
	missingPerc <- numeric(nc)
	perc <- nrow(db)
	for(i in 1:nc){
		missingPerc[i] <- nrow(db[rowSums(is.na(db))==i-1,])
	}
	missingPerc <- missingPerc
}

missingPercMore <- function(db){
	nc <- ncol(db) + 1
	missingPerc <- numeric(nc)
	perc <- nrow(db)
	for(i in 1:nc){
		missingPerc[i] <- nrow(db[rowSums(is.na(db))>=i-1,])
	}
	missingPerc <- missingPerc
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

meanImpute <- function(db){
	df_test <- db
	for (var in 1:ncol(df_test)) {
	    if (class(df_test[,var]) %in% c("numeric","integer")) {
	        df_test[is.na(df_test[,var]),var] <- mean(df_test[,var], na.rm = TRUE)
	    } else if (class(df_test[,var]) %in% c("character", "factor")) {
	        df_test[is.na(df_test[,var]),var] <- Mode(df_test[,var], na.rm = TRUE)
	    }
	}
	return(df_test)
}

Mode <- function (x, na.rm) {
    xtab <- table(x)
    xmode <- names(which(xtab == max(xtab)))
    if (length(xmode) > 1) xmode <- ">1 mode"
    return(xmode)
}

# Main evaluation of applying MICE to any dataset
# db is the dataset
# nDb is the number of artifically complete datasets
# nClust is the number of clusters for analysis
clemi <- function(db,nDb=4,nClust=3, ...){
	set.seed(Sys.time())
	nDb <- 4
	nClust <- 3
	completeDB <- completeData(db)
	artDB <- artMiss(db)
	# Impute artDB and create artificially complete db
	mi <- mice(artDB, ...)
	# Create nDb amount of artificially complete data
	for (i in 1:nDb){
		assign(paste("artCompleteDB", i, sep = ""), complete(mi,i)) 
	}
	# Impute artDB with mean imputation for reference
	artCompMean <- meanImpute(artDB)
	# Regression on dataset to compare them
	########2
	# Cluster datasets so we can compare them
	completeClust <- clara(completeDB,nClust)
	# Cluster mean imputed dataset
	artCompMeanClust <- clara(artCompMean,nClust)
	# Cluster all imputed datasets
	for (i in 1:nDb){
		assign(paste("artCompleteClust", i, sep=""), clara(get(paste("artCompleteDB", i, sep="")),nClust))
	}
	# Important parts from the clustering outcomes for complete dataset
	compClustInfo <- list(completeClust$clusinfo, completeClust$silinfo$clus.avg.widths, completeClust$silinfo$avg.width )#,completeClust$medoids)
	# Important parts from the clustering outcomes for mean imputed dataset
	artCompMeanClustInfo <- list(artCompMeanClust$clusinfo, artCompMeanClust$silinfo$clus.avg.widths, artCompMeanClust$silinfo$avg.width )#,artCompMeanClust$medoids)
	# Important parts from the clustering outcomes for all imputed datasets
	for (i in 1:nDb){
		assign(paste("artCompClustInfo", i, sep=""), list(
			(get(paste("artCompleteClust", i, sep="")))$clusinfo
			,(get(paste("artCompleteClust", i, sep="")))$silinfo$clus.avg.widths
			,(get(paste("artCompleteClust", i, sep="")))$silinfo$avg.width
			# ,(get(paste("artCompleteClust", i, sep="")))$medoids 
			))
	}
	# Artificially complete cluster information into one average for comparison
	averageArtCompClustInfo <- artCompClustInfo1
	for (i in 2:nDb) {
		averageArtCompClustInfo[[1]] <- ( (get(paste("artCompClustInfo", i, sep="")))[[1]] ) + averageArtCompClustInfo[[1]]
		averageArtCompClustInfo[[2]] <- ( (get(paste("artCompClustInfo", i, sep="")))[[2]] ) + averageArtCompClustInfo[[2]]
		averageArtCompClustInfo[[3]] <- ( (get(paste("artCompClustInfo", i, sep="")))[[3]] ) + averageArtCompClustInfo[[3]]
	}
	averageArtCompClustInfo[[1]] <- averageArtCompClustInfo[[1]]/nDb
	averageArtCompClustInfo[[2]] <- averageArtCompClustInfo[[2]]/nDb
	averageArtCompClustInfo[[3]] <- averageArtCompClustInfo[[3]]/nDb
	# Find distance from complete to averaged artificial
	artDifference <- list(abs(compClustInfo[[1]] - averageArtCompClustInfo[[1]])
						 ,abs(compClustInfo[[2]] - averageArtCompClustInfo[[2]])
						 ,abs(compClustInfo[[3]] - averageArtCompClustInfo[[3]]))
	meanDifference <- list(abs(compClustInfo[[1]] - artCompMeanClustInfo[[1]])
						  ,abs(compClustInfo[[2]] - artCompMeanClustInfo[[2]])
						  ,abs(compClustInfo[[3]] - artCompMeanClustInfo[[3]]))
	# Return dbs for observation
	# list <- list(compClustInfo, artCompMeanClustInfo, artCompClustInfo1, artCompClustInfo2)
	# list <- list(completeClust, artCompleteClust1, artCompleteClust2, artCompleteClust3, artCompleteClust4)
	list <- list(artDifference,meanDifference,compClustInfo)
	return(list)
	#
}

evaluateClemi <- function(db){
	n <- 10
	results <- list(n-1)
	for(i in 1:(n-1)){
		df <- db
		set.seed(42)
		random_rows <- sample(1:nrow(df),round(nrow(df)*(i/n)))
		for(i_row in random_rows){
		    df[i_row,sample(1:ncol(df),sample(1:ncol(df),1))] <- NA
		} 
		# set.seed(Sys.time())
		print(i)
		results[[i]] <- (df)
	}

	return(results)
}

testRMSE <- function(db){
	n <- 10
	results <- list(n-1)
	for(i in 1:(n-1)){
		df <- db
		set.seed(42)
		random_rows <- sample(1:nrow(df),round(nrow(df)*(i/n)))
		for(i_row in random_rows){
		    df[i_row,sample(1:ncol(df),sample(1:ncol(df),1))] <- NA
		} 
		# set.seed(Sys.time())
		print(i)
		# results[[i]] <- rmse(complete(mice(df)),db)
		results[[i]] <- rmse(meanImpute(df),db)
	}

	return(results)
}

rmse2 <- function(error)
{
    sqrt(mean(error^2))
}

## Creates a list of datasets, from 10%missingness allowed to 90%missingness allowed
getDbPerc <- function(db){
	seq <- seq(0.3,0.9,0.1)
	datasets <- list(length(seq))
	results <- list(length(seq))
	for (i in 1:7) {
		datasets[[i]] <- db[which(rowMeans(is.na(db)) <= (i+2)/10), ]
		results[[i]] <- clemi(datasets[[i]])
	}
	return(results)
}
 
plotGraph <- function(results,name){
	n <- length(results)
	clustSizeMi <- list(n)
	clustSizeMean <- list(n)
	avgClustWidthMi <- list(n)
	avgClustWidthMean <- list(n)
	avgWidthMi <- list(n)
	avgWidthMean <- list(n)
	for(i in 1:n){
		clustSizeMi[[i]] <- sum(results[[i]][[1]][[1]][,1])
		clustSizeMean[[i]] <- sum(results[[i]][[2]][[1]][,1])
		avgClustWidthMi[[i]] <- sum(results[[i]][[1]][[2]])
		avgClustWidthMean[[i]] <- sum(results[[i]][[2]][[2]])
		avgWidthMi[[i]] <- results[[i]][[1]][[3]]
		avgWidthMean[[i]] <- results[[i]][[2]][[3]]
	}
	# return(clustSizeMi)
	pdf(paste(name,"pdf",sep="."))
	layout(matrix(c(1,2,3), nrow=3, byrow=FALSE))
	seq <- (1:n)*10
	plot(ylab="Cluster Size Difference",seq,xlab="% of Missing Data",clustSizeMi,type="b",col="blue")
	points(seq,clustSizeMean,col="red",type="b")
	axis(1, at = seq(10,90, by = 10))
	grid()	

	plot(ylab="Cluster Widths Difference",seq,xlab="% of Missing Data",avgClustWidthMi,type="b",col="blue")
	points(seq,avgClustWidthMean,col="red",type="b")
	axis(1, at = seq(10,90, by = 10))
	grid()

	plot(ylab="Average Cluster Width Difference",seq,xlab="% of Missing Data",avgWidthMi,type="b",col="blue")
	points(seq,avgWidthMean,col="red",type="b")
	axis(1, at = seq(10,90, by = 10))
	grid()
	dev.off()
}

multiTest <- function(db,name){
	seq <- 1:5
	for (i in seq) {
		test <- evaluateClemi(db)
		plotGraph(test,paste(name,i,sep=""))
	}
}

# library(foreign)
# db <- "abdn-cleanse.sav"
# dataset <- read.spss(db, to.data.frame=TRUE)

# db <- nhanes
# db$hyp <- as.factor(db$hyp)
# db$chl <- as.integer(db$chl)

# data <- read.csv("forestfires.csv",sep=",")


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





























