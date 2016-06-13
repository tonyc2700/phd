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

# Given a dataset with missingness, extract
# the complete records from the dataset
# and make it mimic the missingness of the
# larger set.
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

abdn <- read.csv("abdn.csv")
abdn[1] <- NULL
# Get original missingness percentages
lperc <- colPerc(abdn)
# Get subset of abdn of complete cases
abdnComplete <- completeData(abdn)
# Generate artificial 
abdnArt <- artMiss(abdn)
# Get missingness percentages from artificial data
lpercArt <- colPerc(abdnArt)



# testMiss[sample(seq(testMiss),20)]<-NA
	# db[sample(seq(db),perc)] <- NA