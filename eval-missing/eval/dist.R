# Analyse the missingness distribution of
# a given dataset

library(foreign)

toCSV <- function(db, rmFst = FALSE){
	sav <- paste(db, "sav", sep=".")
	csv <- paste(db, "csv", sep=".")
	dataset = read.spss(sav, to.data.frame=TRUE)
	if(rmFst == TRUE){
		dataset[1] <- NULL
	}
	write.csv(dataset, file=csv)
	return(dataset)
}

numNA <- function(db,func){
	nc <- ncol(db)
	numNa <- numeric(nc)
	for(i in 0:nc){
		numNa[i+1] <- nrow(func(db,i))
	}
	numNa
}

eq <- function(db,n){
	db[rowSums(is.na(db))==n,]
}

min <- function(db,n){
	db[rowSums(is.na(db))>=n,]
}

toPerc <- function(v){
	v <- v/sum(v)
}

plotDist <- function(v){
	scatter.smooth(v)
}

# Get missing details for abdn equal to missing
abdnEqual <- numNA(abdn,eq)
# Turn missing equal to percentages
abdnEP <- toPerc(abdnEqual)
# Get missing details for abdn min to miss
abdnMin <- numNA(abdn,min)
# Turn missing min to percentages
# abdnMP <- toPerc(abdnMin)  #<<<< Doesn't work coz we divide by sum of vector....
abdnMP <- abdnMin/2000
# Plot graph
# plot <- plot(abdnEP, type="o", col="red")
plot(abdnMP, type="o", col="red"); lines(abdnEP, type="b", col="blue")
