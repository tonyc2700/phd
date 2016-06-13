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
# Change to percentages
abdnMP <- abdnMP*100
abdnEP <- abdnEP*100
# plot <- plot(abdnEP, type="o", col="red")

jpeg('minmiss.jpg')

plot(abdnMP, type="b", col="red",axes=FALSE,ann=FALSE)
axis(1, at=1:10)
axis(2, las=1, at=20*0:5)
title(xlab="Number of not missing per record")
title(ylab="Percentage of completeness")

dev.off()

jpeg('eqmiss.jpg')

plot(abdnEP, type="b", col="blue",axes=FALSE,ann=FALSE)
axis(1, at=1:10, lab=c("1st","2nd","3rd","4th","5th","6th","7th","8th","9th","10th"))
axis(2, las=1, at=5*0:6)
title(xlab="Column")
title(ylab="Percentage of missingness")

dev.off()


