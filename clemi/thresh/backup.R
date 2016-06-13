equalNA <- function(db){
	nc <- ncol(db)
	# perc <- nrow(db)
	numNa <- numeric(nc)
	for(i in 0:nc){
		numNa[i+1] <- nrow(db[rowSums(is.na(db))==i,])
	}
	# numNa/perc
	numNa
}

minNA <- function(db){
	nc <- ncol(db)
	# perc <- nrow(db)
	numNa <- numeric(nc)
	for(i in 0:nc){
		numNa[i+1] <- nrow(db[rowSums(is.na(db))>=i,])
	}
	# numNa/perc
	numNa
}