
data <- read.csv("danish-data.csv")

data.smk <- data[,1:2]
data$heavy.S <- NULL
data$cont.S <- NULL

getBest <- function(data){
	tri1 <- data.frame(matrix(ncol = 2, nrow = nrow(data)))
	tri1[,1] <- replicate(nrow(data),110.0)
	tri2 <- tri1
	for (i in 1:nrow(data)) {
		row <- data[i,]
		for (j in 1:7) {
			tempTri1 <- abs(row[j*7-5]-20.0)
			tempTri2 <- abs(row[j*7-5]-32.0)
			if(tempTri1 <= tri1[i,1] && tempTri1 < 9){
				tri1[i,1] <- tempTri1
				tri1[i,2] <- j
			}
			if(tempTri2 <= tri2[i,1] && row[j*7-5] > 28){
				tri2[i,1] <- tempTri2
				tri2[i,2] <- j
			}
		}
	}
	returnList <- list(tri1[,1], tri1[,2],  tri2[,1], tri2[,2])
	return(returnList)
}

getData <- function(db,dn){
	data <- db
	tmp <- db[1:7]
	num <- dn
	for(i in 1:nrow(data)){
		row <- data[i,]
		if(!is.na(num[i])){
			j <- num[i]
			tmp[i,] <- row[(j*7-6):(j*7)]
		}
		if(is.na(num[i])){
			tmp[i,] <- replicate(7, NA)
		}
	}
	return(tmp)
}


allBest <- getBest(data)
data.tri1num <- allBest[[2]]
data.tri2num <- allBest[[4]]
data.1Tri <- getData(data, data.tri1num)
data.2Tri <- getData(data, data.tri2num)



cleanse <- function(data){
	tri1 <- data.frame(matrix(ncol = 2, nrow = nrow(data)))
	tri1[,1] <- replicate(nrow(data),100.0)
	tri2 <- data.frame(matrix(ncol = 2, nrow = nrow(data)))
	tri2[,1] <- replicate(nrow(data),100.0)
	tempData <- data
	for(i in 1:(nrow(data))){
		row <- data[i,]
		for(j in 1:(ncol(row)/7)){
			tempTri1 <- abs(row[,(j*7-5)]-20)
			tempTri2 <- abs(row[,(j*7-5)]-32)
			if(tempTri1<tri1[i,1] && tempTri1>11 && tempTri1<29){
				tri1[i,1] <- tempTri1
				tri1[i,2] <- j
			}
			if(tempTri2<tri2[i,1] && tempTri2>28){
				tri2[i,1] <- tempTri2
				tri2[i,2] <- j
			}
		}
	}
	returnList <- list("tri1"= tri1, "tri2" = tri1)
	return(returnList)
}


cleanse2 <- function(data){
	tri1 <- data.frame(matrix(ncol = 2, nrow = nrow(data)))
	tri2 <- data.frame(matrix(ncol = 2, nrow = nrow(data)))
	for(i in 1:(nrow(data))){
		row <- data[i,]
		for(j in 1:(ncol(row)/7)){
			tempTri1 = abs(row[,(j*7-5)]-20)
			tempTri2 = abs(row[,(j*7-5)]-32)
			if(tempTri1 < tri1[i,j]){
				tri1[i,j] = tempTri1

			}
		}
	}
}


cleanse3 <- function(data){
	# tri1 <- NULL
	# tri2 <- NULL
	tempData <- data
	for(i in 1:(nrow(data))){
		row <- data[i,]
		tempTri1 <- which.min(abs(row-20.0))
		tempTri2 <- which.min(abs(row-32.0))
		if(tempTri1 > 11 && tempTri1 < 28 && tempTri2 > 28){

		}
	}
	return(tri1)
	return(tri2)
}



