library(mice)


data <- read.csv("miceTestDb64k.csv", header=TRUE)
nrow(data)

t1 <- proc.time()

imp <- mice(data)


t2 <- proc.time() - t1

minutes <- t2[3]/60

hours <- minutes/60

name <- paste("results64k", Sys.time(),".txt", sep = "")

write(x=minutes, file=name)



