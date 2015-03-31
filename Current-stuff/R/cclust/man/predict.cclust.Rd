\name{predict.cclust}
\title{Assign clusters to new data}
\usage{
\method{predict}{cclust}(object, newdata, \dots)
}
\alias{predict.cclust}
\arguments{
 \item{object}{Object of class \code{"cclust"} returned by a clustering algorithm such as \code{\link{cclust}}}
 \item{newdata}{Data matrix where columns correspond to variables and
   rows to observations}
  \item{...}{currently not used}
}
\description{
  Assigns each data point (row in \code{newdata}) the cluster corresponding to
  the closest center found in \code{object}.
}
\value{
  \code{predict.cclust} returns an object of class \code{"cclust"}.
  Only \code{size} is changed as compared to the argument
  \code{object}. 
  \item{cluster}{Vector containing the indices of the clusters where
    the data is mapped.}
  \item{size}{The number of data points in each cluster.}
}
\author{Evgenia Dimitriadou}
\seealso{\code{\link{cclust}}}
\examples{
# a 2-dimensional example
x<-rbind(matrix(rnorm(100,sd=0.3),ncol=2),
         matrix(rnorm(100,mean=1,sd=0.3),ncol=2))
cl<-cclust(x,2,20,verbose=TRUE,method="kmeans")
plot(x, col=cl$cluster)   

# a 3-dimensional example
x<-rbind(matrix(rnorm(150,sd=0.3),ncol=3),
         matrix(rnorm(150,mean=1,sd=0.3),ncol=3),
         matrix(rnorm(150,mean=2,sd=0.3),ncol=3))
cl<-cclust(x,6,20,verbose=TRUE,method="kmeans")
plot(x, col=cl$cluster)

# assign classes to some new data
y<-rbind(matrix(rnorm(33,sd=0.3),ncol=3),
         matrix(rnorm(33,mean=1,sd=0.3),ncol=3),
         matrix(rnorm(3,mean=2,sd=0.3),ncol=3))
ycl<-predict(cl, y)
plot(y, col=ycl$cluster)
}
\keyword{cluster}



