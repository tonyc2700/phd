\name{clustIndex}
\title{Cluster Indexes}
\usage{ clustIndex ( y, x, index = "all" ) }
\alias{clustIndex}
\arguments{
\item{y}{Object of class \code{"cclust"} returned by a clustering algorithm such as \code{\link{kmeans}}}
\item{x}{Data matrix where columns correspond to variables and rows to observations}
\item{index}{The indexes that are calculated \code{"calinski"},
  \code{"cindex"}, \code{"db"}, \code{"hartigan"}, \code{"ratkowsky"},
  \code{"scott"}, \code{"marriot"}, \code{"ball"}, \code{"trcovw"},
  \code{"tracew"}, \code{"friedman"}, \code{"rubin"}, \code{"ssi"},
  \code{"likelihood"}, and \code{"all"} for all the
  indexes. Abbreviations of these names are also accepted.}
}

\description{
  \code{y} is the result of a clustering algorithm of class such
  as \code{"cclust"}.
  This function is calculating the values of several clustering
  indexes. The values of the indexes can be independently used in order
  to determine the number of clusters existing in a data set.
}

\details{
  The description of the indexes is categorized into 3 groups, based
  on the statistics mainly used to compute them.
  
  The first group is based on the sum of squares within (\eqn{SSW})
  and between (\eqn{SSB}) the clusters. These statistics measure the
  dispersion of the data points in a cluster and between the clusters
  respectively. These indexes are:
  \describe{
    \item{\bold{calinski}:}{
      \eqn{(SSB/(k-1))/(SSW/(n-k))}, where \eqn{n} is the number of
      data points and \eqn{k} is the number of clusters.
    }
    \item{\bold{hartigan}:}{then \eqn{\log(SSB/SSW)}.}
    \item{\bold{ratkowsky}:}{
      \eqn{mean(\sqrt{(varSSB/varSST)})}, where \eqn{varSSB} stands
      for the \eqn{SSB} for every variable and \eqn{varSST} for the
      total sum of squares for every variable.
    }
    \item{\bold{ball}:}{
      \eqn{SSW/k}, where \eqn{k} is the number of clusters.
    }
  }
  The second group is based on the statistics of \eqn{T}, i.e., the
  scatter matrix of the data points, and \eqn{W}, which is the sum of the
  scatter matrices in every group. These indexes are:
  \describe{
    \item{\bold{scott}:}{
      \eqn{n\log(|T|/|W|)}, where \eqn{n} is the number of data points
      and \eqn{|\cdot|} stands for the determinant of a matrix.}
    \item{\bold{marriot}:}{
      \eqn{k^2 |W|}, where \eqn{k} is the number of clusters.}
    \item{\bold{trcovw}:}{\eqn{Trace Cov W}.}
    \item{\bold{tracew}:}{\eqn{Trace W}.}
    \item{\bold{friedman}:}{
      \eqn{Trace W^{(-1)} B}, where \eqn{B} is the scatter matrix of
      the cluster centers.}
    \item{\bold{rubin}:}{\eqn{|T|/|W|}.}
  }
  The third group consists of four algorithms not belonging to the
  previous ones and not having anything in common.
  \describe{
    \item{\bold{cindex}:}{
      if the data set is binary, then while the C-Index is a cluster
      similarity measure, is expressed as:\cr
      \eqn{[d_{(w)}-\min(d_{(w)})]/[\max(d_{(w)})-\min(d_{(w)})]},
      where \eqn{d_{(w)}} is the sum of all \eqn{n_{(d)}} within
      cluster distances, \eqn{\min(d_{(w)})} is the sum of the
      \eqn{n_{(d)}} smallest pairwise distances in the data set, and
      \eqn{\max (d_{(w)})} is the sum of the \eqn{n_{(d)}} biggest
      pairwise distances.  In order to compute the C-Index all
      pairwise distances in the data set have to be computed and
      stored.  In the case of binary data, the storage of the
      distances is creating no problems since there are only a few
      possible distances.  However, the computation of all distances
      can make this index prohibitive for large data sets.}
    \item{\bold{db}:}{
      \eqn{R=(1/n)*sum(R_{(i)})}
      where \eqn{R_{(i)}} stands for the maximum value of
      \eqn{R_{(ij)}} for \eqn{i\neq j}, and \eqn{R_{(ij)}} for
      \eqn{R_{(ij)}=(SSW_{(i)}+SSW_{(j)})/DC_{(ij)}}, where
      \eqn{DC_{(ij)}} is the 
      distance between the centers of two clusters \eqn{i, j}.}
    \item{\bold{likelihood}:}{
      under the assumption of
      independence of the variables within a cluster, a cluster solution
      can be regarded as a mixture model for the data, where the cluster
      centers give the probabilities for each variable to be
      \eqn{1}. Therefore, the negative Log-likelihood can be computed and
      used as a quantity measure for a cluster solution. Note that the
      assumptions for applying special penalty terms, like in AIC or BIC,
      are not fulfilled in this model, and also they show no effect for
      these data sets.}
    \item{\bold{ssi}:}{this ``Simple Structure Index''
      combines three elements which influence the interpretability of a
      solution, i.e., the maximum difference of each variable between the
      clusters, the sizes of the most contrasting clusters and the
      deviation of a variable in the cluster centers compared to its
      overall mean. These three elements are multiplicatively combined and
      normalized to give a value between \eqn{0} and \eqn{1}.}
  }
}

\value{
    Returns an vector with the indexes values.
}
\references{Andreas Weingessel, Evgenia Dimitriadou and Sara Dolnicar,
    An Examination Of Indexes For Determining The Number
    Of Clusters In Binary Data Sets,\cr
    \url{http://www.wu-wien.ac.at/am/wp99.htm#29}\cr
    and the references therein.
}
\author{Evgenia Dimitriadou and Andreas Weingessel
}

\seealso{\code{\link{cclust}}, \code{\link{kmeans}}}

\examples{
# a 2-dimensional example
x<-rbind(matrix(rnorm(100,sd=0.3),ncol=2),
         matrix(rnorm(100,mean=1,sd=0.3),ncol=2))
cl<-cclust(x,2,20,verbose=TRUE,method="kmeans")
resultindexes <- clustIndex(cl,x, index="all")
resultindexes   
}
\keyword{cluster}





