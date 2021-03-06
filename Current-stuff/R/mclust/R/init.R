#############################################################################
## Initialization for d-dim data ############################################
#############################################################################

# old original (default)
# hc <- function(modelName = mclust.options("hcModelNames")[1], data, ...)
# {
#   switch(EXPR = modelName,
#          E = ,
#          V = ,
#          EII  = ,
#          VII = ,
#          EEE = ,
#          VVV = TRUE,
#          stop("invalid model name for hierarchical clustering"))
#   funcName <- paste("hc", modelName, sep = "")
#   mc <- match.call(expand.dots = TRUE)
#   mc[[1]] <- as.name(funcName)
#   mc[[2]] <- NULL
#   eval(mc, parent.frame())
# }

# new
hc <- function(modelName = mclust.options("hcModelNames")[1], 
               data, use = mclust.options("hcUse"), ...)
{
  switch(EXPR = modelName,
         E = ,
         V = ,
         EII  = ,
         VII = ,
         EEE = ,
         VVV = TRUE,
         stop("invalid model name for hierarchical clustering"))
  funcName <- paste("hc", modelName, sep = "")
  mc <- match.call(expand.dots = TRUE)
  mc$use <- NULL
  data <- data.matrix(data)

  use <- toupper(use)
  if(use == "RANDOM")
    { return(randomPairs(data, ...)) }
  switch(use,
         "VARS" = { Z <- data },
         "STD" = { Z <- scale(data, center = TRUE, scale = TRUE) },
         "PCR" = { data <- scale(data, center = TRUE, scale = TRUE)
                   SVD <- svd(data, nu=0)
                   # evalues <- sqrt(SVD$d^2/(nrow(data)-1))
                   Z <- data %*% SVD$v },
         "PCS" = { data <- scale(data, center = TRUE, scale = FALSE)
                   SVD <- svd(data, nu=0)
                   # evalues <- sqrt(SVD$d^2/(nrow(data)-1))
                   Z <- data %*% SVD$v },
         "SPH" = { data <- scale(data, center = TRUE, scale = FALSE)
                   n <- nrow(data)
                   Sigma <- var(data) * (n - 1)/n
                   SVD <- svd(Sigma, nu = 0)
                   Z <- data %*% SVD$v %*% diag(1/sqrt(SVD$d)) },
         "SVD" = { data <- scale(data, center = TRUE, scale = TRUE)
                   n <- nrow(data)
                   SVD <- svd(data, nu=0)
                   Z <- data %*% SVD$v %*% diag(1/sqrt(SVD$d)) },
         stop("'use' argument not allowed. See help(mclust.options)")
  )
  
  mc$data <- Z 
  mc[[1]] <- as.name(funcName)
  mc[[2]] <- NULL
  eval(mc, parent.frame())
}

# Create a hierarchical structure using a random partition of the data
# See help(hc, package = "mclust")
# Usage:
# Mclust(data, initialization = list(hcPairs = randomPairs(data)))

randomPairs <- function(data, seed = NULL, ...)
{
  if(!is.null(seed)) set.seed(seed)
  data <- as.matrix(data)
  n <- nrow(data)
  tree <- matrix(sample(1:n, n, replace = FALSE), nrow = 2, ncol = ceiling(n/2))
  tree <- apply(tree, 2, sort)
  ind <- unique(tree[1,])
  while(ncol(tree) < (n-1))
  { 
    addtree <- sort(sample(ind, size = 2, replace = FALSE))
    ind <- setdiff(ind, addtree[2])
    tree <- cbind(tree, addtree)
  }
  dimnames(tree) <- NULL
  structure(tree, initialPartition = 1:n, dimensions = c(n,2))
}


hclass <- function(hcPairs, G)
{
  initial <- attributes(hcPairs)$init
  n <- length(initial)
  k <- length(unique(initial))
  G <- if(missing(G)) k:2 else rev(sort(unique(G)))
  select <- k - G
  if(length(select) == 1 && !select)
    return(matrix(initial, ncol = 1, dimnames = list(NULL, 
                                                     as.character(G))))
  bad <- select < 0 | select >= k
  if(all(bad))
    stop("No classification with the specified number of clusters")
  if(any(bad))
    warning("Some selected classifications are inconsistent\n                          with mclust object"
    )
  L <- length(select)
  cl <- matrix(as.double(NA), nrow = n, ncol = L, dimnames = list(NULL, as.character(
    G)))
  if(select[1])
    m <- 1
  else {
    cl[, 1] <- initial
    m <- 2
  }
  for(l in 1:max(select)) {
    ij <- hcPairs[, l]
    i <- min(ij)
    j <- max(ij)
    initial[initial == j] <- i
    if(select[m] == l) {
      cl[, m] <- initial
      m <- m + 1
    }
  }
  apply(cl[, L:1, drop = FALSE], 2, partconv, consec = TRUE)
}

hcEII <- function(data, partition, minclus = 1, ...)
{
  if(minclus < 1) stop("minclus must be positive")
  if(any(is.na(data)))
    stop("missing values not allowed in data")
  #====================================================================
  dimdat <- dim(data)
  oneD <- is.null(dimdat) || length(dimdat[dimdat > 1]) == 1
  if(oneD || length(dimdat) > 2)
    stop("data should in the form of a matrix")
  data <- as.matrix(data)
  dimnames(data) <- NULL
  n <- nrow(data)
  p <- ncol(data)
  if(missing(partition))
    partition <- 1:n
  else if(length(partition) != n)
    stop("partition must assign a class to each observation")
  partition <- partconv(partition, consec = TRUE)
  l <- length(unique(partition))
  attr(partition, "unique") <- l
  m <- l - minclus
  if(m <= 0)
    stop("initial number of clusters is not greater than minclus")
  if(n <= p)
    warning("# of observations <= data dimension")
  #=============================================================
  storage.mode(data) <- "double"
  ld <- max(c((l * (l - 1))/2, 3 * m))
  temp <- .Fortran("hceii",
                   data,
                   as.integer(n),
                   as.integer(p),
                   as.integer(partition),
                   as.integer(l),
                   as.integer(m),
                   double(p),
                   as.integer(ld),
                   double(ld),
                   PACKAGE = "mclust")[c(1, 9)]
  temp[[1]] <- temp[[1]][1:m, 1:2, drop = FALSE]
  temp[[2]] <- temp[[2]][1:m]
  change <- temp[[2]]
  structure(t(temp[[1]]), initialPartition = partition, 
            dimensions = dimdat, modelName = "EII", 
            call =  match.call())
}

hcEEE <- function(data, partition, minclus = 1, ...)
{
  if(minclus < 1) stop("minclus must be positive")
  if(any(is.na(data)))
    stop("missing values not allowed in data")
  #=====================================================================
  dimdat <- dim(data)
  oneD <- is.null(dimdat) || length(dimdat[dimdat > 1]) == 1
  if(oneD || length(dimdat) > 2)
    stop("data should in the form of a matrix")
  data <- as.matrix(data)
  dimnames(data) <- NULL
  n <- nrow(data)
  p <- ncol(data)
  if(n <= p)
    warning("# of observations <= data dimension")
  if(missing(partition))
    partition <- 1:n
  else if(length(partition) != n)
    stop("partition must assign a class to each observation")
  partition <- partconv(partition, consec = TRUE)
  l <- length(unique(partition))
  attr(partition, "unique") <- l
  m <- l - minclus
  if(m <= 0)
    stop("initial number of clusters is not greater than minclus")
  storage.mode(data) <- "double"
  
  ## R 2.12.0: 32 bit Windows build fails due to compiler bug
  ## workaround: removal (hopefully temporary) of hc functionality for EEE
  
  # Luca: commente the next line and uncommented below
  #  stop("hc for EEE model is not currently supported")
  
  temp <- .Fortran("hceee",
                   data,
                   as.integer(n),
                   as.integer(p),
                   as.integer(partition),
                   as.integer(l),
                   as.integer(m),
                   if(p < 3) integer(m) else integer(1),
                   if(p < 4) integer(m) else integer(1),
                   double(p),
                   double(p * p),
                   double(p * p),
                   double(p * p),
                   PACKAGE ="mclust")[c(1, 7:10)]
  #
  # currently temp[[5]] is not output
  temp[[4]] <- temp[[4]][1:2]
  temp[[5]] <- temp[[5]][1:2]
  names(temp[[5]]) <- c("determinant", "trace")
  temp[[1]] <- temp[[1]][1:(m + 1),  ]
  if(p < 3)
    tree <- rbind(temp[[2]], temp[[3]])
  else if(p < 4)
    tree <- rbind(temp[[1]][-1, 3], temp[[3]])
  else tree <- t(temp[[1]][-1, 3:4, drop = FALSE])
  determinant <- temp[[1]][, 1]
  attr(determinant, "breakpoints") <- temp[[4]]
  trace <- temp[[1]][, 2]
  structure(tree,  initialPartition = partition, 
            dimensions = dimdat, modelName = "EEE", 
            call = match.call())
}

hcVII <- function(data, partition, minclus = 1, alpha = 1, ...)
{
  if(minclus < 1) stop("minclus must be positive")
  if(any(is.na(data)))
    stop("missing values not allowed in data")
  #=====================================================================
  dimdat <- dim(data)
  oneD <- is.null(dimdat) || length(dimdat[dimdat > 1]) == 1
  if(oneD || length(dimdat) > 2)
    stop("data should in the form of a matrix")
  data <- as.matrix(data)
  dimnames(data) <- NULL
  n <- nrow(data)
  p <- ncol(data)
  if(n <= p)
    warning("# of observations <= data dimension")
  if(missing(partition))
    partition <- 1:n
  else if(length(partition) != n)
    stop("partition must assign a class to each observation")
  partition <- partconv(partition, consec = TRUE)
  l <- length(unique(partition))
  attr(partition, "unique") <- l
  m <- l - minclus
  if(m <= 0)
    stop("initial number of clusters is not greater than minclus")
  storage.mode(data) <- "double"
  ll <- (l * (l - 1))/2
  ld <- max(n, ll, 3 * m)
  alpha <- alpha * traceW(data/sqrt(n * p))
  alpha <- max(alpha, .Machine$double.eps)
  temp <- .Fortran("hcvii",
                   data,
                   as.integer(n),
                   as.integer(p),
                   as.integer(partition),
                   as.integer(l),
                   as.integer(m),
                   as.double(alpha),
                   double(p),
                   as.integer(ld),
                   double(ld),
                   PACKAGE = "mclust")[c(1, 10)]
  temp[[1]] <- temp[[1]][1:m, 1:2, drop = FALSE]
  temp[[2]] <- temp[[2]][1:m]
  change <- temp[[2]]
  structure(t(temp[[1]]), initialPartition = partition, 
            dimensions = dimdat, modelName = "VII", 
            call = match.call())
}

hcVVV <- function(data, partition, minclus = 1, alpha = 1, beta = 1, ...)
{
  if(minclus < 1) stop("minclus must be positive")
  if(any(is.na(data)))
    stop("missing values not allowed in data")
  dimdat <- dim(data)
  oneD <- is.null(dimdat) || length(dimdat[dimdat > 1]) == 1
  if(oneD || length(dimdat) > 2)
    stop("data should in the form of a matrix")
  data <- as.matrix(data)
  dimnames(data) <- NULL
  n <- nrow(data)
  p <- ncol(data)
  if(n <= p)
    warning("# of observations <= data dimension")
  if(missing(partition))
    partition <- 1:n
  else if(length(partition) != n)
    stop("partition must assign a class to each observation")
  partition <- partconv(partition, consec = TRUE)
  l <- length(unique(partition))
  attr(partition, "unique") <- l
  m <- l - minclus
  if(m <= 0)
    stop("initial number of clusters is not greater than minclus")
  storage.mode(data) <- "double"
  ll <- (l * (l - 1))/2
  #  dp <- duplicated(partition)
  #x[c((1:n)[!dp],(1:n)[dp]),], 
  #as.integer(c(partition[!dp], partition[dp])), 
  ld <- max(n, ll + 1, 3 * m)
  alpha <- alpha * traceW(data/sqrt(n * p))
  alpha <- max(alpha, .Machine$double.eps)
  temp <- .Fortran("hcvvv",
                   cbind(data, 0.),
                   as.integer(n),
                   as.integer(p),
                   as.integer(partition),
                   as.integer(l),
                   as.integer(m),
                   as.double(alpha),
                   as.double(beta),
                   double(p),
                   double(p * p),
                   double(p * p),
                   double(p * p),
                   as.integer(ld),
                   double(ld),
                   PACKAGE = "mclust")[c(1, 14)]
  temp[[1]] <- temp[[1]][1:m, 1:2, drop = FALSE]
  temp[[2]] <- temp[[2]][1:m]
  change <- temp[[2]] 
  structure(t(temp[[1]]), initialPartition = partition, 
            dimensions = dimdat, modelName = "VVV", 
            call = match.call())
}

## Initialization for 1-dim data ############################################

# This version is bugged when a quantile is equal to the following
qclass <- function (x, k) 
{
  q <- quantile(x, seq(from = 0, to = 1, by = 1/k))
  cl <- rep(0, length(x))
  q[1] <- q[1] - 1
  for(i in 1:k) 
    cl[x > q[i] & x <= q[i+1]] <- i
  return(cl)
}
# This should correct the above bug
qclass <- function (x, k) 
{
  x <- as.vector(x)
  eps <- sqrt(.Machine$double.eps)
  q <- NA
  n <- k
  while(length(q) < (k+1))
  { n <- n + 1
    q <- unique(quantile(x, seq(from = 0, to = 1, length = n))) 
  }
  if(length(q) > (k+1))
  { dq <- diff(q)
    nr <- length(q)-k-1
    q <- q[-order(dq)[1:nr]]
  }
  q[1] <- min(x) - eps
  q[length(q)] <- max(x) + eps
  cl <- rep(0, length(x))
  for(i in 1:k) 
  { cl[ x >= q[i] & x < q[i+1] ] <- i }
  return(cl)
}

hcE <- function(data, partition, minclus = 1, ...)
{
  if(minclus < 1) stop("minclus must be positive")
  if(any(is.na(data)))
    stop("missing values not allowed in data")
  #====================================================================
  dimdat <- dim(data)
  oneD <- is.null(dimdat) || length(dimdat[dimdat > 1]) == 1
  if(!oneD)
    stop("data must be one-dimensional")
  data <- as.vector(data)
  n <- length(data)
  if(missing(partition))
    partition <- 1:n
  else if(length(partition) != n)
    stop("partition must assign a class to each observation")
  partition <- partconv(partition, consec = TRUE)
  l <- length(unique(partition))
  attr(partition, "unique") <- l
  m <- l - minclus
  if(m <= 0)
    stop("initial number of clusters is not greater than minclus")
  storage.mode(data) <- "double"
  ld <- max(c((l * (l - 1))/2, 3 * m))
  temp <- .Fortran("hc1e",
                   data,
                   as.integer(n),
                   as.integer(partition),
                   as.integer(l),
                   as.integer(m),
                   as.integer(ld),
                   double(ld),
                   PACKAGE = "mclust")[c(1, 3, 7)]
  temp[[1]] <- temp[[1]][1:m]
  temp[[2]] <- temp[[2]][1:m]
  temp[[3]] <- temp[[3]][1:m]
  change <- temp[[3]]
  structure(rbind(temp[[1]], temp[[2]]),   initialPartition = partition, 
            dimensions = n, modelName = "E",
            call = match.call())
}

hcV <- function(data, partition, minclus = 1, alpha = 1, ...)
{
  if(minclus < 1) stop("minclus must be positive")
  if(any(is.na(data)))
    stop("missing values not allowed in data")
  #=====================================================================
  dimdat <- dim(data)
  oneD <- is.null(dimdat) || length(dimdat[dimdat > 1]) == 1
  if(!oneD)
    stop("data must be one-dimensional")
  data <- as.vector(data)
  n <- length(data)
  if(missing(partition))
    partition <- 1:n
  else if(length(partition) != n)
    stop("partition must assign a class to each observation")
  partition <- partconv(partition, consec = TRUE)
  l <- length(unique(partition))
  attr(partition, "unique") <- l
  m <- l - minclus
  if(m <= 0)
    stop("initial number of clusters is not greater than minclus")
  storage.mode(data) <- "double"
  alpha <- alpha * (vecnorm(data - mean(data))^2/n)
  alpha <- min(alpha, .Machine$double.eps)
  ld <- max(c((l * (l - 1))/2, 3 * m))
  temp <- .Fortran("hc1v",
                   data,
                   as.integer(n),
                   as.integer(partition),
                   as.integer(l),
                   as.integer(m),
                   as.double(alpha),
                   as.integer(ld),
                   double(ld),
                   PACKAGE = "mclust")[c(1, 3, 8)]
  temp[[1]] <- temp[[1]][1:m]
  temp[[2]] <- temp[[2]][1:m]
  temp[[3]] <- temp[[3]][1:m]
  change <- temp[[3]]
  structure(rbind(temp[[1]], temp[[2]]),   initialPartition = partition, 
            dimensions = n, modelName = "V",
            call = match.call())
}
