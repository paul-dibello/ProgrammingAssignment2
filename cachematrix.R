## The functions makeCacheMatrix creates a matrix of functions to cache data
## Essentially the functions are assessor methods to store and retrieve data to
## the cache.  The solveCache functions inverts the cached matrix and returns it.

## makeCache accepts a matrix to be cached.  Then creates sub functions to store
## and retrieve (set and get) both the original matrix (mtx) and its inverse 
## (inverseMatrix)
makeCacheMatrix <- function(x = matrix()) {
  inverseMatrix <<- NULL
  mtx <<- x
  
  set <- function(y) {
    mtx <<- y
    inverseMatrix <<- NULL
  }
  
  get <- function() mtx
  
  setInverse <- function(inverse) inverseMatrix <<- inverse
  
  getInverse <- function() {
    inverseMatrix
  }
  ## Create a matrix of functions to be returned.
  cells <- c(set,setInverse,get,getInverse)
  rnames <- c("set","get")
  cnames <- c("matrix","inverse")
  cacheMatrix <- matrix(cells, nrow=2, ncol=2, byrow=TRUE, dimnames=list(rnames, cnames))
  cacheMatrix
}


## cacheSolve takes in the function x (which is makeCacheMatrix above), checks
## if the inverse matrix is cached and then returns it, or gets the cached original
## matrix (which is passed as an arguement to makeCacheMatrix) and inverts it and 
## caches it.
##
## I've executed this by first makeing a matrix (d <- matrix(rnorm(16),4,4))
## and then calling cacheSolve(makeCacheMatrix(d))
##
## The inverse of d is returned.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  inverse <- x[["get","inverse"]]()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x[["get","matrix"]]()
  ##print(data)
  inverse <- solve(data)
  x[["set","inverse"]](inverse)
  inverse
  
}

