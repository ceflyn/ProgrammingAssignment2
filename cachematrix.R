## Functions to calculate the inverse of a matrix, and store the result so the 
## calculation only needs to be performed once

## Creates a list of functions that can be used to cache the inverse of the matrix x

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(matinverse) inv <<- matinverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## calculates the inverse of CacheMatrix x, and caches the result to avoid running 
## the calculation multiple times

cacheSolve <- function(x, ...) {
  inv_mat<-x$getinv()
  if(!is.null(inv_mat)){
    return(inv_mat)
  }
  data <- x$get()
  inv_mat <- solve(data, ...)
  x$setinv(inv_mat)
  inv_mat
}
