## Matrix inversion is usually a costly computation and there may be some
## benefit to caching the inverse of a matrix rather than 
## compute it repeatedly. This functions solve this problem.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  inverz <- NULL
  set <- function(y) {
    x <<- y
    inverz <<- NULL
  }
  get <- function() x
  setInv <- function(Inverzea) inverz <<- Inverzea
  getInv <- function() inverz
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverz <- x$getInv()
  if(!is.null(inverz)) {
    message("getting cached data")
    return(inverz)
  }
  data <- x$get()
  inverz <- solve(data)
  x$setInv(inverz)
  inverz
}
