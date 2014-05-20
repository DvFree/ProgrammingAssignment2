## This pair of functions will invert a matrix using minimal power by not repeating the computation.
## The functions take advantage of the scoping rules of R.

##makeCacheMatrix() creates a list composed of 4 functions: set(), get(), setInverse(), and getInverse()
##set() will set the value of the matrix
##get() will get the value of the matrix
##setInverse() will set the value of the inverse
##getInverse() will get the value of the inverse, whether computed or found in the cache

makeCacheMatrix <- function(m = matrix()) {
  mInverse <- NULL
  
  set <- function(y){
    m <<- y
    mInverse <<- NULL 
  }
  get <- function() m
  setInverse <- function(inverse) mInverse <<- inverse
  getInverse <- function() mInverse
  
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse
       )
}

## cacheSolve will compute, cache, and return the inverse of the passed-in matrix.
## First, it checks if the inverse has already been calculated and is in the cache.
## If it is, cacheSolve returns the result from the cache and prints a message saying the data came from the cache.
## If the inverse hasn't yet been solved and cached, cacheSolve will do that and return the inverse.

cacheSolve <- function(x, ...) {
  mInverse <- x$getInverse()
  
  if(!is.null(mInverse)) {
    message("getting cached data")
    return(mInverse)
  }
  
  data <- x$get()
  mInverse <- solve(data, ...)
  
  x$setInverse(mInverse)
  mInverse
}
