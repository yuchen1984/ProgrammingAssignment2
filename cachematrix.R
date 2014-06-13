## The codes below provide an example implementation of matrix inverse function
## with a cache. Matrix inversion is usually a costly computation. This cache
## inverse allow store and reuse the previous matrix inverse result and avoid
## repetitive computation.

## An extended data structure of matrix, which allows store and load the
## precomputed result of the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # set the original input matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  # load the original input matrix
  get <- function() x
  
  # store the matrix inverse from external precomputation
  setInverse <- function(inverse) inv <<- inverse
  
  # load the cached matrix inverse
  getInverse <- function() inv
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## A cached version of matrix inverse. It will reuse result the precomputed
## matrix inverse if possible.
## The input of the function must be a cacheMatrix
## It returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
  ## Try load the inverse from the cache
  inv <- x$getInverse()
  if(!is.null(inv))
  {
    # Found the result in the cache.
    message("getting cached data")
    return(inv)
  }
  ## If there is a cache missing, we recompute the inverse and cache the result
  ## by setInverse
  data <- x$get()
  m <- solve(data, ...)
  x$setInverse(m)
  m
}
