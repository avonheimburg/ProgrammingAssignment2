## The functions work together to cache the computation of the inverse of a matrix.
## makeCacheMatrix() creates a structure to store the matrix and its inverse
## cacheSolve computes the inverse and caches it, or returns the cached matrix

## Create a structure for a matrix and its inverse
## The set()-method returned will also reset the inverse
## so we do not accidentally return stale data

makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  set <- function(newValue) {
    x <<- newValue
    cachedInverse <<- NULL
  }
    
  get <- function() x
  
  getinverse <- function() cachedInverse
  setinverse <- function(inverse) {
    cachedInverse <<- inverse
  }
  
  # finally, return all public functions as a list
  list(get = get, set = set, getinverse = getinverse, setinverse = setinverse)

}


## Checks if there is a cached inverse
## If yes, returns it directly
cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
