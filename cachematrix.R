## makeCacheMetrix and cacheSolve are functions for matrix inverse  calculation
## with caching of results. Matrix inverse calculations are slow for large matrixes.
## Caching the results improves the performance of the calculation. Caching improves 
## performance if you are calculating matrix inverse for the same matrix multiple times.

## makeCacheMatrix creates a matrix container list. The created list contains 
## get and set functions for storing and retrieving of the matrix. getinverse and setinverse
## functions get or set the matrix inverse cached value.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve calculates matrix inverse of the x matrix. This function
## first checks if there is a cached inverse. If there is a cached inverse then
## cacheSolve returns the cached value immediately. If there is no cached inverse 
## it computes the inverse first, stores it in the cache, and returns the inverse value. 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
