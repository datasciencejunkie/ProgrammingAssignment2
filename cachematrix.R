## The functions compute the inverse of a matrix only for the first
## time and return inverse as a cached value subsequently. This
## approach offers great performance benefit as the expensive
## computation will not be repeated for each subsequent calls to the
## function.

## Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function(z) {
    m <<- z
  }
  
  getinverse <- function() m
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Computes the inverse of the special "matrix" returned by makeCacheMatrix().
## If the inverse has already been calculated (and the matrix has not changed),
## then the function retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  
  m  
}