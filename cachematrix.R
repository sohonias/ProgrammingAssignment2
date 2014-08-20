## This file contains two functions. Primary goal of this pair of functions
## is to cache a value that is computationally expensive(ex: inverting a matrix) 
## and to use it when needed. The first function 'makeCacheMatrix' prepares 
## an environment in which values can be cached/retrieved and the second function 
## 'cacheSolve' leaverages the same.
## Assumption: The input-matrix is always invertible.

## makeCacheMatrix accepts and stores a matrix with which to work. It also extends
## the functionality by adding getters and setters for the matrix and its inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(i) inv <<- i
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
  
}


## cacheSolve function accepts a special matrix created by makeCacheMatrix.
## It checks if the inverse of this special matrix is already calculated
## If inverse is available it is returned. Otherwise, this function would 
## compute the inverse, cache it (for later use) and return it.
## 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getInv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data)
  x$setInv(i)
  i
}
