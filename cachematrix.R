## Put comments here that give an overall description of what your
## functions do

## This utility function returns lisf of functions for calculating
## inverse of a matrix. Already solved matrices are cached
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This method depends on makeCacheMatrix functions for a list of 
## utility functions. It will first look for a cached solution for 
## a matrix, and if not found, it will try to solve the matrix and then
## add the solution into global cache. 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("Using cached data")
    return(m)
  }else{
    message("Calculating inverse...")
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  
}
