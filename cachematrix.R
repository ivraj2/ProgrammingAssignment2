## Pair of utility methods for calculating matrix inverse with
## the solve(M) -function. The solutions are cached, so that
## the inverse of any given matrix is calculated only once. 

## Example usage:
## f1 <- makeCacheMatrix(matrix(c(6,7,8,9),2,2))
## m1 <- cacheSolve(f1)


## This function returns lisf of functions for calculating
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
  if(!is.null(m)) { ## Already calculated, so just return from cache
    message("Using cached data")
    return(m)
  }else{
    message("Calculating inverse...")
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m) ## Add result to cache
  m
  
}
