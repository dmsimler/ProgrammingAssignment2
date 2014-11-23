## ############################################################
## This script contains the following functions:
## makeCacheMatrix - This function creates a special "matrix" object 
##                   that can cache its inverse.
## cacheSolve - This function computes the inverse of the special 
##              "matrix" returned by makeCacheMatrix. 
##              If the inverse has already been calculated (and 
##              the matrix has not changed), then the cachesolve 
##              retrieves the inverse from the cache.
## ############################################################


## ############################################################
## function makeCacheMatrix:
## This function takes a matrix (x) as input and returns a list 
## of 4 functions that are used to manage a cache of the inverse
## of that input matrix x.  These functions are as follows:
## get - returns the value of the input matrix x
## set - sets the value of input matrix x
## setinverse - sets the  value of the inverse of the input matrix x
## getinverse - returns the value of the inverse of the input matrix x.
##
## Note: The input matrix x is assumed to be invertible and therefore 
##       square. 
## ############################################################
makeCacheMatrix <- function(x = matrix()) {
  ## initialize the cached inverse value to nothing
  cached_inverse <- NULL
  
  ## Create the definition of the set function which just initializes
  ## the 
  set <- function(y) {
    x <<- y
    cached_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) cached_inverse <<- inverse
  getinverse <- function() cached_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## ############################################################
## function cacheSolve:
## This function takes a "special matrix" x (assumed to be returned
## by the function makeCacheMatrix defined above) which is a list 
## containing four functions that manage caching the inverse of
## of a square matrix, and returns the inverse of a given input
## matrix.  If the inverse is in the cache, that value is returned
## otherwise it is calculated, set in the cache, and then returned
## to the caller.
## ############################################################
cacheSolve <- function(x, ...) {
  
  ## Check to see if the inverse of the matrix in question is in the cache
  inverse <- x$getinverse()
  
  if(!is.null(inverse)) {
    ## There is a valid value for the inverse in the cache, so write out a message
    ## and return the value in the cache to the caller
    message("getting cached inverse of the matrix")
    return(inverse)
  }
  
  ## Made it to here, so that means the inverse wasn't found in the cache and that we'll 
  ## need to calculate it
  
  ## Get the original matrix data
  matrix_data <- x$get()
  
  ##Calculate the inverse of the matrix
  inverse <- solve(matrix_data, ...)
  
  ## Set the newly calculated value of the inverse in the cache
  x$setinverse(inverse)
  
  ## Return the calcalulated inverse to the caller
  return(inverse)
}



