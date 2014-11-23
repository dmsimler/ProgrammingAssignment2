## Put comments here that give an overall description of what your
## functions do

## Dave updated the file

## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  
  ## Check to see if the inverse of the matrix in question is in the cache
  inverse <- NULL
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


