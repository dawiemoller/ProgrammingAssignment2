## Utility functions used for computing the inverse of a matrix in an efficient
## manner by utilizing a caching methodology


## makeCacheMatrix
## Input: A symmetrical invertible matrix
## Output:  A list providing access to four getter/setter methods
## Summary: Utility function which caches a matrix and its inverse.
## The getter and setter methods serve to get/set the value of the matrix as
## well as get/set the value of  the vector's inverse
## Internally these matrices are cached by means of the <<- operator which
## stores the values in an environment different from the current environment
makeCacheMatrix <- function(inputMatrix = matrix()) {
  inverseMatrix <- NULL
  
  set <- function(input){
    inputMatrix <<- input
    inverseMatrix <<- NULL
  }
  
  get <- function() inputMatrix
  setInverse <- function(inverse) inverseMatrix <<- inverse
  getInverse <- function() inverseMatrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve
## Input: A list containing four elements which are expected to be getter/setter
## methods provided by the output of the makeCacheMatrix method
## Output: The inverse of a matrix passed to the makeCacheMatrix method
## Summary: Checks for the existence of an inverse value in the cache provided
## by the makeCacheMatrix method and if a cached value is available it is
## returned.  If a cached value is not available the inverse is calculated and
## cached via the setter provided by makeCacheMatrix
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data)
  x$setInverse(inverse)
  inverse
}
