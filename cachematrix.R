## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Establish function to create an iverse of a given matrix
## allows for variables x and inverse to be retrieved from other function
## using <<-

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function () inv
  list(set =set, get=get, setInverse=setInverse, getInverse=getInverse)
  
}


## retrieves cache of the makeCacheMatrix function

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
    ## Return a matrix that is the inverse of 'x'
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}