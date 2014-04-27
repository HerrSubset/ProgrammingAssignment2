## This file contains two functions to work with a matrix object
## that can cache its inverse

## create a special matrix object
makeCacheMatrix <- function(sourcematrix = matrix()) {
  matrixinverse <- NULL
  #set function that will allow to set the matrix within the object
  set <- function(y){
    sourcematrix <<- y
    matrixinverse <<- NULL
  }
  #get function that returns the matrix
  get <- function() sourcematrix
  #setinverse function that sets the inverse
  setinverse <- function(inverse) matrixinverse <<- inverse
  #returns the inverse
  getinverse <- function() matrixinverse
  
  #return a list with the above 4 functions
  list(set=set, get=get,setinverse=setinverse,getinverse=getinverse)
}


## retrieve cache out of matrix object and set inverse cache
## if it was not yet available
cacheSolve <- function(x, ...) {
  #define variable to contain result
  res <- NULL
  #get inverse of the x object
  inverse <- x$getinverse()
  
  #set res to inverse if it was already defined
  #otherwise calculate it and set it in the object
  if(!is.null(inverse)){
    message("getting cached data")
    res <- inverse
  } else {
    data <- x$get()
    inverse <- solve(data,...)
    x$setinverse(inverse)
    res <- inverse
  }
  return(res)
}
