## This file contains two functions to work with a matrix object
## that can cache its inverse

## create a special matrix object
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  #set function that will allow to set the matrix within the object
  set <- function(y){
    m <<- y
    i <<- NULL
  }
  #get function that returns the matrix
  get <- function() m
  #setinverse function that sets the inverse
  setinverse <- function(inverse) i <<- inverse
  #returns the inverse
  getinverse <- function() i
  
  #return a list with the above 4 functions
  list(set=set, get=get,setinverse=setinverse,getinverse=getinverse)
}


## retrieve cache out of matrix object and set inverse cache
## if it was not yet available
cacheSolve <- function(x, ...) {
  #define variable to contain result
  res <- NULL
  #get inverse of the x object
  i <- x$getinverse()
  
  #set res to inverse if it was already defined
  #otherwise calculate it and set it in the object
  if(!is.null(i)){
    message("getting cached data")
    res <- i
  } else {
    data <- x$get()
    i <- solve(data,...)
    x$setinverse(i)
    res <- i
  }
  return(res)
}
