## Caching the inverse of a matrix
## Following functions help to minimize the cost for computing a inverse of a matrix 
## by creating a object which can stores matrix and cache its inverse.  

## A object called 'matrix' which is used to cache the inverse, is created by this functions. 

makeCacheMatrix <- function(x = matrix()){
  Inverse <- NULL
  set <- function(y){
    x <<- y
    Inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inv_matrix) Inverse <<- inv_matrix
  getInverse <- function() Inverse
  list(set = set, 
       get=get, 
       setInverse = setInverse, 
       getInverse = getInverse)
}

## Following function helps to compute the inverse of the above created matrix.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  Inverse <- x$getInverse()
  if(!is.null(Inverse)){
    message("Getting Cashed Data..")
    return(Inverse)
  }
  Data <- x$get()
  Inverse <- solve(Data, ...)
  x$setInverse(Inverse)
  Inverse
}

