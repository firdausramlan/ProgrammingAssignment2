## These functions used to create a matrix and solve its inverse. 
## Inverse will be stored in cache so that subsequence call to the function will return cached result instead of solving it again.


## This function use to build matrix and expose setters and getters for matrix and its matrix. 
## Upon set a matrix, it will invalidate cached inverse if any.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inv) inverse <<- inv
  getinverse <- function() inverse
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function solve the inverse matrix created from `makeCacheMatrix` function.
## The inverse will be stored using `x$setinverse`
## Subsequence call to this function  will return the cached inverse stored previously if any.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inverse <- x$getinverse()
  if(!is.null(inverse)){
    message("Returning cached inverse")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(inverse)
  inverse
}
