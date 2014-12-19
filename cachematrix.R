## The makeCacheMatrix and cacheSolve functions are designed to cache 
## the inverse of a matrix

## This function creates a special "matrix" object that can cache 
## its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  setmat <- function(y){
    x <<- y
    inv <<- NULL
  }
  getmat <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(setmat=setmat, getmat=getmat, setinv=setinv, getinv=getinv )
}


## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$getmat()
  inv <- solve(mat)
  x$setinv(inv)
  inv
}
