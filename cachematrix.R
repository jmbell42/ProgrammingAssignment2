## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This will create a "matrix" list to be used
## by the cacheSolve.R routine
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Write a short comment describing this function
## This routine will calculate the inverse of a Matrix given by
## the routine makeCacheMatrix.R.
## if the Inverse has already been calculated and stored in cache, then it will
## use the Cached data to save time.
## NOTE:  The routine will notify the user if Cached inverse data exists.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)){
    message("getting cached inverse of the matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
}
