## Put comments here that give an overall description of what your functions do
## It is a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {                                 # 1. set the value of the matrix
    x <<- y
    inverse <<- NULL
  }
  get <- function() x                                  # 2. get the value of the matrix
  setinverse <- function(solve) inverse <<- solve      # 3. set the value of the inverse
  getinverse <- function() inverse                     # 4. get the value of the inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 'makeCacheMatrix' above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then 'cacheSolve' should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}

