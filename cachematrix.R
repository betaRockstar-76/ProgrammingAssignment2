## These functions will cache the inverse of a matrix

## This function creates a special "matrix" object that can cache it's inverse

makeCacheMatrix <- function(x = matrix()) {
  inver <- NULL
  set <- function(y) {
    x <<- y
    inver <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inver <<- solve
  getinverse <- function() inver
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse has already been computed, it will be retrieved from the cache

cacheSolve <- function(x, ...) {
  inver <- x$getinverse()
  if(!is.null(inver)){
    message("Getting cached data.")
    return(inver)
  }
  data <- x$get()
  inver <- solve(data, ...)
  x$setinverse(inver)
  inver
}