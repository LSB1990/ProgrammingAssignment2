## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {  ##set matrix
    x <<- y
    inv <<- NULL
  }
  get <- function() x  ##get matrix
  setinverse <- function(inverse) inv <<- inverse  ##set inverse
  getinverse <- function() inv   ##get inverse
  list(set = set, get = get,     ##create the list for return result
       setinverse = setinverse,
       getinverse = getinverse)
}




cacheSolve  <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {   ##check if we don't already have the inverse in the cache
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)   ##compute the inverse otherwise
  x$setinverse(inv)
  inv
}


