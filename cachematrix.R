## cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

  invrs <- NULL
  set <- function(y) {
    x <<- y
    invrs  <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invrs <<- inverse
  getinverse <- function() invrs
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

 ## This function computes the inverse of the special "matrix" created by
 ## makeCacheMatrix above. If the inverse has already been calculated (and the
 ## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
     invrs <- x$getinverse()
  if(!is.null(invrs)) {
    message("getting cached data")
    return(invrs)
  }
  mtrx <- x$get()
  invrs <- solve(mtrx, ...)
  x$setinverse(invrs)
  invrs
  
}
