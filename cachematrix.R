## Put comments here that give an overall description of what your
## functions do

## The purpose of this function is to set up the cashe vector

makeCacheMatrix <- function(x = matrix()) {
     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setinv <- function(solve) m <<- solve
     getinv <- function() m
     list(set = set, get = get,
          setinv = setinv,
          getinv = getinv)
}


## The purpose of this function is to compute the inverse of a matrix ,if it has
## not already been computerd, or return the inverse from cashe if it has been
## previously computed.

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     m <- x$getinv()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data)
     x$setinv(m)
     m
}
