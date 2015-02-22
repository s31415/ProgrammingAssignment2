## Functions that create a data structure which can hold a matrix and
## its inverse. The inverse is cached and thus only computed the first
## time it is needed

## A function that takes as input a matrix an returns a list with
## methods to get and set the input matrix and get and set the inverse
## of the input matrix.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  set.inverse <- function(inverse.in) inverse <<- inverse.in
  get.inverse <- function() inverse
  list(set = set, get = get,
       set.inverse = set.inverse,
       get.inverse = get.inverse)
}


## A function which takes a list such as the one returned from
## makeCacheMatrix and computes the inverse and caches it
## if it has not already been cached but if it has already been cached
## then it returns the cached value.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$get.inverse()
  if(!is.null(inverse)) {
    message("Returning inverse from cache.")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data, ...)
  x$set.inverse(inverse)
  inverse
}
