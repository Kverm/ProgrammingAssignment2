## The functions below together allow for caching the inverse of a matrix

## A function that returns a special matrix object with the ability to cache its inverse when calculated

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
              x <<- y
              inv <<- NULL
  }  
  get <- function() x
  set_inverse <- function(inverse) inv <<- inverse
  get_inverse <- function() inv
  list(set = set, get = get, 
       set_inverse = set_inverse,
       get_inverse = get_inverse)
}


## A function that calculates the inverse of a special matrix object if not already calculated
## and both caches and returns the result

cacheSolve <- function(x, ...) {
        inv <- x$get_inverse()
        if(!is.null(inv)) {
          message("Getting cached data")
          return(inv)
        }
        mx <- x$get()
        inv <- solve(mx, ...)
        x$set_inverse(inv)
        inv
}
