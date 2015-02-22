## This pair functions caches the result of performing the inverse of a square invertable matrix
## to avoid repeated computationally intenseive calls to the solve() method
## makeCacheMatrix(m) creates the caching structure for the matrix m
## cacheSolve(s) returns the inverse of matrix m that was used to creae the caching structure s  

## Returns a list which is the caching structure for calculating inverse of a matrix x
## x - a square invertable matrix
## returns a list containing get and set methods for the created caching structure

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inverse <<- inv
  getinv <- function() inverse
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Returns the inverse of a matix either from precomputed cache, or if the precomputed value
## does not exist, computes and caches the inverse before returning it
## x - a caching structure as returned by the function makeCacheMatrix(m)
## returns the inverse of the matrix m

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}