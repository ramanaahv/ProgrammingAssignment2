## This File contains 2 functions :
## makeCacheMatrix which is used to set a matrix to a variable 
## and set its inverse to another variable.
## cacheSolve which is used to get the inverse of a matrix x
## cacheSolve or compute the inverse and caches it



## makeCacheMatrix has 4 functions : 
## 1. set function is a basic initialse function which 
##    initialises the matrix to variable x and initialise the 
##    inverse of matrix to variable m
## 2. get function which returns the matrix
## 3. setinv function which sets the inverse of matrrix 
##    to variable m
## 4. getinv function which returns the variable m 
##    that is usede to store the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve function first tries to retrieve the 
## inverse of the matrix if it is cached and print the inverse is 
## obtained from the cache else computes
## the inverse of the matrix and caches it.
cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}