## Author: Adriana Céspedes Vindas
## Date: 10/12/2017
## cache of inverse matrix

## This function caches the inverse of a matrix.
## First it sets the value of the matrix
## Then, gets the value of the matrix
## Sets the value of the inverse matrix and gets the value of the inverse
## This code is based on the vector sample of Programming Assigment 2

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function returns the inverse of a matrix.
## The matrix is a parameter of the function. If the inverse is in cache, the function returns the cached data.
## If the data is not in cache the inverse is calculated with solve function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
