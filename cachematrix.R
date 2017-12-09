## A pair of functions that cache the inverse of a given matrix

## A function to create a special object that stores a matrix, with 4 internal set & get functions

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set = function(y){
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setInverse <- function(inversedMatrix) im <<- inversedMatrix
  getInverse <- function() im
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function calculates the inverse of the matrix stored in the above function

cacheSolve <- function(x, ...) {
  im <- x$getInverse()
  if(!is.null(im)){
    message('getting cached data')
    return(im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setInverse(im)
  im
        ## Return a matrix that is the inverse of 'x'
}
