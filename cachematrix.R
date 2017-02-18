## In this assigment, I am trying to cache an inverse of a matrix.
## Assuming that given matrix is invertable.

## This function returns a vector which contains some functions explained below
## setMatrix - changes value of a matrix with given new value 
## getMatrix - returns value of a matrix
## setInverse - changes value of an inverse of a matrix 
## getInverse - returns inverse value of a matrix
makeCacheMatrix <- function(x = matrix()) {
  ## declaration for inverse matrix
  inverseOfMatrix <- NULL
  
  setMatrix <- function(newValue) {
    x <<- newValue
    inverseOfMatrix <<- NULL
  }
  
  getMatrix <- function()
    x
  
  setInverse <- function(newValue)
    inverseOfMatrix <<- newValue
  
  getInverse <- function()
    inverseOfMatrix
  
  ## return all functions defined above
  list(
    setMatrix = setMatrix,
    getMatrix = getMatrix,
    setInverse = setInverse,
    getInverse = getInverse
  )
}


## solve(x) - gives the inverse of a square matrix
## check for more information    ?solve

## This function caches inverse of matrix and returns it
cacheSolve <- function(x, ...) {
  # let us keep inverse of a matrix
  inverseOfMatrix <- x$getInverse()
  
  ## matrix should be checked if it is empty or not
  if (!is.null(inverseOfMatrix)) {
    message('get chached inversed matrix')
    
  } else {
    copy <- x$getMatrix()
    inverseOfMatrix <- solve(copy, ...)
    x$setInverse(inverseOfMatrix)
  }
  inverseOfMatrix
}
