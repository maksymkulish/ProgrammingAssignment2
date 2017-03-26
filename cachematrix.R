## Put comments here that give an overall description of what your
## functions do

## Short comment describing makeCacheMatrix function
# a. returns a list of functions and contains the following functions:
# a1. setMatrix - set the value of a matrix
# a2. getMatrix - get the value of a matrix
# a3. setInverse - set the cahced value (inverse of the matrix)
# a4. getInverse - get the cahced value (inverse of the matrix)
makeCacheMatrix <- function(x = matrix()) {
  inv1 <- NULL
  setMatrix <- function(y) {
      x <<- y
      inv1 <<- NULL
  }
  getMatrix <- function() x
  setInverse <- function(inverse) inv1 <<- inverse
  getInverse <- function() inv1
  list(setMatrix=setMatrix, getMatrix=getMatrix, setInverse=setInverse, getInverse=getInverse)
}


# The following function calculates the inverse of a matrix created with makeCacheMatrix:
# a. cacheSolve checks if the inverse has already been computed:
# a1. if TRUE then it gets the result and skips the calculation.
# a2. if FALSE then it computes the inverse, sets the value in the cache via setInverse function
# computation
# b. cacheSolve does so that the matrix is always has inverse type.
cacheSolve <- function(x, ...) {
  inv1 <- x$getInverse()
  if(!is.null(inv1)) {
      message("There are getting cached data")
      return(inv1)
  }
  dataset <- x$getMatrix()
  inv1 <- solve(dataset)
  x$setInverse(inv1) ## Return a matrix that is the inverse of 'x'
  inv1
}
## Testing makeCacheMatrix:
x = rbind(c(3, -4), c(-4, 3))
rez = makeCacheMatrix(x)
rez$getMatrix()

## Testing cacheSolve - No cache in the 1st execution of cacheSolve
cacheSolve(rez)

## Testing cacheSolve - Retrieving from the cache in the 2nd execution of cacheSolve
cacheSolve(rez)