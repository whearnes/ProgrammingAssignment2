## Create a special type of matrix that allows caching of the inverse.
## Assumes that matrix is invertible -- no error-trapping.

## Example -- note the "Getting cached data" after the first call. When
##    data is changed, it calculates the first time and then retrieves
##    the cached value.
## > t <- matrix(1:4,2,2)
## > solve(t)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > tSpecial <- makeCacheMatrix(t)
## > cacheSolve(tSpecial)
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(tSpecial)
## Getting cached data
## [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > tSpecial <- makeCacheMatrix(matrix(2:5,2,2))
## > cacheSolve(tSpecial)
## [,1] [,2]
## [1,] -2.5    2
## [2,]  1.5   -1
## > cacheSolve(tSpecial)
## Getting cached data
## [,1] [,2]
## [1,] -2.5    2
## [2,]  1.5   -1

## This function creates a matrix that is a list containing a function to:
##    Set the value of the matrix
##    Get the value of the matrix
##    Set the value of the inverse
##    Get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
  ## Set inverse to NULL
  i <- NULL
  
  ## Create the functions
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv 
  getinv <- function() i
  list(set = set, get = get, setinv = setinv, getinv = getinv)

}

## This function calculates the inverse of the matrix created by makeCacheMatrix.
## If the inverse has already been calculated, it retrieves that value.
## If the inverse has not already been calculated, it calculates and stores that value
##    for future reference.

cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  
  ## Check for cached value
  i <-x$getinv()
  if (!is.null(i)) {
    message("Getting cached data")
    return(i)
  }
  
  ## Otherwise calculate
  data <- x$get()
  i <- solve(data)
  x$setinv(i)
  i
  
}
