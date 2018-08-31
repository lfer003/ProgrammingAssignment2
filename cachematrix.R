#" Matrix inversion is usually a costly computation and
## here may be some benefit to caching the inverse 
## of a matrix rather than computing it repeatedly"

## The function takes a matrix and calculates the inverse of it. SO that the answer can
## be cached to  be called when required instead of repeatedly being calculated
##

## Write a short comment describing this function
## THe Function makeCacheMatrix completes the following

## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse
## 4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  ## This sets Variable cm to Null
  cm <- NULL
  ## set the value of the matrix
  set_cm <- function(y) {
    x <<- y
    cm <<- NULL
  }
  ## get the value of the matrix
  get_cm <- function() x
  ## set the value of the inverse
  setinverse_cm <- function(inverse) cm <<- inverse
  ## get the value of the inverse
  getinverse_cm <- function() cm
  ## Creates the list that is the input for the the function cacheSolve
  list(set_cm = set_cm,
       get_cm = get_cm,
       setinverse_cm = setinverse_cm,
       getinverse_cm = getinverse_cm)
}


## This function returns the matrix that is the inverse of the input x
## using the output of the makeCacheMatrix function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  cm <- x$getinverse_cm()
  if (!is.null(cm)) {
    message("getting cached data")
    return(cm)
  }
  data <- x$get_cm()
  cm <- solve(data, ...)
  x$setinverse_cm(cm)
  cm
}
