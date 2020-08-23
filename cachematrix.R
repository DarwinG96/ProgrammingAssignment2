## Hello
## I created 2 function;

## 1. makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## This function builds a set of functions and returns the functions within a list to the parent environment 
##  It also includes the two data objects: 'data' and 'inv'.

makeCacheMatrix <- function(data = matrix()) {  # The input argument  'data' is a "matrix" object
  inv <- NULL                                   # It starts with an empty object named 'inv'
  setd <- function(x) {                         # In this function start two objects: 'data' and 'inv'
    data <<- x                                  # these objects are cached
    inv <<- NULL
  }
  getd <- function() data                       # This function defines the getter for the vector 'data'.
  setinv<- function(inv2) inv <<- inv2          # This function defines the setter for the INVERSE of matrix in 'inv'
  getinv <- function() inv                      # This function defines the getter for the INVERSE 'inv'
  list(setd = setd, getd = getd,                # Create a new object by returning a list(), gives the name 'setd' to the setd() and 'getd' to the getd() function 
       setinv = setinv,                         # gives the name 'setinv' to the setinv() function defined above
       getinv = getinv)                         # gives the name 'getinv' to the geinv() function defined above
}
 

## 2. 2. cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated, then the cachesolve retrieve the inverse from the cache.

cacheSolve <- function(data, ...) {                     # Starts with a single argument 'data', and an ellipsis
  inv <- data$getinv()                                  # Here retrieve the Inverse 'inv' from the object passed
  if(!is.null(inv)) {                                   # Then it checks to see whether the result is NULL
    message("Retrieving the inverse from the cache!")   # If this is TRUE, show message "Retrieving the inverse from the cache!"
    return(inv)                                         # And it gets the INVERSE from the cache and return
  }                                                     # if the condition is FALSE,
  forsolve <- data$getd()                               # gets the 'data' from the input object
  inv <- solve(forsolve, ...)                           # and calculates the INVERSE 
  data$setinv(inv)                                      # then, uses the setinv() function defined above to set the INVERSE in the object 'inv'
  inv                                                   # Print a matrix that is the INVERSE of 'data'
}
