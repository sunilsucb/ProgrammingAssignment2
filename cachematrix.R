## Returns a list of functions to set, get value functions to variables that 
## can be accessed outside of this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <- NULL
  }
  get <- function() x
  setinverse <- function(inversedmatrix) m <<- inversedmatrix
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Get the value of inverse from matrix if its not null and the matrix hasn't changed. 
## If no value exists, calculate it for the first time and set it back
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("Getting cached data.")
    return(m)
  } else {
    message("Calculating the inverse of matrix")
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    return(m)
  }
}