## Assignment 2
## Function definitions: makeCacheMatrix and cacheSolve
## These functions utilize R's lexical scoping rules to cache the output of a time-consuming computation
##   for future use (in this case, a matrix inverse).

## makeCacheMatrix returns a list of functions for getting/setting the inverse computation from matrix 'x'
makeCacheMatrix <- function(x = matrix()) {
  
  minv <- NULL
  
  ## fct #1: set
  set <- function(y) {
    x <<- y
    minv <<- NULL
  }
  
  ## fct #2: get
  get <- function() x
  
  ## fct #3: setinverse
  setInverse <- function(inverse) minv <<- inverse
  
  ## fct #4: getinverse
  getInverse <- function() minv
  
  ## return list
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve checks if the matrix inverse has already been calculated and cached;
##     if yes, it retrieves it, if no, it calculates the inverse and caches it. 
cacheSolve <- function(x, ...) {
  
  ## retrieve the inverse of the matrix
  minv <- x$getInverse()
  
  ## if 'minv' has a value, return it and exit function
  if(!is.null(minv)) {
    message("getting cached data")
    return(minv)
  }
  
  ## otherwise, get data, calc inv, and reset inv
  data <- x$get()
  
  ## Return a matrix that is the inverse of 'x'
  minv <- solve(data, ...)
  x$setInverse(minv)
  minv
}

