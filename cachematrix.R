## makeCacheMatrix provides a list of functions capable of 
## storing a matrix, passing that matrix value to the 
## cacheSolve function for futher computations and 
## returns the computed value

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL  #initialize m object
  
  ## set's matrix variable m to NULL when new matrix called
  ## and assigns new matrix to x.
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  ## retuns matrix value stored in x
  get <- function() x
  
  ## stores the inverse matrix value from cacheSolve
  setinverse <- function(invm) m <<- invm
  
  ## returns either cached or newly calculated inverse matrix
  getinverse <- function() m
  
  ## list of functions in function makeCacheMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## uses functions in makeCacheMatrix to to examine whether there
## is an existing inverse matrix calculation value, calculates
## a new one if there is not and returns the new inverse calculation
## or simply passes back the existing inverse matrix value

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## calls the getinverse function from makeCacheMatrix and
  ## assigns value to m
  m <- x$getinverse()
  
  ## if getinverse does not return a NULL value
  ## then function returns m value from getinverse
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## otherwise, data object is assigned new matrix value
  data <- x$get()
  
  ## m is assigned the inverse matrix calcuation value
  m <- solve(data, ...)
  
  ## setinverse function is called with new inverse matrix result and 
  ## returns m
  x$setinverse(m)
  m
}

