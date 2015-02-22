## These two functions are used for finding the inverse of a square matrix
## (presumably of a high dimension). It is assumed that the provided matrix is
## square and invertible. It is meant as an exercise for showing that every time 
## the inverse of a matrix is needed we do not have to recalculate it, but retrieve it
## from the cache. This is a general idea to be applied to other kinds of computations 

## This function creates a list of functions which when called:
## can initialize (set, or hold) a square matrix
## get the current matrix 
## find the inverse of holded matrix
## grap the inverse  matrix when is previously calculated 

makeCacheMatrix <- function(x = matrix()) {
  # I assume invertible matrix is provided
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,setinverse = setinverse,getinverse = getinverse)
}


## This function basically checks if a new matrix is in place and only then finds its inverse.
## This is done by looking at the placeholder m 
## When it is NULL only then it calculates the inverse
## Otherwise it returns the inverse matrix already residing in m

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("Getting cached inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data) # Although stated in the directions, there is no need to 
  # have "solve" in the form: solve(a,b). So no need to create the diagonal matrix to solve for the inverse, with something like this: solve(data,diag(dim(data)[1]))
  x$setinverse(m)
  m
}