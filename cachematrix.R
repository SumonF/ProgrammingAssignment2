## makeCacheMatrix takes a n x n matrix as input and sets its inverse to NULL. 
## Two helper functions (getMatrix, getInverse) are defined.
## Since singular matrices (ie, not having an inverse) are rare, 
## we ignore this possibilty and do not test for singularity.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  setMatrix <- function(y = matrix()) x <<- y
  
  setInverse <- function(inverse) inv <<- inverse
  
  getMatrix <- function() x
  
  getInverse <- function() inv
  
  list(setMatrix = setMatrix, getMatrix = getMatrix,
       setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve first checks whether the matrix already has an inverse.
## If not, it will calculate the inverse and set it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  else {
    inv <- solve(x$getMatrix())
    x$setInverse(inv)
    inv
  }
}


