## Put comments here that give an overall description of what your
## functions do

## This function creates a matrix from which the inverse is saved
## the cache in order that it doesn't have to be calculated twice. 
makeCacheMatrix <- function(x = matrix()) {
  inv <- matrix()
  set <- function(y) {
    x <<- y
    inv <<- matrix()
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse) 
}


## This function returns the inverse of a matrix created with the function above
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached matrix")
    return(inverse)
  }
  matrix <- x$get()
  inverse <- solve(x)
  x$setInverse(inverse)
  inverse
}
