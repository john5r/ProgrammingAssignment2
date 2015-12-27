## this function creates an object for caching inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    ## <<- operator which can be used to assign a value to an object 
    ## in an environment that is different from the current environment
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  ## which is really a list containing a function to
  ## set the matrix
  ## get the matrix content
  ## set the value of inverse matrix
  ## get the value of inverse matrix
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## this function computes inverse from the previous function. 
## first it checks if it already is there then it computes nothing
## and just returns the cache


cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    ## if there's no message then you can't tell whether it's
    ## taken from cache or calculated again so
    message("getting cached data")
  return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  ## Computing the inverse of a square matrix 
  ## can be done with the solve function in R
  x$setInverse(inv)
  ## sets the value of the inverse in the cache 
  ## via the setInverse function
  inv
}

