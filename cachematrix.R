## Put comments here that give an overall description of what your
### The following function creates a special "matrix" object (a list) containing the following functions:

# set the value of the matrix
# get the value of the matrix
# set the value of its inverse
# get the value of its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


### The following function returns the inverse of a matrix if it has not already been computed.
### On the contrary it returned the cached result without computing it again.  

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
  }