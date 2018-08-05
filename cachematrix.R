## Put comments here that give an overall description of what your
## functions do

##R Programming: Course 2 Week 3 Programming Assignment
##Functions below create a matrix object that has a cached inverse that can be retrieved.

##makeCacheMatrix creates an matrix object that's inverse can be cached for later use.

makeCacheMatrix <- function(x = matrix()) {
  invs <- NULL
  set <- function(y) {
    x <<- y
    invs <<- NULL
  }
  get <- function() x
  setinvs <- function(inverse) invs <<- inverse
  getinvs <- function() invs
  list(set = set, get = get,
       setinvs = setinvs,
       getinvs = getinvs)
}


## Calculates inverse of the matrix object. If it already has been calculated it will be retrieved...
#  ...from the cache.

cacheSolve <- function(x, ...) {
  invs <- x$getinvs()
  if(!is.null(invs)) {
    message("getting cached data")
    return(invs)
  }
  data <- x$get()
  invs <- solve(data, ...)
  x$setinvs(invs)
  invs
}
