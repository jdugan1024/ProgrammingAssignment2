##
## Implement functions to cache the inverse of a matrix.
##
## Matrix inversion is an expensive operation and if it inverse is needed
## frequently caching provides a peformance boost.
##
## Example:
##
## m <- makeCacheMatrix(matrix(c(4,2,7,6), ncol=2))
## m$get()
## inv <- cacheSolve(m)
## m$getinverse()
##

##
## This function creates a special version of a matrix which provides caching.
##
## It provides several methods for working with the cached data:
##
##   get: gets the value of the matrix
##   setinverse: updates the cache with a new inverse
##   getinverse: retrieves the inverse from the cache
##
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(i) inverse <<- i
  getinverse <- function() inverse
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}


## Caclculate the inverse of a matrix using a cached value if available

cacheSolve <- function(x, ...) {
  i <- x$getinverse()
  if(!is.null(i)) {
    return(i)
  }

  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
