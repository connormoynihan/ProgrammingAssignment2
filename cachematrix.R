## 1) makeCacheMatrix: This function creates the matrix object that can cache the inverse.
## 2) cacheSolve: This function computes the inverse of the matrix returned by makeCacheMatrix
##    above. If the inverse has already been calculated, then the cachesolve
##    should retrieve the inverse from the cache.


## The makeCacheMatrix function creates the matrix to store the inverse of the object

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(solve) m <<- solve
      getinv <- function() m
      list(set = set, get = get,
           setinv = setinv,
           getinv = getinv)
}


## This function calculates the inverse of the object

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'

      m <- x$getinv()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      m
}
