## Provide a matrix implementation with cached inverse.
##
## Matrix methods:
##   set(matrix): sets the matrix values
##   get()      : returns the matrix values
##   setinv(inv): sets the inverse matrix
##   getinv()   : returns the inverse matrix

## Create a matrix with cached inverse and four accessor methods

makeCacheMatrix <- function(x = matrix()) {
    cache <- NULL
    
    set <- function(matrix) { ## set a matrix
      x <<- matrix
      cache <<- NULL
    }
    
    get <- function() x ## return matrix
    
    setinv <- function(inv) cache <<- inv ## set cached inverse
    
    getinv <- function() cache ## return cached inverse
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Return a (cached) matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {

    ## get cached inverse and check if not null
    cache <- x$getinv()
    if (!is.null(cache)) {
      message("getting cached data")
      return(cache)
    }
    
    ## compute and cache the inverse
    inv <- solve(x$get(), ...)
    x$setinv(inv)
    inv
}
