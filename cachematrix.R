##  These functions create a matrix inverse and cache the value.
##  That value m can be accessed from outside the environment in which it was created.







## This function calculates and caches a matrix inverse, m.
## It also creates a list of functions.

makeCacheMatrix <- function(x = matrix()) {


 m <- NULL
            set <- function(y) {
                    x <<- y
                    m <<- NULL
            }
            get <- function() x
            setMatrixInverse <- function(solve) m <<- solve
            getMatrixInverse <- function() m
            list(set = set, get = get,
                 setMatrixInverse = setMatrixInverse,
                 getMatrixInverse = getMatrixInverse)

}





## This function creates a cache, m, of a matrix inverse.
## It first calculates the inverse if not already done, otherwise it gets the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getMatrixInverse()
            if(!is.null(m)) {
                    message("getting cached data")
                    return(m)
            }

            data <- x$get()
            m <- solve(data, ...)
            x$setMatrixInverse(m)
            m
}
