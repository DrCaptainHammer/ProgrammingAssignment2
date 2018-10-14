## These two functions work together to calculate the inverse
## of a given matrix and store the calculation in the cache.
## The second cacheSolve function will check to see if the inverse
## of a function has already been calculated, and if so,
## retrieves the inverse from the cache instead of recalculating.

## This function creates an "object" that will store a matrix
## as well as cache the inverse of that matrix.

makeCacheMatrix <- function(x = matrix()) {
    inverse = NULL
    set = function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This function calculates the inverse of the matrix
## while also checking to see if that particular inverse
## was caluclated previously.

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){
            return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
