## Caching functions for Coursera R Programming course Week 3 assignment

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                                  # initialize the cached matrix inverse with NULL
        set <- function(y) {                         # setter function - sets the new matrix, resets the inv to NULL
                x   <<- y
                inv <<- NULL
        }
        get <- function() x                          # getter function - gets the matrix
        setinv <- function(inverse) inv <<- inverse  # function setting the cached inverse of the matrix
        getinv <- function() inv                     # function getting the cached inverse of the matrix
        list(set = set, get = get,                   # list of created functions being returned
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
}
