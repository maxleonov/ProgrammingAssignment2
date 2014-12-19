# Functions defined here:
# 1. makeCacheMatrix: creates a special "matrix" object that can cache its
#    inverse.
# 2. cacheSolve: computes the inverse of the special "matrix" returned by
#    makeCacheMatrix above. If the inverse has already been calculated
#    (and the matrix has not changed), then the cachesolve should retrieve the
#    inverse from the cache.


# Creates a special "matrix" object that can cache its inverse.
# Returns the list of 4 functions:
# 1. set: set the value of the matrix
# 2. get: get the value of the matrix
# 3. setinv: set the value of the inverse
# 4. getinv: get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(solved) inv <<- solved
        getinv <- function() inv
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)
}


# Computes the inverse of the special "matrix" returned by makeCacheMatrix
# Returns a (cached, if possible) matrix that is the inverse of matrix 'x'
cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}