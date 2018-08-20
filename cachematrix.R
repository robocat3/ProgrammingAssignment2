## Inverting a matrix can be a costly computation. These functions
## cache the inverse of a matrix in order to make it a less taxing
## operation.

## The following function creates a special "matrix" that can cache
## the inverse of itself.

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL

    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    get <- function() x

    set_inv <- function(inverse) inv <<- inverse

    get_inv <- function() inv

    list(set = set, get = get, set_inv = set_inv, 
get_inv = get_inv)

}


## The following function computes the inverse of the "matrix" generated
## by the above function.

cacheSolve <- function(x, ...) {

    inv <- x$get_inv()

    if(!is.null(inv)) {
        message("Getting the cached data.")
        return(inv)
    }

    data <- x$get()

    inv <- solve(data)

    x$set_inv(inv)

    inv

}
