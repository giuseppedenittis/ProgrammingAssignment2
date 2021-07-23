## Hereafter you can find two functions to efficiently compute the inverse of a matrix
## The speed-up is obtained by caching the matrix if it has already been inverted

## Creates a special matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {

    ## Initialization
    i <- NULL

    ## Method to set the matrix
    set <- function( matrix ) {
            m <<- matrix
            i <<- NULL
    }

    ## Method the get the matrix
    get <- function() {
    	m
    }

    ## Method to set the inverse of the matrix
    setInverse <- function(inverse) {
        i <<- inverse
    }

    ## Method to get the inverse of the matrix
    getInverse <- function() {
        i
    }

    ## Return a list of the methods
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Compute the inverse of a matrix manipulated by makeCacheMatrix
## If the inverse has already been calculated, cacheSolve retrieves the inverse
## from the cache.
cacheSolve <- function(x, ...) {

    ## Try to get the inverse of x
    m <- x$getInverse()

    ## Return the inverse of x if it already exists
    if( !is.null(m) ) {
            message("getting cached data")
            return(m)
    }

    ## If the inverse has not been computed yet, invert x
    ## Get the matrix
    data <- x$get()

    ## Calculate the inverse using matrix multiplication
    m <- solve(data) %*% data

    ## Set the inverse
    x$setInverse(m)

    ## Return a matrix that is the inverse of x
    m
}
