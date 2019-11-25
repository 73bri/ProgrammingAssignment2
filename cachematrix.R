## Put comments here that give an overall description of what your
## functions do
## with this function we can look up in the cache instead of recomputing again
## solve(x) will calculate the inverse

## Write a short comment describing this function
## The function, makeCacheMatrix creates a special matrix, which will
## set the values in the matrix
## get the value in the matrix
## set the inverse of the matrix
## get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function() inv <<- solve(x)
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
## gets the output of thie inverse matrix
## from the cache this function computes the inverse matrix returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed)
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("getting inversed matrix")
        return(inv)
    }
    mat <- x$get()
    inv <- solve(mat, ...)
    x$setInverse(inv)
    inv
}

