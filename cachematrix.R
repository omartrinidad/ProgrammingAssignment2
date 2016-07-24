
## Programming task 2
## Two functions are created to manage the caching of variables, in this case,
## we are caching the inverse of a matrix

## This function creates an object that stores a matrix and cache's its inverse
## This function returns a list containing four functions:
##
## set, set the matrix, an empty matrix is the default value
## get, get the matrix set
## setinverse, calculate the inverse of the matrix
## getinverse, get the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Return a matrix that is the inverse of `x`, `x` is the list returned by
## `makeCacheMatrix`, ## if the inverse of the matrix has not been calculated,
## then it is done and the result is stored in the cache
## otherwise, we get the inverse stored in the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}


## usage:
matrix.example <- matrix(c(2, 5, 1, 3), 2, 2)
caching.object <- makeCacheMatrix()

# first run, calculating the inverse
cacheSolve(caching.object)
# second run, skipping calculation, getting the result from the cache
cacheSolve(caching.object)
