
## This function creates an object that stores a matrix and cache's its inverse
## This function returns a list containing four functions
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
## `makeCacheMatrix`
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

matrix.example <- matrix(c(2, 5, 1, 3), 2, 2)
caching.object <- makeCacheMatrix()

# first run, calculating the inverse
cacheSolve(caching.object)
# second run, skipping calculation, getting the result from the cache
cacheSolve(caching.object)
