## The file contains a pair of functions that together allow the inverse of a given matrix
## to be calculated once and restored from cache if required again.

## The function makeCacheMatrix takes an argument of an invertible matrix and
## returns a list with a set of functions that allow the inverse matrix to be stored
## in cache of the parent environment.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
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

## The cacheSolve function takes an argument of a makeCacheMatrix object, which is 
## comprised of a list of functions. It calls those functions to see if the inverse 
## of a matrix has previously been stored in cache, and either retrieves the cached 
## version or calculates it and stores the result in cache.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse();
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
