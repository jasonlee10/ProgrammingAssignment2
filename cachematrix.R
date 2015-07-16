## Put comments here that give an overall description of what your
## functions do

## Creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inver) inv <<- inver
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Find the inverse of a matrix. If previously calculated, use cache,
## otherwise calculate.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv))
        {
                message("getting cached data")
                return(inv)
        }
        mtrx <- x$get()
        inv <- solve(mtrx, ...)
        x$setinv(inv)
        inv
}
