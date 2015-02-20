## This 'cachematrix' function is created for Programming Assignment 2
## for the R Programming course. It comprises of two paired functions 
## that determine the inverse of a matrix (especially large ones) by 
## caching the inverse of the original 

## Creates special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y) {
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setcache <- function(solve) s <<- solve
        getcache <- function() s
        list(set = set, get = get,
             setcache = setcache,
             getcache = getcache)
}


## Computes inverse of the matrix returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getcache()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setcache(s)
        s
}
