## Andre Coleman, Jr.
## Data Science: R - Coursera | 01/04/2021
## The series of functions computes the inverse of a matrix either by
## computing it in situ or recalling a cached value

## The makeCacheMatrix creates a cache of the inverse matrix result as well as
## a set of functions to manipulate that inverse
## Input: Matrix, Output: Function List

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        setinversemx <- function(solve) i <<- solve
        getinversemx <- function() i
        return(list(set = set, get = get, setinversemx = setinversemx, 
             getinversemx = getinversemx))
}


## The cacheSolve provides the inverse of a matrix (either by cache or 
## computation)
## Input: Function List, Output: Inverse Matrix

cacheSolve <- function(x, ...) {
        i <- x$getinversemx()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data,...)
        x$setinversemx(i)
        i
}
