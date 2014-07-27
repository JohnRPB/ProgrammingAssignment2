## Functions designed to calculate the inverse of a matrix, or to retrieve the cached value of the inverse, 
## if already calculated.

## makeCacheMatrix creates a list of functions that are used by cacheSolve. It takes a matrix as an input, makes
## it available for cacheSolve to calcuate the inverse, and retrieves the new inverse value from cacheSolve, storing
## it for later use.

makeCacheMatrix <- function(o = matrix()) {
        s <- NULL
        set <- function(y) {
                o <<- y
                s <<- NULL
        }
        get <- function() o
        setinv <- function(inv) s <<- inv
        getinv <- function() s
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## cacheSolve checks to see if object "s" (the inverse), from the makeCacheMatrix function, is NULL (meaning it hasn't been
## calculated before). If not, it retrieves "s"; if so, it calculates a new "s" and sends it to the makeCacheMatrix
## function where it is stored via the '<<-' operator for later use. 

cacheSolve <- function(o, ...) {
        s <- o$getinv()
        if(!is.null(s)) {
                message("getting cached data")
                return(s)
        }
        dat <- o$get()
        s <- solve(dat, ...)
        o$setinv(s)
        s
}