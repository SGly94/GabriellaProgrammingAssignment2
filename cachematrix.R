## Put comments here that give an overall description of what your
## functions do


makeCacheMatrix <- function(x = matrix()) {
## MakeCacheMatrix creates a special matrix object that can cache its inverse
        inv <- NULL 
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setInv <- function(Inv) inv <<- Inv########
        getInv <- function() inv
        list(set = set, get = get, setInv = setInv, getInv = getInv)
}




cacheSolve <- function(x, ...) {
## Compute the -1 of the special matrix returned by MakeCacheMatrix
## Return a matrix that is the inverse of 'x'
        inv <- x$getInv
        if (!is.null(inv)){
                message("getting cached data")
                return (inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setInv(inv)
        inv
}