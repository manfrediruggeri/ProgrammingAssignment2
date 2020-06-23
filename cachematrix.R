## This script aims to calculate Inverse Matrix. Calculation could take time so 
## it's helpful caching the result and showing it if requested more and more. 

## makeCacheMatrix return us an object, actually a list of functions. By means of
## these functions we can get and set Matrix and Inverse Matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y  ## using <<- because x is defined in parent environment
        inv <<- NULL 
    }
    get <- function() x
    setInv <- function(solve) inv <<- solve
    getInv <- function() inv
    list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## cacheSolve return us the inverse matrix. If the inverse has been stored, this
## function gets the result and return it; otherwise cacheSolve calculates the
## inverse and stores it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInv()
        if(!is.null(inv)) {
            message("getting cached inverse matrix")
            return(inv)
        }
        message("calculating inverse matrix")
        data <- x$get()
        inv <- solve(data, ...)
        x$setInv(inv)
        inv
}
