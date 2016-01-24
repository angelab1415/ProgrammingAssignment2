## 
## test matrices:
## ma <- matrix(c(2,5,1,3),nrow=2,ncol=2,byrow=TRUE)
## ma <- matrix(c(1,2,2,3),nrow=2,ncol=2,byrow=TRUE)
## WARNING:use only invertible matrices, i.e. det(A) != 0
## this should be tested, but I don't have time at the moment
##
## usage:
## ta <- makeCacheMatrix(ma)
## cacheSolve(ta)
## cacheSolve(ta)

## function creates matrix and caches inverse
makeCacheMatrix <- function(x = matrix()) {
    B <- NULL
    set <- function(y){
        x <<- y
        B <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) B <<- solve
    getsolve <- function() B
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## function: actual computation of inverse matrix returned by makeCacheMatrix

cacheSolve <- function(x, ...) {
    B <- x$getsolve()
    if(!is.null(B)) {
        message("getting cached data")
        return(B)
    }
    data <- x$get()
    B <- solve(data, ...)
    x$setsolve(B)
    B
}