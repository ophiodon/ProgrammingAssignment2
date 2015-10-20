##Create two functions.
## Function 1:  Create a special "matrix" object that can cache its inverse
## Function 2:  Compute the inverse of the special "matrix" returned by Function 1. 
            ## NOTE: If the inverse has already been calculated(and the matrix hasn't changed) then Function 2
            ## should retrieve the inverse from the cache.
            
## Function 1
## a function to
##   - set the value of the matrix
##   - get the value of the matrix
##   - set the value of the inverse matrix
##   - get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
    n <- NULL
    set <- function(y) {
        x <<- y
        n <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) n <<- inv
    getinverse <- function() n
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## Function 2
## matrix will be inverse of CacheMatrix (Function 1)

cacheSolve <- function(x, ...) {
    n <- x$getinverse()
    if(!is.null(n)) {
        message("Getting Cached Data")
        return(n)
    }
    Data <- x$get()
    n <- solve(Data, ...)
    x$setinverse(n)
    n
}

## Tested CacheMatrix code with test<-makeCacheMatrix(matrix(c(50,25,25,50),c(2,2)))
## Tested cacheSolve with test<-makeCacheMatrix(matrix(c(50,25,25,50),c(2,2)))
## results
            ##       [,1]        [,2]
            ##[1,]  0.02666667 -0.01333333
            ##[2,] -0.01333333  0.02666667
