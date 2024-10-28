## These functions set/calculate and cache the inverse of a matrix
## for easy future retrieval without recalculating

## This function, makeCacheMatrix creates a matrix, which is really a list
## containing functions to: set and get the matrix, and set and get the 
## inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL
        set <- function(y){
                x <<- y
                s <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) s <<- inverse
        getinv <- function() s
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function calculates the inverse of the matrix created
## with the above function. It first checks to see if the 
## inverse has already been calculated. If so, it gets the
## value from cache and skips the computation. Otherwise, it
## calculates the inverse of the matrix and sets the value of 
## inverse inside the setinv function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinv()
        if(!is.null(s)){
                message("getting cached data")
                return(s)
        }
        data <- x$get()
        s <- solve(data, ...)
        x$setinv(s)
        s
}
