## These functions set/calculate and cache the inverse of a matrix
## for easy future retrieval without recalculating

## This function, makeCacheMatrix creates a matrix, which is really a list
## containing functions to: set and get the matrix, and set and get the 
## inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        # initializes inverse variable as null
        s <- NULL
        
        # assigns a matrix
        set <- function(y){
                # set matrix values
                x <<- y
                
                # resets the cached inverse value to null
                # after matrix has been changed or set
                s <<- NULL 
        }
        
        # returns the matrix
        get <- function() x
        
        # assigns a matrix inverse 
        setinv <- function(inverse) s <<- inverse
        
        # returns the inverse matrix
        getinv <- function() s
        
        # returns a list of functions enclosed in the parent function
        list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## This function calculates the inverse of the matrix created
## with the above function. It first checks to see if the 
## inverse has already been calculated. If so, it gets the
## value from cache and skips the computation. Otherwise, it
## calculate the inverse of the matrix and sets the value of 
## inverse inside the setinv function

cacheSolve <- function(x, ...) {
        
        # extracts the value of the cached inverse matrix from 
        # the function above.
        s <- x$getinv()
        
        # checks if the inverse matrix cached variable is not
        # null, and returns a matrix that is the inverse of 'x'
        if(!is.null(s)){
                message("getting cached data")
                return(s)
        }
        
        # if variable is null, it extracts the matrix data
        data <- x$get()
        
        # calculates the matrix inverse
        s <- solve(data, ...)
        
        # sets the matrix inverse in the above function
        x$setinv(s)
        
        # Returns a matrix that is the inverse of 'x'
        s
}

## Example
# creating a makeCacheMatrix object
mat <- makeCacheMatrix(matrix(1:4,2,2))

# checking the values of the matrix
mat$get()

# setting the inverse of the matrix using the first function
mat$setinv(inv(mat$get()))

# checking the values of the cached inverse matrix using the
# second function
p <- cacheSolve(mat)
p
