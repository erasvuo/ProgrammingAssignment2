
makeCacheMatrix <- function(x = matrix()) {
    ## This function, makeCacheMatrix creates a special "matrix", 
    ## which is really a list of four functions:   
    ## 1. set the value of the matrix
    ## 2. get the value of the matrix
    ## 3. set the value of the inverse
    ## 4. get the value of the inverse
    
    inverse <- NULL ## initialize inverse
    ## 1. set value to matrix
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }

    ## 2. get value of matrix
    get <- function() x

    ## 3. calculate and set value to inverse to cache
    setinverse <- function(solve) inverse <<- solve

    ## 4. get value of cached inverse
    getinverse <- function() inverse
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
    
}


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ## At first, let's check if the inverse has already been calculated
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("use cached data")
        return(inverse)
    }    
    ## inverse has not yet been calculated,
    ## let's use solve-function to compute the inverse of a square matrix
    data <- x$get()
    inverse <- solve(data, ...)
    x$setinverse(inverse)
    inverse
}
