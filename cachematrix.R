## These functions are designed to create a matrix that can cache its inverse
## and return it if the inverse has already been computed.

## creates a special matrix and its methods

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y)
        x <<- y
        i <- NULL
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get, setinverse = setinverse,
         getinverse = getinverse)

}


## computes the inverse of the special matrix or returns the inverse
## if it is already computed

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)){
        message("getting cashed data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}
