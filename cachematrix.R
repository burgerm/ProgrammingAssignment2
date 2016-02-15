## Usage of the functions:
## > a <- makeCacheMatrix()
## > a$set(matrix(1:4,2,2))
## > cacheSolve(a)

## create a 'special' matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    ## set the value of the matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    ## get the value of the matrix
    get <- function() x
    ## set the value of the inverse
    setinverse <- function(solve) inv <<- solve
    ## get the value of the inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Get the inverse of the 'special' matrix object created with 'makeCacheMatrix'
cacheSolve <- function(x = matrix(), ...) {
    ## check to see if the inverse has already been calculated
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting inverse from cache")
    }
    else
    {
        ## calculate the inverse
        data <- x$get()
        inv <- solve(data, ...)
        ## sets the value of the inverse in the cache
        x$setinverse(inv)
    }
    inv
}