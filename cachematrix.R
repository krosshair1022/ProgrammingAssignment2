

## Functions which create a matrix object and cache its inverse

## Function which creates the matrix object and calculates its inverse
makeCacheMatrix <- function(x = matrix()) 
{
    if(is.null(x) || is.na(x) || nrow(x)!=ncol(x))
    {
        print("Inverse can only be calcualted on a Square Matrix")
        return()
    }
    inv <- NULL
    set <- function(y) 
    {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(solve) inv <<- solve
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Function which calcuates the inverse of x
## First checks to see if the inverse has already been calculated. 
## If previously cached returns it else calculates the inverse
cacheSolve <- function(x, ...) 
{
    inv <- x$getInverse()
    if(!is.null(inv)) {
        message("Getting cached inverse")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setInverse(inv)
    inv
}
