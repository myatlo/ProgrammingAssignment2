## Put comments here that give an overall description of what your
## functions do

## Function creates special object, which is a list containing a function to set and get values of matrix, 
## set and get inverse value of matrix.

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
            set <- function(y) {
                    x <<- y
                    inv <<- NULL
            }
            get <- function() x
            setinverse <- function(inverse) inv <<- inverse
            getinverse <- function() inv
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)
}


## The function calculates inverse of a matrix, which is assumed to be inversable. 
## Firstly it checks to see if inverse has already been calculated. 
## If so, it `get`s the mean from the cache and skips the computation. 

cacheSolve <- function(x, ...) {
            inv <- x$getinverse()
            if(!is.null(inv)) {
                    message("getting cached data")
                    return(inv)
            }
            data <- x$get()
            inv <- solve(data, ...)
            x$setinverse(inv)
            inv
}
