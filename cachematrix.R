## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function:
## "makeCacheMatrix" initialize a matrix object and allow to cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL                             ## initialize inv as NULL as placeholder for the matrix inverse 
    set <- function(y) {                    ## introduce the "set" function for the matrix 
        x <<- y                         
        inv <<- NULL                    
    }
    get <- function() x                     ## introduce the get fucntion
    
    setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
    getinverse <- function() inv                     ## gets the value of inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## lists referring to the introduced functions

}


## Write a short comment describing this function:
## 1) calculate the inverse of the matrix from the previous function
## 2) if the matrix has alredy been calculated, it retrieve the cached value for the inverse

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {                     ## if inv != NULL the cached value is retrieved
        message("getting cached data")
        return(inv)
    }
    data <- x$get()                         ## otherwise the inverse is calculated with "solve"
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
