## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(X = matrix()) {
    InvX <- matrix()
    
    set <- function(Y) {
        X <<- Y
        InvX <<- matrix()
    }
    
    get <- function() X
    
    setInverse <- function(Inverse) InvX <<- Inverse
    
    getInverse <- function() InvX
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}





## Write a short comment describing this function
#his function computes the inverse of the special "matrix" returned
#by makeCacheMatrix. If the inverse has already been calculated
#(and the matrix has not changed), then the cachesolve should retrieve 
#the inverse from the cache

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    InvX <- x$getInverse()
    
    if(!is.na(InvX)) {
        message("getting cached inverse matrix")
        return(InvX)
    }
    

    data <- x$get()
    InvX <- solve(data, ...)
    
    x$setInverse(InvX)
    
    InvX
}
