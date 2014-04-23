## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
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

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    InvX <- x$getInverse()
    
    if(!is.na(InvX)) {
        message("getting cached inverse matrix")
        return(InvX)
    }
    

    data <- x$get()
    InvX <- solve(data, ...)
    
    x$getInverse()
    
    InvX
}
