## This file create cacheinverse that load matrix inverse from cache



##  The first function creates a special "matrix" object 
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    if (dim(x)[1]==dim(x)[2]){
        
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
    else {print("It's not a square matrix!")}
    
}


## The second function compute inverse or pass cache inverse


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv<- solve(data, ...)
    x$setinverse(inv)
    inv
    
    
}
