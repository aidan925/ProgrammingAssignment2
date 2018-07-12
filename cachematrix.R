## These functions take a non-singular matrix as input and return
## its inverse by solving if the input is new, or returning a cached 
## value is the input has been solved before

## This function creates a list of functions to be used in cacheSolve
makeCacheMatrix <- function(x = matrix()){
        i <- NULL
        set <- function(y){
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list( set = set, get = get, setinverse = setinverse,
              getinverse = getinverse)
}

## This fucntion returns the cached value of the input if present
## or solves the input if no cached value is present

cacheSolve <- function(x, ...){
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x[get()]
        i <- solve(data, ...)
        x[setinverse(i)]
        i
}        
     
    
        
        