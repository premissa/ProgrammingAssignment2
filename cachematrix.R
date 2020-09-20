## This script provides two functions. The pair of functions calculates the inverse
## of a matrix given by a parameter, The inverse is stored in the cache that 
## makes possible to avoid multiple calculations of the same inverse,

## This function provides a list, a cache matrix which stores the matrix, and 
## provides function to acces its values along with its inverse. 

makeCacheMatrix <- function(x = matrix()) {
    
        m <- NULL
        set <- function(y){
            
                x <<- y
                m <<- NULL
        }
    
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
    
        list(set = set, get = get, setinv = setinv, getinv = getinv)
    
}


## This function uses the cacheMatrix for inverse calculations. If the inverse
## is available, the inverse won't be calculated, but will be obtained from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<- x$getinv()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data,...)
        x$setinv(m)
        m
}
