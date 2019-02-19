## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                             ## initialize inv as NULL, inv- the value of matrix inverse
        set <- function(y) {                    ## define 'set' function - assign new
                 x <<- y
                inv <<- NULL   
        }
        get <- function() x                     ## define the 'get' fucntion - return the value of the matrix 
        setinverse <- function(inverse) inv <<- inverse  ## 'setinverse'function - assign the value of inv in parent environment
        getinverse <- function() inv                   ## 'getinverse'function - get the value of inv where called
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {                             ## if inv is not null
                message("getting cached data")  
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)                               ## retrieve the inverse from the cache
        inv
}
