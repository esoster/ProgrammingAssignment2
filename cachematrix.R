## set of two functions; inverse of m1 is stored in the cache; whenver x is reset, the value of m1 cahced inthe object is cleared, 
## forcing cacheSolve() to recalculate the inverse

## function that creates a special 'matrix' object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {     ## initialize x as a function arg. with default value of empty matrix
        m1 <- NULL              ## initializes object m1
        set <- function(y) {   ## set the value of the vector
                x <<- y         ## assign the input argument to x in the parent environ.
                m1 <<- NULL     ## assign NULL to m1 in the parent environ., will clear
                                ## any value of m1 cached by prior execution of cachesolve()
        }
        get <- function() x     ## get the value of the matrix
        setinverse <- function(solve) m1 <<- solve ## set the value of inverse
        getinverse <- function() m1 ## get the value of inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
        ## creates a special vector which is really a list containing a function to
        ## set the value of the matrix, get the value of the  matrix, set the value of inverse
        ## get the value of inverse and allows you to call these by name
}


## calculates the inverse of the special 'matrix' above, 1st checks to see if this is already calculated, if so, gets it from cache
## if not, calculates the inverse and sets the value in the cache (assuming matrix has not changed)

cacheSolve <- function(x, ...) {
        m1 <- x$getinverse()
        if(!is.null(m1)) {      ## is there an inverse in the cache already?
                message("getting cached data")
                return(m1)      ## return cached inverse
        }
     data <- x$get()           ## if no cached inverse, find the inverse
     m1 <- solve(data, ...)     
     x$setinverse(m1)           ## and set as the inverse in the cache
     m1
}
