## These functions implement a cache version of matrix inversion, which is a
## potentially time-consuming computation.

## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse
## It returns a list containing four functions:
## 	set: a function to set the value of the matrix
## 	get: a function to get the value of the matrix
## 	setinverse: a function to set the value of the inverse
## 	getinverse: a function to get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) m <<- inverse
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The function cacheSolve computes the inverse of a matrix of the type returned by makeCacheMatrix above.
## First, the function checks to see if the inverse has already been calculated. In this case the
## 	function will retrieve the inverse from the cache and will skip the computation.
## Otherwise it will calculate the inverse of the matrix and set the value of the inverse
## 	in the cache via the setinverse function. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## First check if the inverse has already been calculated
        m <- x$getinverse()
        if(!is.null(m)) {                       ## if it has, then simply return it from the cache and end
                message("getting cached data")
                return(m)
        }							
        data <- x$get()                         ## if it hasn't, then get the matrix
        m <- solve(data, ...)                   ## compute the inverse of the matrix
        x$setinverse(m)	                        ## update the cache with the inverse of the matrix
        m                                       ## and finally return the inverse
}
