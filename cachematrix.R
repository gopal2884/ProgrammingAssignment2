##makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
##cacheSolve: This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
##Computing the inverse of a square matrix can be done with the solve function in R. 

##For example, if X is a square invertible matrix, then solve(X) returns its inverse.

## create and set the matrix type

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        
        ## set the matrix
        
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        ## get the matrix
        get <- function() x
        
        ## set and get the cached matrix
        setcache <- function(inversematrix) inverse <<- inversematrix
        getcache <- function() inverse
        
        ##this returns the type of the cached matrix
        list(set = set, get = get,
             setcache = setcache,
             getcache = getcache)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ## need to use SOLVE
        inverse <- x$getcache()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setcache(inverse)
        return(inverse)
}

## Test Script

x <- matrix(1:9, nrow=3, ncol = 3)
xx <- makeCacheMatrix(x)
xx$get()
xx$setcache(solve(x))
xx$getcache()

