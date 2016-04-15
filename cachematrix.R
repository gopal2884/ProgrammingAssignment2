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
        
        ##need to check if the matrix is already cached
        inverse <- x$getcache()
        ## If it is return the cached result otherwise move forward
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        
        ## Go and get the Matrix
        data <- x$get()
        
        ## Solve the Matrix
        inverse <- solve(data, ...)
        
        ## Cache the Matrix
        x$setcache(inverse)
        
        ## Return the inversed Matrix
        return(inverse)
}


## Test Script

rand <- runif(10000)
matrx <- matrix(rand, nrow=100, ncol=100)
x<-makeCacheMatrix(matrx)

start.time = Sys.time()
cacheSolve(x)
dur1 = Sys.time() - start.time


start.time = Sys.time()
cacheSolve(x)
dur = Sys.time() - start.time
print(dur1)
print(dur)