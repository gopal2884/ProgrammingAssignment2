## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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
        setcache <- function(inversematrix) inverse <<- matrixinverse
        getcache <- function() inverse
        
        ##this returns the type of the cached matrix
        list(set = set, get = get,
             setcache = setcache,
             getcache = getcache)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
