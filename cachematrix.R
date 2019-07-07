## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## the function makeCacheMatrix creates a special matrix whose inverse can be cached.

makeCacheMatrix <- function(x = matrix()) {

        ## initializing the inverse property
        n = NULL

        ## method to set and get the matrix
        set = function(y) {
                x <<- y
                n <<- NULL
        }
        get = function() x

        ## method to set and get the inverse of the matrix
        setinverse = function(inverse) n <<- inverse
        getinverse = function() n

        ## returns a list of get and set methods
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)

}


## Write a short comment describing this function

## the function computes the inverse of the matrix returned by the above function - "makeCacheMatrix".
## if in case, the inverse has already been calculated, the function below should
## retrieve the inverse from the cache. 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        n = x$getinverse()
        
        ## return the inverse if already calculated
        if (!is.null(n)){
                message("getting cached data")
                return(n)
        }     

        ## get the matrix 
        data = x$get()

        ##calculate the inverse of the matrix
        n = solve(data, ...)
        x$setinverse(n)
        
        n

}
