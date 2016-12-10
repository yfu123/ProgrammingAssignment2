## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly
## The following two functions cache the inverse of a matrix.

## makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse.

## The function creates a list containing the following 4 functions:
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse matrix
## 4) get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
        x_inv <- NULL
        
        set <- function(y) {
                x <<- y
                x_inv <<- NULL
        }
        get <- function() x
        
        setinv <- function(solve) x_inv <<- solve
        getinv <- function() x_inv
        
        list(set = set, get = get, setinv = setinv, getinv = getinv)
        
}


## cacheSolve: 
## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the inverse 
## from the cache. Otherwise, it calculates the inverse of the data and sets the 
## value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        x_inv <- x$getinv()
        
        if(!is.null(x_inv)) {
                message("getting cached data")
                return(x_inv)
        }
        data <- x$get()
        x_inv <- solve(data,...)
        x$setinv(x_inv)
        x_inv
}
