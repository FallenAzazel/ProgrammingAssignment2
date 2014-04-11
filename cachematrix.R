## Coursera - R Programming by Roger D. Peng
## Programming Assignment 2 - Peer Reviewed

## Utilising R, this assignment aims to using caching techniques to speed up CPU costly calculations, in this
## case calculations on matrices. This involves the use of two functions - 
##           'makeCacheMatrix' This function creates a special "matrix" object that can cache its inverse
##	     'cacheSolve'      This function computes the inverse of the special "matrix" returned by makeCacheMatrix.
##                             If the inverse has already been calculated (and the matrix has not changed), then the 
##                             cachesolve should retrieve the inverse from the cache.

## Objects 'x' will be the matrix and 'inv' its solved inverse matrix

## After creating the special matrix object, this function will cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        set_inv <- function(solve) inv <<- solve
        get_inv <- function() inv
        list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv)
		
}


## This function will check to see if the solution to the inverse matrix 'x' has already been cached, if it has then 
## will return the cached data, if not  will go ahead and calculate the inverse, cache it and return it.


cacheSolve <- function(x, ...) {
   ## Return a matrix that is the inverse of 'x'

		
		#Checks to see if the solution to the inverse of the matrix object x is already cached
        inv <- x$get_inv()
        if(!is.null(inv)) {
                message("getting cached inverse matrix data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$set_inv(inv)
        inv
 
}
