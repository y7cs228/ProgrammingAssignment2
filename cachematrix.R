## Put comments here that give an overall description of what your
## functions do

##There are two functions makeCacheMatrix and cacheSolve in this R Script.

##makeCacheMatrix- This function creates a special "matrix" object that can cache its inverse.

##cacheSolve - This function returns the inverse of a matrix x which was already
## computed. If not it will compute the inverse and caches its result

## Write a short comment describing this function

## This function returns a list of 4 functions which are 
## set - which sets the new matrix to x and initializes the inverse to null
## get - returns the matrix
## setinverse - caches the inverse of a matrix
## getinverse - returns the cached inverse of a matirx

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x   <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) inv <<- inv
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## Write a short comment describing this function

## The below function will return the inverse of a matrix if the original matrix
## was not changed. If the original matrix is changed then it will again calculate
## the inverse of matrix and it caches the result . 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached inverse of the matrix")
                return(inv)
        }
        res <- x$get()
        inv <- solve(res)
        x$setinverse(inv)
        inv
}
