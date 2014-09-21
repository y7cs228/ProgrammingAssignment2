## ----------------------
## Script Description: ##
## ----------------------
## This script is used to illustrate the concept of lexical scoping in R by calculating
## and storing the inverse of a matrix in cache and retrieving it later from cahe.
## In this script we are using a special assignment operator "<<" to distinguish 
## between the global environment and function environment. 


## ----------------------
## Functions  used: ##
## ----------------------
## 1.makeCacheMatrix - This function creates a special "matrix" object that can cache its inverse.
## 2.cacheSolve      - This function returns the inverse of a matrix x which was already
##                     computed. If not it will compute the inverse and caches its result

## ------------------------
## Function Description: ##
## ------------------------

## 1. makeCacheMatrix
## Input  - A matrix which is invertible
## Output - This function returns a list of 4 functions which are 
##    1. set - which sets the new matrix to x and initializes the inverse to null
##    2. get - returns the matrix
##    3. setinverse - caches the inverse of a matrix
##    4. getinverse - returns the cached inverse of a matirx

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ## This set function will sets a new matrix and initializes the inverse
        ## to a NULL value.
        
        set <- function(y) {
                x   <<- y
                inv <<- NULL
        }
        get <- function() x
        ## This will set the inverse of the new matrix in Global Environment
        setinverse <- function(inv) inv <<- inv
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## ------------------------
## Function Description: ##
## ------------------------

## 2.cacheSolve
## Input  - Takes a matrix as input
## Output - It will return the inverse of a matrix if the original matrix
##          was not changed. If the original matrix is changed then it will again calculate
##          the inverse of matrix and it caches the result . 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached inverse of the matrix")
                return(inv)
        }
        res <- x$get()
        ## Solve function returns the inverse of a non-singular matrix
        inv <- solve(res)
        ## This will store the inverse of the matrix computed above.
        x$setinverse(inv)
        inv
}
