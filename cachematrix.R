## Matrix inversion can be a costly computation, particularly if the inverse is
## calculated repeatedly (e.g. in a for loop). makeCacheMatrix and cacheSolve
## provide a pair of functions that cache the inverse of a matrix and return 
## the cached version of the inverse if the matrix has not changed.
##
## Usage:
##
## Create sample matrix
## m<-matrix(c(10,1,20,3,5,8,13,21,34,55,5,144,233,377,610,987),nrow=4,byrow=T)
##
## Make a cacheable version of our matrix
## cm <- makeCacheMatrix(m)
##
## Get the inverse of our matrix. The first time we request the inverse, the
## function calculates the inverse.
## cacheSolve(cm)
##[,1]         [,2]        [,3]        [,4]
##[1,]  1.111111e-01  -0.76530612  0.02380952  0.01247166
##[2,]  2.222222e-01 -48.52891156  0.03571429  1.02664399
##[3,] -1.290600e-21   0.00170068 -0.01190476  0.00170068
##[4,] -1.111111e-01  18.71598639 -0.01190476 -0.39512472
##
## Request the inverse of the matrix once more. This time, the function
## returns the cached version of the inverse instead of calculating it.
## a <- cacheSolve(cm)
## Returning cached inverse
## a
##[,1]         [,2]        [,3]        [,4]
##[1,]  1.111111e-01  -0.76530612  0.02380952  0.01247166
##[2,]  2.222222e-01 -48.52891156  0.03571429  1.02664399
##[3,] -1.290600e-21   0.00170068 -0.01190476  0.00170068
##[4,] -1.111111e-01  18.71598639 -0.01190476 -0.39512472
##
## Create a new matrix and use it to update our special cacheable matrix
## n=matrix(c(9,10,3,20,25,30,35,40,45,50,3,8,65,70,75,80), nrow=4, byrow=T)
## cm$setMatrix(n)
##
## Get the inverse of our new matrix and note that the cache has changed
## b <- cacheSolve(cm)
## identical(a,b)
##[1] FALSE


## Creates a special "matrix" object that can cache its inverse. The matrix
## object consists of a list of functions to set the value of the matrix, get
## the value of the matrix, set the value of the cached inverse, and to get
## the value of the cached inverse.
makeCacheMatrix <- function(x = matrix()) {
    ## Cached version of matrix; initially NULL indicating not calculated yet
    inverse <- NULL
    ## Function to set the value of the matrix; resets cached inverse to
    ## NULL indicating because the matrix has been changed and no cached value
    setMatrix <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    ## Function to return the value of the matrix
    getMatrix <- function() x
    ## Function to update the value of matrix inverse
    setInverse <- function(i) inverse <<- i
    ## Function to return the value of the matrix inverse
    getInverse <- function() inverse
    
    ## Create the special matrix object as a list of access functions
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}

## Computes the inverse of a matrix object created with makeCacheMatrix.
## If the inverse has already been calculated and the matrix has not changed,
## then the function retrieves the inverse from the cache instead of
## caculating it again.
cacheSolve <- function(x, ...) {
    ## Retrieve the cached version of the inverse
    inverse <- x$getInverse()
    ## If the inverse has already been calculated return that value
    if(!is.null(inverse)) {
        message("Returning cached inverse")
        return(inverse)
    }
    ## Otherwise get the source matrix, calculate the inverse, and cache it
    data <- x$getMatrix()           ## Get original source matrix
    inverse <- solve(data, ...)     ## Calculate the inverse
    x$setInverse(inverse)           ## Cache the inverse 
    inverse                         ## and return it.
}
