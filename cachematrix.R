## Uses a function to generate an environment in which to cache a matrix object. 
##This cache will be used to store the results of the calculation of a matrices inverse
## Code structure is based on the functions to cache a vector located at 
## https://github.com/rdpeng/ProgrammingAssignment2/blob/master/README.md


## This function creates an environment in which we can store and modify a variable I.
##  as well as a matrix x.  "I" will act as a container for an inverse matrix.  
## Along with tools to manipulate "I".  The final output will be a list of functions.

makeCacheMatrix <- function(x = matrix()) {
	I <- NULL
	## The set function allows for the input matrix of the function to be changed.
	## also overwrites the cached matrix with NULL to allow for recalculation.
	setx <- function(y) {
		x <<- y
		I <<- NULL
	}
	## the get function returns the currently stored matrix
	getx <- function() x
	## the set solve function creates and stores an inverse of the x matrix in I
	setinverse <- function(s) I <<- s
	## the get inverse function retrieves the currently stored value of I
	getinverse <- function() I
	##outputs a list of functions
	list(setx = setx, getx = getx, setinverse = setinverse, getinverse = getinverse)
}


## This function retrieves the storage variable form makeCacheMatrix and tests 
## it to determine its state.  If it is empty it will use solve to compute the inverse
## and store it in makeCacheMatrix, otherwise it will simply return the stored value.
## This function accepts the list of functions from makeCacheMatrix as input as well as 

cacheSolve <- function(x, ...) {
    ## retrieves the currently cached value of m from the make inverse function
	I <- x$getinverse()
	## test if the I already has a value and returns it if it does
    if(!is.null(I)) {
        message("Retrieving cached inverse")
        return(I)
    }
	## retrieves the currently active x value form the makeinverse fucntion
    data <- x$getx()
	##  creates the inverse and stores it localy as value m
    I <- solve(data, ...)
	## writes the value m obtained above to the cached value m in the make
	## inverse function.
    x$setinverse(I)
    I
}
