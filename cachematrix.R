## The following two functions cache the inverse of an invertible matrix
## along with the original value of the invertible matrix in a data structure
## called makeCacheMatrix
##
## Code and solution is based on stub from rdpeng and example code in
## ProgrammingAssignment2/README.md

########################################################################
########################################################################

## makeCacheMatrix is a data structure that takes a matrix as input, then
## stores the input matrix and its inverse (if one is available).
##
## makeCacheMatrix is stored internally as a list of 4 functions.
##
## x: variable storing the value of the matrix
## inverse: variable storing the value of the inverse matrix, or NULL if the 
##          inverse has yet to be calculated
##
## set(y): change the value of x to that stored in y
## get(): get the value of x
## setinverse(inv): change the value of inverse to that store in inv
## getinverse(): get the value of inverse

########################################################################
########################################################################
makeCacheMatrix <- function(x = matrix()) {
    ## initialize the value of inverse to NULL
    inverse <- NULL
    
    ## FUNCTION: change the value of the matrix x to y and reset the value of 
    ## inverse to NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }

    ## FUNCTION: get the value of the cached matrix
    get <- function() x
    
    ## FUNCTION: Assume inv is a matrix and store the value of inv in inverse
    setinverse <- function(inv) inverse <<- inv
    
    ## FUNCTION: get the value of the cached inverse matrix
    getinverse <- function() inverse
    
    ## return the list consisting of the 4 functions: set(), get(),
    ## setinverse() and getinverse()
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

########################################################################
########################################################################

## cacheSolve takes an object of type makeCacheMatrix as input, then determines
## whether or not the inverse of the matrix stored in makeCacheMatrix has been 
## calculated
##
## If so, cacheSolve simply returns the cached inverse matrix stored in 
## makeCacheMatrix
##
## Otherwise, it calculates the inverse matrix of the matrix stored in 
## makeCacheMatrix, then modifies the value of the variable inverse in 
## makeCacheMatrix to the calculated inverse matrix via the setinverse()
## function in the makeCacheMatrix data structure

########################################################################
########################################################################
cacheSolve <- function(x, ...) {
    ## initialize the value of inverse based on the value from the getinverse()
    ## function of the makeCacheMatrix data structure x
    inverse <- x$getinverse()
    
    ## Determine the inverse matrix has already been calculated and, if so,
    ## return that inverse
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    
    ## If the inverse matrix has not been calculated:
    data <-x$get()                  ## get the value of the original matrix
    inverse <- solve(data, ...)     ## use solve() to calculate the inverse
    x$setinverse(inverse)           ## store the value of inverse in x
    inverse                         ## return the inverse matrix
}