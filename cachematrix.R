## makeCache matrix creates a special matrix object that contains two internal variables and a handful of functions
## to get and set those variables.
## cacheSolve controls whether the inverse for matrix object passed to it be calculated, or retrieved from the cache.


# Uncomment to clean the workspace variables if required.
rm(list=ls(all=TRUE))


## The makeCacheMatrix creates a matrix object in in an alternate environment.
makeCacheMatrix <- function(x = matrix()) {
    # Variables
    m <<- x
    inv <<- NULL
    # Getter and Setter Functions for each of the above variables
    setm <- function(x) {m <<- x}
    getm <- function() m
    setinv <- function(x) {inv <<- x}
    getinv <- function() inv
    list(setm = setm, getm = getm, setinv = setinv, getinv = getinv)
}


## The cacheSolve is like a helper function which manages whether the inverse has to be calculated or used
## from the cache.
cacheSolve <- function(x, ...) {

    # Read in the provided matrix object
    this.inv <- x$getinv()
    # If there is already a cached value of inv available, just return the cached value
    if(!is.null(this.inv)) {  
        message("getting cached data")
        return(this.inv)
    }

    # Otherwise, calculate the inverse and update the cache
    this.mat <- x$getm()
    
    # Calculate the inverse, Show the output, Update the cache value
    sol <- solve(this.mat)
    print(this.mat)
    x$setinv(sol)
}


# Testing functionality
mat <- makeCacheMatrix(matrix(c(4, 3, 6, 1, 7, 3, 1, 4, 4), nrow = 3, ncol = 3))  # Create a matrix object
cacheSolve(mat)  # solve for the inverse. For the first time, it is calculated for real.
cacheSolve(mat)  # for later attempts, the cached values are used


