## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## Creates a matrix object
makeCacheMatrix <- function(x = matrix()) {
    ## Initialization
    m <- NULL
    
    ## Set a new matrix and unsets the cache
    set <- function(y) {
        ## Check if setting a new matrix
        if(!is.matrix(y)) {
            stop("Not a matrix");
        }
        x <<- y
        m <<- NULL
    }
    
    ## Get the matrix
    get <- function() x
    
    ## Sets the inverse of the matrix
    setsolve <- function(inverse) m <<- inverse
    
    ## Gets the inverse of the matrix
    getsolve <- function() m
    
    ## List the details of the matrix
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## Returns the inverse of the special "matrix" returned by makeCacheMatrix above 
## If the inverse is cached, it returns the cached inverse. 
## Otherwise, it calculates the inverse and sets the inverse cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    ## checking if the inverse is cached
    m <- x$getsolve()
    if(!is.null(m)) {
        ## the inverse is cached, returning the cached inverse
        message("getting cached data")
        return(m)
    }
    
    ## the inverse is not cached, calculating the inverse
    
    ## getting the matrix
    data <- x$get()
    
    ## calculating the inverse
    m <- solve(data)
    
    ## setting the cache for inverse of the matrix
    x$setsolve(m)
    
    ## return the calculated inverse
    m
}