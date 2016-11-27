## The bellow two functions are used to compute inverse of a matrix. As this can be 
## computationally intensive, it is preferable to cache the inverse for a spesific matrix
## and reuse it without repeated computation. This is done in the following way:

## makeCacheMatrix takes one argument, a matrix. It will first define the inverse, creating
## the variable in a sense, as NULL. Then, set function is defined, which is used to set ("change")
## the input matrix to parent value and to set inverse to NULL, making sure the cache is 
## empty ( inverse needs to be redefined since the matrix has been changed), it will also
## use parent enviroment values. Get is also defined as a function and it merely retrieves
## the matrix, either frm the argument of the makeCacheMatrix or as the argument from the 
## set function. Setinv function will define inverse through parent environment, while
## getinv function will return inverse as it has been defined in makeCacheMatrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function( y )
    {
        x   <<- y
        inv <<- NULL
    }
    get <- function() x
    
    setinv <<- function( inverse ) inv <<- inverse
    getinv <<- function() inv
    list( set = set, get = get, setinv = setinv, getinv = getinv)
}


## CacheSolve returns value of the inverse. It first checks whether cache already holds the
## value in question, in which case it shows "Getting cached data" message and returns cached 
## value; or it calculates the inverse using Solve function and the input data given to function
## cacheSolve. It also sets (caches) the inverse value to the data variable using setinv function.

cacheSolve <- function(x, ...)
{
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()
    if(!is.null( inv )) {
        message( "Getting cached data" )
        return( inv )
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinv( inv )
    inv
}
