## These two functions cache the inverse of a matrix to avoid recalcultate the inverse each
## time we need it.
## use example: whith mat a inversible square matrix 
## First create the special cache matrix : mat_cache <- makeCacheMatrix(mat)
## the to obtain the inverse of the matrix : cacheSolve(mat_cache).

## put a matrix in cache memory and return function to use it
## parameter : the matrix to put in cache 
## return a list of function in order to manipulate the cache matrix
## set : set the value of the matrix
## get : return the value of the matrix
## setsolve : set the value of the inverse of the matrix
## getsolve : get the value of the inverse of the matrice

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
            x <<- y
            m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set,
         get = get,
         setsolve = setsolve,
         getsolve = getsolve)    
}


## Check if the inverse of the matrix is store in cache and if not calculate and store it
## 
## return the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <-x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
