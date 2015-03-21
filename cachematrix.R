## These functions act in conjunction to allow the cacheing
## of the inverse of a matrix so as to save computation time
## if the inverse will be needed repeatedly.
## usage:
## start with an invertible matrix 'c'
## x <- makeCacheMatrix(c)
## x$get()   ## will return the original c matrix
## x$getsolve()   ## will initially return NULL before cacheSolve is run
## cacheSolve(x)  ## will calculate and return the inverse of c
## x$getsolve()   ## will now return the cached copy of the inverse of c
## cacheSolve(x)  ## will now return the cached copy of the inverse of c
##                   thereby saving computation cycles instead of using 
##                   solve(c) repeatedly for the inverse


## This function will make a matrix object that will be able to cache
## its own inverse after the related function cacheSolve has been 
## run the first time.

makeCacheMatrix <- function(x = matrix()) {
    s <- NULL
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve
    )
}


## This function will first check for the existence of 
## the inverse of the matrix in the makeCacheMatrix object
## and will return the cached copy of the matrix if it 
## exists.  If not, it will calculate the inverse of the 
## given matrix and cache it in the makeCacheMatrix object
## for future calls.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
