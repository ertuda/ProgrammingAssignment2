## makeCacheMatrix and cacheSolve functions generates a matrix object
## which keeps a copy of its inverse to be used later without a need to 
## repeat inversion process again

## makeCacheMatrix generates a matrix object that can cache its inverse
## returns a list of functions to set and get solve of matrix

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## cacheSolve computes the inverse of the matrix made by makeCacheMatrix
## takes the already calculated inverse from cahce if possible

cacheSolve <- function(x, ...) {

    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
