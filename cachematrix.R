## The functions are designed to cache the inverse of a square matrix.
## Because computing the inverse of a square matrix may take too long 
## for a big matirx. If the contents of a matrix are not changing, it may make
## sense to cache the inversed matrix so that when we need it again, it
## can be looked up in the cache rather than recomputed. 

## This function creates a special "matrix" object that can cache its inverse

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
         getsolve = getsolve)
}

## This function computes the inverse of the special "matrix" 
returned by `makeCacheMatrix` above. If the inverse has already 
been calculated (and the matrix has not changed), then `cacheSolve` 
should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    s <- x$getsolve()
    if(!is.null(s)) {
        message("getting cached data")
        return(s) ## Return a matrix that is the inverse of 'x'
    }
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s ## Return a matrix that is the inverse of 'x'
}

