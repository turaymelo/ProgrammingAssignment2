## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##This function returns the inversed matrix of the input matrix
makeCacheMatrix <- function(x = matrix()) {
		m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        computeinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             computeinv = computeinv,
             getinv = getinv)
}


## Write a short comment describing this function
##This functiona verifies if the matrix is in cache. In case it is, it returns it, otherwise it calculates
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		 m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$computeinv(m)
        m
}