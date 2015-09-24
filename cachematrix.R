## The functions below work together to take an input matrix (which has an inverse 
## possible) to first check to see if the matrix inverse has been already solved.
## If it has been solved, it recalss that solution, otherwise it solves for the 
## inverse and then returns it.

## makeCacheMatrix takes an input matrix, x, and creates a special list which
## serves as a "cache" from which it can either be solved, or the soltion can
## be recalled if it has been solved already.

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


## cacheSolve takes the special list and checks it to see if the input
## matrix has been solved already.  If so, it returns the inverse.  If not,
## it solves it then returns the inverse.

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
## Return a matrix that is the inverse of 'x'
}
