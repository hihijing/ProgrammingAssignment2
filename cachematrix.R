# The two functions overall cache the inverse of a matrix.
# Assumption: the matrices to be calculated are all invertible.



# makeCacheMatrix() builds a set of 4 functions
#               -- set and get the value of the matrix
#                  set and get the value of the inverse if the matrix --
# and returns the functions within a list to the parent environment.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse, getinverse = getinverse)
}



# cacheSolve() calculate and store the inverse of a matrix
# for the input argument of makeCacheMatrix.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse() ## Return a matrix that is the inverse of 'x'
        if(!is.null(inv)) {
                message("Getting cached data.")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)   ## set inverted matrix in cache
        inv
}
