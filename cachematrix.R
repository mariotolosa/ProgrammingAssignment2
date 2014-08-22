## The function creates a special vector which contains the matrix to be inverted, 
## the function to get that matrix, the function to invert it and the function to get the inverted matrix


makeCacheMatrix <- function(x = matrix()) {
 m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)


}


## This function checks if the matrix has already been inverted, if it has it gets the inverted matrix, 
## if it does not then it inverts it
cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
