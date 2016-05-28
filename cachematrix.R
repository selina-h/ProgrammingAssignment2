## makeCacheMatrix takes a matrix and take the inverse and store it in the 
## global function
## cacheSolve looks for existing cached inverse of the matrix, and prints the
## inverse. If no cache exist then it solves for the inverse and prints it


## makeCacheMatrix returns a list that has the matrix and the inverse stored 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get, 
             setinv = setinv,
             getinv = getinv)
}


## cacheSolve looks for the stored inverse, returns a msg and the inverse if
## found. If not solves for the inverse and prints it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m
}
