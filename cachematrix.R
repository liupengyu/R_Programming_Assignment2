## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        inverseMt <- NULL
        set <- function(y) {
                x <<- y
                inverseMt <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) inverseMt <<- inverse
        getInverse <- function() inverseMt
        list(set = set,
             get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		 ## Return a matrix that is the inverse of 'x'
        inverseMt <- x$getInverse()
        if (!is.null(inverseMt)) {
                message("getting cached data")
                return(inverseMt)
        }
        mat <- x$get()
        inverseMt <- solve(mat, ...)
        x$setInverse(inverseMt)
        inverseMt
}
