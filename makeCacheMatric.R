#creating a special matrix object which can cache it's inverse
#MakeCacheMatrix inspired heavily by makeVector
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  #make inverse a list of zero length
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        #value of the matrix
        get <- function() x
        ##inverse of the matrix should be set by this
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        ##getting the inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}
##solves the inverse matrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        ##getting the matrix inverse
        inv <- x$getinverse()
        ## now make sure the matrix wasn't inverted before
        if (!is.null(inv)) {
                message("getting cached inverse matrix")
                return(inv)
        } 
        else { ##for the case the matrix doesn't exist
                data <- x$get()
                inv <- solve(data, ...)
                ## set the inverse of the matrix 
                x$setinverse(inv)
                inv
        }
}
