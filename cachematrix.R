## This function computes the inverse of a matrix creating first the object matrix and then computes its inverse using solve R function

## This function creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {

        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
             
}

## This function computes the inverse of an input matrix returned by makeCacheMatrix function using solve R function
cacheSolve <- function(x, ...) {
      
        inv <- x$getinv()
      
        if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv                     
        
}
