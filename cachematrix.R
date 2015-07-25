
## makeCacheMatrix function creates a special matrix object which 
## stores its inverse.
##      Input: matrix whose inverse is cached
##      It contains four subfunctions:
##       - set: stores the input matrix of the function and initializes the 
##         cached inverse to null
##       - get: returns the matrix stored
##       - setinv: stores the inverse of the input matrix
##       - getinv: returns the inverse of the matrix stored


makeCacheMatrix <- function(x = matrix()) {
        
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse_matrix) inv <<- inverse_matrix
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## cachesolve function returns the inverse of a matrix 
##      Input: matrix previously created with makeCacheMatrix function
##      whose inverse we want to obtain
##      The function checks if the inverse of the input matrix has been 
##      already computed and cached
##              If true, then returns the cached inverse matrix
##              If not, obtains the matrix by means of the function, calculates
##              the inverse by means of solve function and stores the inverse 
##              using the setinv function for future uses

cacheSolve <- function(x, ...) {
        
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data_matrix <- x$get()
        inv <- solve(data_matrix, ...)
        x$setinv(inv)
        inv
    
}
