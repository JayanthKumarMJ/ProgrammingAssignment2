## makeCacheMatrix funciton caches the matrix and it's inverse
## cacheSolve function checks if 
## the inverse of the matrix is already calculate if not calculates and stores it

## caches the matrix and it's inverse.

makeCacheMatrix <- function(x = matrix()) {
    invM <- NULL
    set <- function(matrix){
        x <<- matrix
        invM <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inverse) invM <<- inverse
    
    getInverse <- function() invM
    
    list(set = set, get = get, setInverse = setInverse, 
         getInverse = getInverse )
}


## checks if matrix inverse is already available in cache, 
## if not calculates the inverse and stores for the next computation.

cacheSolve <- function(x, ...) {
    invMat <- x$getInverse()
    if(!is.null(invMat)){
        message("got data from cache")
        invMat
    }
    matrix <- x$get()
    invMat <- solve(matrix, ...)
    x$setInverse(invMat)
    invMat
}