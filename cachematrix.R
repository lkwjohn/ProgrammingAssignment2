## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    matrix <- NULL
    #setter
    set <- function(x, inverse){
        matrix <<- x
        inverseMatrix <<- inverse
    }
    get <- function() x
    
    #getter
    getMatrix <- function() matrix
    getInverseMatrix <- function() inverseMatrix
    
    list(set = set, get = get, 
         getMatrix = getMatrix,
         getInverseMatrix = getInverseMatrix)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    matrix <- x$getMatrix()
    if(!is.null(matrix)){
        message("getting cached data")
        return(x$getInverseMatrix())
    }
    else{ # either cached matrix is null or new data
        message("New")
        inverseMatrix <- solve(x$get(), ...)
        x$set(x$get(), inverseMatrix)
    }
    inverseMatrix
}
