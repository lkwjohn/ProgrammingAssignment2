## Put comments here that give an overall description of what your
#Step to use the matrix cahce function
#set.seed(1)
#a <- makeCacheMatrix(matrix(runif(16,1, 10), ncol=4))
#set.seed(2)
#b <- makeCacheMatrix(matrix(runif(16,1, 10), ncol=4))

#compare the 2 matrix to ensure they are not the same!

#cacheSolve(a)
#New
#[,1]        [,2]       [,3]         [,4]
#[1,] -0.38915474  0.12559816  0.0683678  0.277015941
#[2,]  0.03811230  0.09077431 -0.1268085  0.005974722
#[3,]  0.45822876 -0.09047967  0.0858147 -0.432591651
#[4,] -0.09095902 -0.09240122  0.0732453  0.186122279
#cacheSolve(a)
#getting cached data
#[,1]        [,2]       [,3]         [,4]
#[1,] -0.38915474  0.12559816  0.0683678  0.277015941
#[2,]  0.03811230  0.09077431 -0.1268085  0.005974722
#[3,]  0.45822876 -0.09047967  0.0858147 -0.432591651
#[4,] -0.09095902 -0.09240122  0.0732453  0.186122279
#cacheSolve(c)
#New
#[,1]       [,2]       [,3]       [,4]
#[1,]  0.16720509  1.3333366 -0.3792527 -1.6612431
#[2,] -0.09385804 -0.7910760  0.3600236  0.9053094
#[3,] -0.02005985  0.4969901 -0.1656700 -0.2877070
#[4,]  0.01026516 -0.9930744  0.1956493  1.1536008


## functions do
## Write a short comment describing this function
#This function takes in the matrix and store it into its local environment
#The inverse result is also store
#This function structure using setter and getter concept from programming
#Thus allowing other function to get the data stored previously
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
#This function accept a input and check whether is there a matrix being stored before
#if the matrix is not stored, then it will compute it and store it
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
