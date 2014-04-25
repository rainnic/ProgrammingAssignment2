## Assignment: Caching the Inverse of a Matrix
## Following the instructions I have created two functions:
## 1) makeCacheMatrix
## 2) cacheSolve
##
## Using the scope rules of the R language, it is possible to put in cache
## the matrix inversion and then solve it reducing the computation time.
##
## Usage:
## NOTE: It works only and only if "matrix_example" is a square matrix 
## and its determinant is not 0, otherwise you will recieve an error message.
## matrix_example <- matrix(runif(16, 3.0, 7.5),4,4)
## a <- makeCacheMatrix(matrix_example)
## b <- cacheSolve(a)
##
## Try print(matrix_example %*% b, digits= 0), the result will be roughly I (identity matrix)
##


## The following function creates a special "matrix" object
## that can cache its inverse.
## It is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse
## get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inverse <<- inverse
        getinverse <- function() inverse
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getinverse()
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data, ...)
        x$setinverse(inverse)
        inverse
}
