## Put comments here that give an overall description of what your
## functions do:
## makeCacheMatrix() takes a matrix and generates an object which 
## will store the matrix along with it's inverse. cacheSolve() takes 
## the "matrix object" created by makeCacheMatrix() and computes 
## the inverse (if it hasn't already been computed) and stores it in 
## the object. If it has been pre-computed then cacheSolve() just 
## returns the inverse from the object. 

## Write a short comment describing this function:
##
## Creates special "matrix" object that can cache its inverse.
## the marix x and inverse inv are stored in the environment of
## makeCacheMatrix().
## CacheSolve() calls various functions in makeCacheMatrix()
## to set the inverse and retrieve the inverse if it has already
## been caclulated

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function:
##
## Takes special list/matrix object which contains the matrix and 
## the cached inverse (NULL on first call of cacheSolve) of the matrix. 
## If the inverse hasn't been cached in the object, solve() is called
## to generate the inverse and the setinverse() function to cache it in
## the list/matrix object. If it has been called previously on the object
## then the getinverse() function returns the cached inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
