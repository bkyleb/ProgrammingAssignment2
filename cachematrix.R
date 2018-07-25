## makeCacheMatrix and cacheSolve, this pair of functions cache the inverse of an input matrix
## without computing repeatedly, this pair of funtions return cached inverse of the matrix 
## if its contents are not changing

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        v<-NULL
## if setting a new matrix to the function, v will return to null and the pair functions will recaculate
## the inverse of the new input matrix
        set<-function(y){
                x<<-y
                v<<-NULL
        }
## output the matrix
        get<-function()x
## cache the inverse
        setinverse<-function(inverse) v<<-inverse
## output  the inverse caculated in the function cacheSolve
        getinverse<-function()v
## create a list of the nested functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
## assgin inverse value from makeCacheMatrix function to v
        v <-x$getinverse()
## if the inverse of the input matrix is not calculated before, v is null and skip if statement
## if the inverse is calculated before, cacheSolve will return the message and pre-caculated value "v"
        if(!is.null(v)){
                message("getting cached data")
                return(v)
        }
## the new input matrix is assigned to "data" and inverse will be caculated and assigned to v
## the caculated inverse is cached and printed
        data<-x$get()
        v<-solve(data,...)
        x$setinverse(v)
        v
}