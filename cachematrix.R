## makeCacheMatrix and cacheSolve, this pair of functions cache the inverse of an input matrix
## without computing repeatedly, this pair of funtions return cached inverse of the matrix 
## if its contents are not changing

## makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        v<-NULL
        set<-function(y){
                x<<-y
                v<<-NULL
        }
        get<-function()x
        setinverse<-function(inverse) v<<-inverse
        getinverse<-function()v
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## cacheSolve function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed), 
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        v <-x$getinverse()
        if(!is.null(v)){
                message("getting cached data")
                return(v)
        }
        data<-x$get()
        v<-solve(data,...)
        x$setinverse(v)
        v
}