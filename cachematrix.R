## This function  creat the inverse of a matrix

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        m<- NULL
        set<- function(y){
                x<<- y
                m<<- NULL
        }
        get<- function() x
        setinverse<- function(solve) m<<- solve
        getinverse<- function() m
        list(set=set,get=get,
             setinverse=setinverse,
             getinverse=getinverse)

}


## This function check whether the inverse has been computed
## If it is, it will capture the result, if it is not, it will compute it.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
