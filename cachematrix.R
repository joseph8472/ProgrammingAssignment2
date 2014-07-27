## Put comments here that give an overall description of what your
## functions do

## Function creates an object with 4 methods
##      set() - sets object's internal matrix or creates an empty one by default
##      get() - returns internal matrix
##      setinverse() - assigns an inverse matrix to the object
##      getinverse() - returns internal inverse matrix

makeCacheMatrix <- function(x = matrix()) 
{
    m <- NULL
    set <- function(y) 
    {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(matrixinverse) m <<- matrixinverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)    
}

## Function solves inverse of matrix x if it hasn't already been
## cached. If it has - returns the cached version

cacheSolve <- function(x, ...) 
{
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) 
    {
        message("getting cached data")
        return(m)            
    }
    message("solving inverse matrix")
    
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m    
}
