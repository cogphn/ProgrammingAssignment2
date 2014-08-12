## Put comments here that give an overall description of what your
## functions do

## This function caches all information about the matrix object we plan
## to process. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        setMatrix <- function(y){
                i <<- NULL
                x <<- y
        }
        getMatrix <- function() x
        getInverse <- function() i
        setInverse <- function(inverse) i <<- inverse
        list(setMatrix = setMatrix, getMatrix = getMatrix, getInverse=getInverse, setInverse = setInverse)
                
}


## uses the MASS library to calculate the inverse of a matrix within the makeCacheMatrix function, once 
## it has not been previously calculated 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        
        if(!is.null(inverse)){ 
                message("getting cached data")
                return(inverse)
        }
        m <- x$getMatrix()
        
        library(MASS)
        inv <- ginv(m)
        x$setInverse(inv)
        detach("package:MASS", unload=T)
        inv
        
}
