## The makeCacheMatrix function holds the matrix we plan on working with
## The cacheSolve function calculates the inverse of the stored matrix
## and stores it within the makeCacheMatrix function for quick retreival


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


## uses the MASS library to calculate the inverse of a matrix within
## the makeCacheMatrix function, once 
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
