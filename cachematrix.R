## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function will create a matrix object and cache its inverse value

makeCacheMatrix <- function(x = matrix()) {
  
        matInv <- NULL              ## initializing the matrix as NULL
        set <- function(y) {        ## Setting the matrix value
                x <<- y             ## 'superassignment' defining the value of x in Global Environment
                matInv <<- NULL     ## reseting the matrix value if there is a new matrix
        }
        
        ## defining the get function and returning the value of the matrix argument
        get <- function() x 
        
        ## defining the value of matInv in Global Environment
        setInv <- function(matrix) matInv <<- matrix  
        
        ## getting the value of matInv where called
        getInv <- function() matInv
        
        ## definning field names to use in function  
        list(set = set, get = get,setInv = setInv,getInv = getInv)
        
        
}


## Write a short comment describing this function
## This function will return the value of the inverse matrix cached in 
## makeCacheMatrix function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

        matInv <- x$getInv()
        if(!is.null(matInv)) {
                message("getting cached data")
                return(matInv)
        }
        data <- x$get()
        matInv <- solve(data, ...)
        x$setInv(matInv)
        matInv
 
        
## Testing the function:
## source("cachematrix.R")
## a<-makeCacheMatrix()
## a$set(matrix(c(1,1,2,0),2,2))
## cachesolve(a)
               
        
}
