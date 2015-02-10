## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        #Initiate inverse value in the environment of the
        #parent function makeCacheMatrix() to NULL
        inv <- NULL
        
        #Make child function set() to set values of a matrix
        set <- function(y) {
                # New value of matrix x is assigned from parent environment walking up
                # (e.g.The globel environment where makeCacheMatrix was defined)
                x <<- y 
        }
        
        #Make child function get() to get the values from matrix x
        get <- function() x
        
        #Make child function setinv() to store the inverse value of the matrix 
        #in cache. The inverse value is found in parent environments of setinv()
        setinv <- function(inverse) inv <<- inverse
        
        #Make child function of getinv() to obtain the inverse value from the cache
        getinv <- function() inv
        
        #Make a list of all above four functions
        list(set=set,get=get,setinv=setinv,getinv=getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #Obtain invers value by calling child function getinve() of x
        inv <- x$getinv()
        #If this value is not NULL,that means it has been cached
        if(!is.null(inv)){
                message("Getting cached data!")
                return(inv)
        }
        #Or obtain data from the matrix x by calling child funciton get() of x
        data <- x$get()
        #Test if this matrix is invertible (determinant is not 0)
        #If it is invertible, return its value and set it to the cache
        #Or return the message that the matrix is not invertible
        if (det(data)!=0){inv <- solve(data, ...) 
                          x$setinv(inv) 
                          return(inv)
        }
        else {
                return("The Matrix is not invertible!")
        }
        
}
