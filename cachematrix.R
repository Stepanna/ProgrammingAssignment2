## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#This function set the matrix, get it, set the value of the inversed matrix and get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
        
        #regulary the value of inversed matrix is undefined, 
        #because we didn't get it
        invMatrix <- NULL
        
        #store a matrix, inverse is unknown
        set <- function(y){
                x <<- y
                invMatrix <<- NULL
        }
        
        #returns the matrix
        get <- function() x
        
        #cache the inversed matrix
        setInv <- function(x) invMatrix <<- solve(x)
        
        #returnes the cached value
        getInv <- function() invMatrix
        
        #return a list, each element is a function
        list (set = set, get = get, setIncv = setIncv, getInv = getInv)
}


## Write a short comment describing this function

#Calculates a value of inversed matrix, created in function makeCacheMatrix
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        #get cached value
        invMatrix <- x$getInv()
        
        #if value exists return it
        if(!is.null(invMatrix)) {
                message("getting cached data")
                return(invMatrix)
        }
        
        #if it is not get the matrix, calculate 
        #inversed matrix and store it in cache
        data <- x$get()
        invMatrix <- solve(data)
        x$setInv(x)
        
        #return the inverse
        invMatrix
}
