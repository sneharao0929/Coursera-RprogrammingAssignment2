## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly. 
## the purpose of this code is write  pair of functions that cache the inverse of a matrix.

## 

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # defines a function to set the vector, x, to a new vector, y, and resets the inverse i to NULL 
        # in parent enviroiment
        i <-NULL
        set<-function(y){
                x<<-y
                i<<-NULL
        }
        
        #x returns the vector, x
        get<-function() x
                
        #sets the inverse, i,    
        setinverse <-function(solve) i<<- solve
        
        #returns the mean, m
        getinverse<-function() i
        
        
        #returns the 'special vector' containing all of the functions just defined
        
        list(set=set, get=get,
             setinverse=setinverse,
             getinverse=getinverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.



cacheSolve <- function(x, ...) {
        # Try to get the existing cache inversed matrix
        
        i<-x$getinverse()
        
        # if cache inversed matrix , display the message and return the object 
        if(!is.null(i) ){
                message("getting cached data")
                return(i)
        }
        
        # since the cache inversed matrix does the exist, create a inverse of 
        # data provided at the run time.
        inverse<-x$get()
        
        # creates the inverse of atrix
        i<-solve(inverse, ...)
        
        ## cache it
        x$setinverse(i)
        
        ## Return a matrix that is the inverse of 'x'
        
        
}


##### run the script
c<-makeCacheMatrix()
c$set(matrix(c(1,2,3,4) ,nrow=2,ncol=2))
cacheSolve(c) 

# getting cached data
# [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5








