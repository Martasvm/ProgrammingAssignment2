makeCacheMatrix <- function(x = numerical(),...) { #in this case, input will be a matrix, so we have to use option matrix
        #The matrix m, which is the inverse of x is set to null everytime we pass through the function.
        m <- NULL              
        #Now we include the set function.
        #The purpose of this function is to let you assign a new value to the object, if that's necessary to save memory.  
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        # The next three functions are defined when we run makeVector, but they won't be used until we call the cacheSolve function:
        
        get <- function() x
        #with this function we can get the data in x, but it won't appear when makeCacheMatrix is called
        #but in case the cacheSolve function needs to access it.
        setinverse <- function(storeinverse) m <<- storeinverse
        #It is also called the first time CacheSolve is called, to store the inverse for next accesses
        getinverse <- function() m
        #This function returns the value of the inverse. In the first access it will return NULL, and in the subsequent accesses it will return the value already computed.
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
        m <- x$getinverse() #This function access the inverse matrix to check if it had already been computed and stored
        #If it was computed before, the CacheSolve function has previously stored a value in getinverse, so the value will be taken from there.
        #Otherwise, this function will return NULL.
        if(!is.null(m)) {  #in case it was already computed before, m will be different from NULL and the condition will apply.
                message("getting cached data") #a message is displayed to indicate that we are using the already computed inverse matrix
                return(m) #and the inverse matrix will be displayed. At this point the function would stop.
        }
        # But, if m is NULL, the commands inside the if does not apply, and the function go on executing following commands:
        data <- x$get()  #it takes the matrix we want to invert accessing the get function in the MakeCacheMatrix
        m <- solve(data, ...)   #and compute the inverse of that matrix
        x$setinverse(m)   #Finally, it sets the obtained value inside the MakeCacheMatrix function
        #so it can be accessed directly next time we use it, avoiding all the previous calculations.
        m   #Finally, it prints the value of theinverse matrix of X
}
