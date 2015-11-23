## Put comments here that give an overall description of what your
## functions do

##Creating function 

makeCacheMatrix <- function(x = matrix()) {
 m <- NULL  # Initializing Matrix
 # Caching function
        set <- function(y) {
                x <<- y
                ## Swapping incase of Caching.
                m <<- NULL
        }
        
        get <- function() x
        #Solving Inverse of Matrix 
        setinverse <- function(solve) m <<- solve
        # getValue of Inverse Matrix
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


#Used to check if Cache has solved matrix and retrieve else claculate.
cacheSolve <- function(x, ...) {
        # Return a matrix that is the inverse of 'x'
           m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        #getValue of Matrix if no cache
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
