## The purpose of the function below is to: 
# 1. Create a special "matrix" object that can cache its inverse. 
# 2. Create a function that computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL                                           # Initialize an inverse variable 'm' to NULL
    set <- function(y){                                 # 'y' in this case is the numeric arg passed into 'makeCacheMatrix'
        x <<- y                                         # Set 'x' for the function enviromnent to 'y'
        m <<- NULL                                      # Set 'm' for the 'makeCacheMatrix' environment to NULL
    get <- function() x                                 # Create a function 'get' in the 'makeCacheMatrix'
    setfunction <- function(inverse) m <<- solve(x) {   # Takes a value ('inverse') and sets it to the value of 'm' in the 'makeCacheMatrix' frame
    getfunction <- function() m                         # Returns the value of 'm' from the above frame
        cache1 <- x                                     # Create a variable 'cache1' in the function enviromnent to 'x'
        cache1 <<- y                                    # Set 'cache1' for the function enviromnent to 'y'
    }
    list(get = getfunction, set = setfunction, cache1 = cache1)    # Lists out the values of the functions in the makeCacheMatrix frame
}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    cache2 <- cache$get()                       ## Checks the'cache' environment and assigns the 'cache2' value from that environment to this one.
    message("Checking cache...")
    if(is.null(cache2)) {                       ## If the 'cache' environment has not been evaluated before, the function prints the message. 
        message("No previously cached value.")
    } else { 
        print(cache2)                           ## If the 'cache' environment has been evaluated before, the function prints the message and the value of the cache inverse. 
    }
    message("Setting new cached value...")
    cache$set(x)                                ## Assign the inverse matrix to 'x'
    print(cache$get())                          ## Return a matrix that is the inverse of 'x'
    message("Finished.")       
}
