## Basically, the makeCacheMatrix function creates a matrix and a list of functions and sets the values for x and m in the global environment
## The cacheSolve function uses the functions created in the aforementioned function. This function checks if the inverse matrix has already been created
## If the matrix has not yet been created, this function creates it and returns the matrix. If the matrix is already created, it shows both a message and the matrix.


## First, the value for m is made empty in the global environment
## Secondly, a function "get" is created, which gets the value "x" (note: this value has just been created by the "set" function)
## Thirdly, a function is created for "setinv" which inverses the value "m". this value "m" is set in the cacheSolve function through the "get" function. 
## Therefore, "m" is basically similar to the value x. Eventually, this function (setinv) assigns the inversed matrix to "m" in the global environment
## Subsequently, a function is created for getting the value m, which we just saw is the inversed matrix of x. 

makeCacheMatrix <- function(x = matrix()) {
        m <<- NULL
        get <- function() x
        setinv <- function(solve) m <<- solve 
        getinv <- function() m
        list(get = get,
             setinv = setinv,
             getinv = getinv)
}

## First, the inverse of the matrix is loaded to "m"
## Secondly, R checks if "m" is NULL (empty) or not
## If inv is not empty, the message "getting cached data" is displayed 
## And inverse matrix is returned

## If inv is empty, the matrix is loaded into "data" by using the "get" function
## The inverse of the matrix is set to "m"
## The inverse is set in the global environment using the setinv function
## The inverse of the matrix is returned

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        return(m)
}