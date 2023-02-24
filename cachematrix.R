#this create  a function that create a special Matrix
makeMatrix <- function(x = matrix()) {     
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x     
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



 #this calculate (in cache) the inverse matrix of the first function if  the inverse is already computed, otherwise it will compute the inverse matrix again

cacheinverse <- function(x, ...) {                           
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
