##Caching the inverse of a matrix,
##using the <<- operator to assign a value to a different environment

## Creating a special "matrix", which is a list containing a function to do:
        ##set the values of the matrix
        ##get the values of the matrix
        ##set the value of the inverse of the matrix
        ##get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) { 
        i <- NULL 
        set <- function(y) { 
                x <<- y 
                i <<- NULL 
        } 
        get <- function() x 
        setinverse <- function(inv) i <<- inv 
        getinverse <- function() i 
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
} 

## Calculate the inverse of the special "matrix" created with the above 
## function, reusing cached result if it is available 

cacheSolve <- function(x, ...) { 
        i <- x$getinverse() 
        if(!is.null(i)) { 
                message("getting cached data") 
                return(i) 
        } 
        m <- x$get() 
        i <- solve(m, ...) 
        x$setinverse(i) 
        i 
} 

## Testing:
## testmatrix <- matrix(c(5, 0, 0, 5), c(2, 2))
## m <- makeCacheMatrix(testmatrix) 
## cacheSolve(m) 


