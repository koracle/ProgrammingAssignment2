## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

# the following function creates a special "matrix", which is really a list
# containing a function to
#
# 1.  set the value of the matrix
# 2.  get the value of the matrix
# 3.  set the value of the inverse matrix
# 4.  get the value of the inverse matrix

# "<<-" operator in functions "set*" save variables "x" and "inverse" in the
# global enviroment out of their own evaluation environments so they don't 
# disappear when the functions return.
#
# functions "get*" don't find variables "x" and "inverse" in their evaluation
# enviroment so they look for them in their parent environment, global enviroment,
# where those variables had been saved

makeCacheMatrix <- function(x = matrix()) {
    
    inverse <- NULL
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Write a short comment describing this function

# The following function returns the inverse of the special "matrix" created
# with the above function. It first checks to see if the inverse 
# matrix has already been calculated. If so, it `get`s the inverse matrix from 
# the cache and skips the computation. Otherwise, it calculates the inverse 
# matrix of the data and sets the value of the inverse matrix in the cache via 
# the `setinverse` function.
# some error handling with "tryCatch" added

cacheSolve <- function(x, ...) {
    
    ## error handling
    ehndl <<- function(e) {
        if (e$message[1] == "'a' must be a numeric matrix") {
            stop("not a numeric matrix")
        }
        if (e$message[1] == "Lapack routine dgesv: system is exactly singular: U[2,2] = 0") {
            stop("it's a singular matrix, does not have a matrix inverse")
        }
        if (length(grep("must be square", e$message[1]))>0) {
            stop("matrix must be square")
        }
        str(e)
    }
    
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- tryCatch(solve(data), error = ehndl)
    x$setinverse(inverse)
    inverse    
}

### how to use example
### ---------------------
### create a cache matrix
# >cm <- makeCacheMatrix()
### set its value
# > cm$set(rbind(c(1,1/4),c(1/4,1)))
### get matrix inverse
# >im <- cacheSolve(cm)
# > im
#       [,1]        [,2]
# [1,]  1.0666667 -0.2666667
# [2,]  -0.2666667  1.0666667
### try again to check cache trick 
# >im <- cacheSolve(cm)
# getting cached data
# >
### check we got an inverse matrix
# > cm$get() %*% im
#       [,1] [,2]
# [1,]    1    0
# [2,]    0    1
# > 
#
### some data for checking errors
### --------------------------------
### cm$set(rbind(c("1","2"),c("3","4"))) # no numeric matrix
### cm$set(rbind(c(1,1),c(1,1))) # singular matrix
### cm$set(c(1,2,3,4)) # no square matrix


