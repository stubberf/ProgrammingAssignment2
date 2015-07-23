## The two functions when used in combination will compute the 
## the inverse of a matrix and cache the results. Therefore if
## the inverse of the same matrix is needed the cached results
## are returned. 


## This function creates a list of functions that
## 1) stores a matrix
## 2) return the stored matrix
## 3) store the inverse of a matrix and 
## 4) return the stored matrix inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
            x <<- y
            inv <<- NULL
    }
    get <- function(){x}
    setinv <- function(inverse){inv <<- inverse}
    getinv <- function(){inv}
    list(set = set, get = get, 
         setinv = setinv, 
         getinv = getinv)
}


## This function takes the output of the function "makeCacheMatrix" 
## as inpute and computes the inverse of a matrix and using the 
## stores the results as a cached value. This value is returned if
## if the inverse of the same matrix is needed. 

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    inv <- x$getinv()
    if(!is.null(inv)){
      message("getting cached data")  
      return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    return(inv)
}
