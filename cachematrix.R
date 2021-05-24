## Put comments here that give an overall description of what your
## functions do

## makeCachMatrix take a matrix as argument and retetn a list contain of four function:
## 01-set()
## 02-get()
## 03-setinverse()
## 04-getinverse()
makeCacheMatrix <- function(x = matrix()) {
    i = NULL
    set <- function(y){
        x <<- y
        i <<- NULL
    }
    get <- function(){
        return(x)
    }
    setinverse <- function(inverse){
        i <<- inverse
    }
    getinverse <- function(){
        return(i)
    }
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function
## cachesolve take a matrix as argument and return it's inverse 
cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if (!is.null(i)){
            message("getting cached data")
            return(i)
        }
        matrix <- x$get()
        i <- solve(matrix)
        x$setinverse(i)
        return(i)
}

