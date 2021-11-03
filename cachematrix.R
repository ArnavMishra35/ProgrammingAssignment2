## The solution to this asssignment consist of two functions- makeCacheMatrix and cacheSolve. The first function creates the matix which stores the value
## of the inverse and from which it is retrieved. The second function solves the inverse of the given matrix and caches it, or retrieves it if it's already cached.

## makeCacheMatrix works as follows- it builds a set of functions and returns the functions within a list to the parent environment. This allows for caching and 
## retreiving of data and functions.

 makeCachematrix <- function(x=matrix()){
   inv <- NULL
   set <- function(y){
     x <<- y
     inv <<- NULL}
   
   get <- function() {x}
   
   setInverse <- function(inverse){inv <<- inverse}
   
   getInverse <- function() {inv}
   
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
 }

}


## The cacheSolve function first checks if the inverse of the matrix has been calculated earlier or not, it a cached value is available, it is retrived directly. 
## Otherwise, the inverse is calculated and cached. 

cacheSolve <- function(x, ...){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}

## Now, calculating an example 
examplematrix <- makeCachematrix(matrix(1:4, nrow = 2, ncol = 2))
cacheSolve(examplematrix)

