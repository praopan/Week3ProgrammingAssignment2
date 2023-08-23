## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix is building a matrix that can cache its inverse

## Write a short comment describing this function
library (MASS)
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL         #initializing inverse as NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function()x   #function to get matrix x
  setinv <- function(inverse)inv <<-inverse
  getinv <- function() {
    inver <- ginv(x)
    inver%*%x    #function to obtain inverse of the matrix
  }
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function
##cachesolve is computing the inverse of the above matrix. If the inverse has already been calculated then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) 
{
  inv<- x$getinv()
  if(!is.null(inv)){ 
    message("getting cached data!")
    return(inv)
    }        
  data <- x$get()
  inv <- solve(data,...)  #calculate inverse value
  x$setinv(inv)
  inv    ## return a matrix of inverse x
}

