## makeCacheMatrix and cacheSolve functins are defined to operate on 
## matrices to calculate and cache inverse matrices. The inverse operation is an computationally expensive 
## one, so we need to cache the inverse matrix once it has been calculated
## for cost-effective reusability.


## makeCacheMatrix creates a matrix with some special functionality.
## You can set the matrix by passing a matrix as an argument to the set 
## fuction. Similarly, setinv function sets and caches the inverse matrix.
## The get/getinv functions return the matrix and it's inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv<-NULL
    set <- function(y){
      x<<-y
      inv<<-NULL
    }
    get <- function() x
    setinv <- function(inverse) inv<<-inverse 
    getinv <- function() inv
    list (set = set, get = get, 
          setinv = setinv,
          getinv = getinv)
}


## cacheSolve function simply gets the inverse of the matrix if it has 
## already been cached. Otherwise, it calculates the inverse (assuming the 
## matrix is invertible) and sets it in the matrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
      inv <- x$getinv()
      if (!is.null(inv)){
        message("getting cached data")
        return(inv)
      }
      data <- x$get()
      inv <- solve(data)
      x$setinv(inv)
      inv
}
