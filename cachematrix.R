
## Sorry for broken English. First of all, i've found a discussion topic
## "Simple test matrices for the lexical scoping programming assignment"
## and tested it with those matrices. It works like it should.

## First function takes a matrix as an argument x and creates an object with
## it's own environment, and that environment with it's own functions: 
## set, get, setinv, getinv.
## More about these function in a cacheSolve comment.

makeCacheMatrix <- function(x = matrix()) {

  invmtrx <- NULL
  set <- function(y) {
    x <<- y
    invmtrx <<- NULL
  }
  get <- function() x
  setinv <- function(inv) invmtrx <<- inv
  getinv <- function() invmtrx
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## If you want to use cacheSolve, first of all you need to make a matrix using
## this function because cacheSolve calls functions in makeCacheMatrix to:
## 1. return inverse matrix from makeCacheMatrix with function getinv.
## 2. if that inverse matrix doesn't exist yet in cache, then it uses get
## function to  retrieve it's original value from makeCacheMatrix and solves it.
## 3. when it solves it, it sets new inverse value with function setinv and also
## returns it.

cacheSolve <- function(x, ...) {
       
  invmtrx <- x$getinv()
  if(!is.null(invmtrx)) {
    message("getting cached data")
    return(invmtrx)
  }
  data <- x$get()
  invmtrx <- solve(data, ...)
  x$setinv(invmtrx)
  invmtrx


}
