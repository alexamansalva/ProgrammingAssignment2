## R Programming Course: https://class.coursera.org/rprog-010
## R Programming Assignment 2: Lexical Scoping
## https://github.com/alexamansalva/ProgrammingAssignment2.git

## makeCacheMatrix function creates a special "matrix", which is really a list containing a function to
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse
  )
}

## cacheSolve function return a inverse matrix using cached data
cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
      message("getting cached data")
      return(i)
    }
    data <- x$get()
    # Here it's the master function! :-) 
    i <- solve(data)
    x$setinverse(i)
    # return the inverse
    i
}
