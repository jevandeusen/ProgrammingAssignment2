## cachematrix.R
## Coursera rprog-014
## ProgrammingAssignment2
## John Van Deusen
## makeCacheMatrix(...) takes a single square matrix x and returns a list of functions.
##
makeCacheMatrix <- function(x = matrix()) {
     cmX <- x
     cmI <- NULL
     
     ## getX() - returns the stored x matrix
     getX <- function() {
          cmX
     }
     
     ## setX(x) Sets the value of the stored matrix cmX and sets its inverse to NULL
     setX <- function(x, cmL) {
          cmX <<- x
          cmI <<- NULL
     }
     
     ## cacheSolve() returns the inverse of the makeCacheMatrix() instance. It will either return
     ##   a cached copy of the inverse or the solved inverse depending upon whether casheSolve() has
     ##   been previously invoked.
     cacheSolve <- function() {
          if (is.null(cmI)) {
               cmI <<- solve(cmX)
          }
          cmI
     }
     ## The list returned by the makeCacheMatrix(x) call.
     list(getX=getX, setX=setX, cacheSolve=cacheSolve)
}

## cacheSolve(x, ...) takes a square, invertible matrix x, and returns the inverse.
##   If cacheSolve(x) has been invoked previously with a matrix identical to x, the a cached copy
##   of the inverse of x is returned.
## A number of matrices can be passed to cacheSolve() and it will cache the inverses for all of them.
##   This is done by accessing a linked list of instances of the makeCacheMatrix(x) call.
##   A single variable holding the instance(s) of the makeCacheMatrix(x) call, cacheSolveList,
##   is stored in the global environment.
cacheSolve <- function(x, ...) {
     ## The size of the cacheSolveList is the maximum number of inverses that will be stored.
     ##   Additional matricies will cause the oldest to be overwritten.
     if (!exists("cacheSolveList") || is.null(cacheSolveList)) {
          cacheSolveList <<- vector("list", 5)
     }
     index <- 1
     while (!is.null(cacheSolveList[[index]])) {
          if (identical(x, cacheSolveList[[index]]$getX())) {
               return(cacheSolveList[[index]]$cacheSolve())
          }
          ## if index exceeds the length of cacheSolveList delete the oldest
          if (index >= length(cacheSolveList)) {
               cacheSolveList[[1]] <<- NULL
               break
          } else {
               index <- index + 1
          }
     }
     cacheSolveList[[index]] <<- makeCacheMatrix(x)
     cacheSolveList[[index]]$cacheSolve()    
}
## cacheSolveClear() sets the instance of the cacheSolveList variable in the global environment to NULL.
cacheSolveClear <- function() {
     if (exists("cacheSolveList")) { cacheSolveList <<- NULL }
}