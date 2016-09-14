#===================================================================
# This script demonstrates a method of storing objects in cache memory
# and retrieving them at will. This is possible because of the lexical 
# scoping properties utilized in the [R] scripting language. The key is
# that when a function is created a new environment is created with it
# containing all objects defined within the function. Now, because
# lexical scoping allows an object to retain information of its parent
# environment, the execution environment information will remain in
# memory. This means we can store the results of costly computations in
# memory for quick access instead of re-computing them every time we
# need them.
#========================================================================
#     Auther: Brandon M. Thacker
#       Date: 9.14.2016
#========================================================================



# makeCacheMatrix() stores and produces the values x = matrix() and
# and the inverse of x "inv" using seting and getting functions.
# Once exicuted given x the result is a list of these functions which
# can be called in other functions.
makeCacheMatrix <- function(x=matrix()){ # init
            # initialize inverse object
            inv <- NULL
            
            # define function to set x externally.
            # "<<-" looks to assign y to x outside set.M
            set.M <- function(y=matrix()){
                  x <<- y
                  inv <<- NULL
            }
            # retrieve x
            get.M <- function() {
                  x
            }
            # set the inverse of x
            set.Inv <- function(inverse){
                  inv <<- inverse
            }
            # retrieve inv
            get.Inv <- function() {
                  inv
            }
            # retern a list of function objects
            list(set.M = set.M, get.M = get.M,
                 set.Inv = set.Inv,
                 get.Inv = get.Inv)

}


# cacheSolve() takes the list output from makeCacheMatrix()
# and feeds the functions into the cacheSolve() environment.
# the existance of inv is checked within makeCacheMatrix().
# inv is returned if it does; else, the inverse of x is 
# calculated first and set in makeCacheMatrix() for later.
cacheSolve <- function(x,...) {
      
      # retrieve the value of inv from makeCacheMatrix()
      inv <- x$get.Inv()
      
      # Check to see if the inverse already exists
      if(!is.null(inv)) {
            # if so
            message("getting cached data")
            return(inv)
      }
      # otherwise calculate it from the matrix      
      mat <- x$get.M()
      inv <- solve(mat)
      
      # store it in makeCacheMatrix() and print inv
      x$set.Inv(inv)
      inv

}
#========================================================================
# Here is a run through example!!
#========================================================================

#> source("cachematrix.R")

#> M <- matrix(c(1,3,3,1),2,2)      - create a square matrix M 
#> M                                
#[,1] [,2]                                                 
#[1,]    1    3                     
#[2,]    3    1                     
#
#> A <- makeCacheMatrix(M)          - initialize cache function
#                                     with M 
#
#> cacheSolve(A)                    - calcuate and return the
#                                     inverse of A 
#[,1]   [,2]
#[1,] -0.125  0.375
#[2,]  0.375 -0.125
#
#> cacheSolve(A)                    - retrieve stored inverse                                     A data
#getting cached data                   A from cache function
#[,1]   [,2]
#[1,] -0.125  0.375
#[2,]  0.375 -0.125
#
#> In.M <- A$get.Inv()              - check that matrix product
#> In.M%*%M                           of A and inv(A) gives back
#[,1] [,2]                            the identity matrix
#[1,]    1    0
#[2,]    0    1                     - nice!