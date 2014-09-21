#####################################
## Author: Tariq Alhindi          September 9/21/2014
##
## Creating two functions that get a matrix and calculate its inverse for the first
## time and then stores it (cache it) to avoid recalculations in future function calls
#####################################



# This function gets a matrix as an argument creates a "special" matrix 
# with needed getters and setters to be used later for caching

makeCacheMatrix <- function(x = matrix()) {
  
  #defining a variable to store the inverse
  inv <- NULL
  
  #changing the matrix and deleting the cached inverse, if any
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  #getting the value of the matrix
  get <- function() x
  
  #setting the value of the inverse to equal the passed argumant to the function
  setInverse <- function(inverse) inv <<- inverse
  
  #getting the current value of the inverse
  getInverse <- function() inv
  
  #returning a list of functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}




# This function get a "special" matrix as an argument and calcuate its inverse
# or gets the cached value of the inverse in case it has been calculated before

cacheSolve <- function(x, ...) {
  
  #getting the current value of the inverse
  inv <- x$getInverse()
  
  #checking if the current list have a value cached for the inverse
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  #storing the value of the matrix
  data <- x$get()
  
  #calculating the inverse
  inv <- solve(data, ...)
  
  #updating the list to store the newly calculated inverse in the cache 
  x$setInverse(inv)
  
  #returning the value of the inverse
  inv
}
