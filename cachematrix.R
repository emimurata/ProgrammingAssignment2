## makeCacheMatrix is a function that saves a list with 4 other functions in 
##  order to cache a matrix and its inverse, if it has been computed.

makeCacheMatrix<-function(x=matrix()){
  inverse<-NULL
  set_matrix <- function(y) { #this two functions overwrite the matrix stored and reset the value of the inverse to null
    x <<- y
    inverse <<- NULL #resets the inverse to null
  }
  get_matrix<- function() x #this two functions are for storing the matrix
  set_inverse<-function(solve) inverse<<-solve #this two functions are for overwring the inversed matrix being stored if the original matrix change 
  get_inverse<-function()inverse
  list(set_matrix=set_matrix,
       get_matrix=get_matrix,
       set_inverse=set_inverse,
       get_inverse=get_inverse)
}

## cacheInverse is a function that checks if the inverse of the matrix has been stored and prints it or computes it if needed

cacheSolve <- function(x) {
  inverse <- x$get_inverse() #retrives the inversed matrix stored
  if(!is.null(inverse)) { #checks if the matrix is there 
    message("getting cached data")
    return(inverse) #if its there, it is returned here
  }
  matrix <- x$get_matrix() #if it is not, the new matrix is set
  inverse <- solve(matrix) #the new inversed matrix is calculated
  x$set_inverse(inverse) #then stored
  inverse #and printed
}