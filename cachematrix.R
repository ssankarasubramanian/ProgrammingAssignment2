## Matrix inversion-complex process in computation terms.
##Caching of inverse matrix improves computation time for repetetively used matrix

## makCacheMatrix function creates a matrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) { # x will be input matrix
  
  #inverse of matrix reset to NULL for every call to makeCacheMatrix()
  inverse_matrix<-NULL 
  
  set<-function(y){
    
    x<<-y #caches the input matrix
    
    inverse_matrix<<-NULL # inverse_matrix will hold the value of matrix inverse
  } 
  
  get<-function() x #Returns original matrix value
  
  
  # inverse of matrix is set by solve function to set_inverse
  #The above function is called by cacheSolve if the input matrix is entered first time
  set_inverse<-function(solve) inverse_matrix<<- solve 
  
  # The below function will return the cached value of inverse matrix
  # if the input matrix was entered before 
  get_inverse<-function() inverse_matrix 
  
  # This is a list of functions accessed for every new input matrix entered 
  # so as to create a new object. 
  list(set=set, get=get,
       set_inverse=set_inverse,
       get_inverse=get_inverse)
}

cacheSolve <- function(x=matrix(), ...) { # input x is object created by makeCacheMatrix
  
  m<-x$get_inverse() #accesses the cached value of inverse of object matrix 'x'
  
  if(!is.null(m)){ 
    # if inverse of input object matrix 'x' was already cached then this if block gets executed
    # and cached inverse of object matrix x is retrieved without any latency
    message("Retrieving the cached data")
    return(m)
  }
  matrix<-x$get() #The control reaches this section if there is no cached value for an input object matrix x
  m<-solve(matrix, ...) # Inverse of matrix is calculated by solve function and stored in m if its not cached before
  x$set_inverse(m) #Inverse of matrix is cached if not cached before
  m
}
                
