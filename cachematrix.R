#The first function, makeVector creates a special "vector", which is really a list containing a function to
#set the value of the vector
#set the value of the vector
#set the value of the mean
#get the value of the mean
#makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {

  inverse <- NULL         # valor de la matriz inversa
  
  set <- function(y) {    # establecer valor de la matriz
    x <<- y
    inverse <<- NULL
  }                        
  
  get <- function() x       # obtener valor de la matriz
  
  setInverse <- function(inverseInput) inverse <<- inverseInput   # establecer el valor de la inversa

  getInverse <- function() inverse    # obtener el valor de la inversa

  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)    # devolver la lista de funciones


}

#cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
#retrieve the inverse from the cache.



cacheSolve <- function(x, ...) {
  
  inverse <- x$getInverse()     # revisar si ya entrega la inversa la funcion anterior
  
  if(!is.null(inverse)){        # obtener la inversa

    return(inverse)
  }
  
  data <- x$get()          #obtener la matriz
  
  inverse <- solve(data)   #calcular la inversa con la funcion "solve()"
  
  x$setInverse(inverse)    # obtener la inversa
  
  inverse                 # entregar el resultado
  
}
