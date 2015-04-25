#It is useful to store the inverse of matrix in a cache to avoid repeating the costly computation again and again 

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
	inv<- NULL													#inv store the inverse matrix
	set<-function(y) {											#Define the set function for the matrix
		x<<-y
		inv<<-NULL
	}
	get <- function() x											#Define the get function to retrieve the matrix
	setinv<-function (inverse) inv <<-inverse					#Computes and store the inverse matrix
	getinv<-function() inv										#Define the getinv function retrieving the inverse matrix
	list(set=set, get=get, setinv = setinv, getinv = getinv)	#Return the list with all defined functions
}


# The function below checks is the inverse of a matrix has been computed.
#If yes is retrieves the cache inverse matrix, if not, it computes the inverse and stores it in cache

cacheSolve <- function(x, ...) {
       inv<-x$getinv()
	   if(!is.null(inv)){
			message("getting cache data")
			return(inv)
	   }
	   data<-x$get()
	   inv<-solve(data,...)
	   x$setinv(inv)
	   inv
}
