## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inv<-NULL
	set<-function(y){  #set the value of the vector
		x<<-y
		inv<<-NULL
	}
	get<-function() x    #get the value of the vector
	setInverse<-function(inverse) inv <<-inverse #set the value of the inverse
	getInverse<-function() inv #get the value of the inverse
	list(set=set,
		get=get,
		setInverse=setInverse,
		getInverse=getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
	inv<-x$getInverse()
	if(!is.null(inv)){ #check if the inverse value exist
			message("getting cached data")
			return(inv)
		}
		mat<-x$get()
		inv<-solve(mat, ...) #calculate the inverse matrix
		x$setInverse(inv)
		inv #return the inverse matrix
        ## Return a matrix that is the inverse of 'x'
}
