## These two functions are written to make a special version of matrix and cache##the inverse of a Matrix to avoid repeatedly computing the inverse.

## The makeCacheMatrix function is designed to create a special "matrix" using 
## the initial matrix value of x, which is really a list containing a function to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse
## 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
       
	m<-NULL
        ##set m to store the inverse, initial it to NULL
	set<-function(y){
		x<<-y
		m<<-NULL
                ##if the value of set is updated, set m to be NULL again because                ##the inverse would be calculated again.
	}
	get<-function() x
	setinv<-function(inv) m<<-inv
	getinv<-function() m
	list(set=set,get=get,setinv=setinv,getinv=getinv)
}


##    The cacheSolve function returns a matrix that is the inverse of "x"

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x', be careful that here x is        ## the special matrix defined by function makeCacheMatrix
	m<-x$getinv()
        ## m is used to store the inverse
	if(!is.null(m)){
		message("getting cached data")
		return(m)
                ## If the cache value of inverse of "x" is not NULL, which mean                  s the same "matrix"'s inverse has been calculated before, henc                  e return it  
	}
	data<-x$get()
	m<-solve(x$get()) ## recalculate the inverse
	x$setinv(m)       ## store the inverse into cache
	m
}

