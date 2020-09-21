## Put comments here that give an overall description of what your
## function do

## There are two functions: makecachematrix, cachesolve
## makecachematrix consists of inv, set, get, setinv, getinv
makecachematrix<-function(x=matrix()){
  inv<-NULL ## initializing inverse as NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function(){x} ## function to get matrix x
  setinv<-function(inverse){inv<<-inverse}
  getinv<-function(){inv} ## function to get an inverse of the matrix
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}

## Write a short comment describing this function
## This is used to get cache data

cachesolve<-function(x,...) ## get cache data
  {
  inv<-x$getinv()
  if(!is.null(inv)){ ## checking whether inverse is NULL
    message("getting cached data")
    return(inv) ## returns inverse value
  }
  data<-x$get()
  inv<-solve(data,...) ## calculates inverse value
  x$setinv(inv)
  inv ## returns a matrix that is the inverse of 'x'
}