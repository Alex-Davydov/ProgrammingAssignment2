makeCacheMatrix<-function(x=matrix()){
  inv<-NULL
  set<-function(y){
    x<<-y
    inv<<-NULL
  }
  get<-function() x
  setInverse<-function(inverse) inv<<-inverse
  getInverse<-function() inv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}

cacheSolve<-function(x,...){
  inv<-x$getInverse()
  if (!is.null(inv)){
    message("Retreiving matrix from cache")
    return(inv)
  }else{
    inv<-solve(x$get(),...)
    x$setInverse(inv)
    return(inv) 
  }
}