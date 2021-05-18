newton_optimization_hd<- function(f,var,values = NULL,tol=0,iter=100){
  if(is.null(values)){
    l<-list()
    for(p in 1:length(var)){

      l[p]<-1
    }

    values <- l
  }
  val <- unlist(values)

  firstDerivs <- Deriv(f)
  hess <- NULL

  for(v in var){
    secondDerivs <- Deriv(firstDerivs,v)
    hess <- rbind(hess,do.call(secondDerivs,values))
  }

  hessInverse <- solve(hess)
  gradient <- do.call(firstDerivs,values)
  x <- val - (hessInverse%*%gradient)
  #print("starting loop")
  k <- 0
  while(k < iter){
    hess <- NULL
    k <- k+1
    val = x

    values <- as.list(x)
    #print(k)
    #print(x)
    for(v in var){
      secondDerivs <- Deriv(firstDerivs,v)
      hess <- rbind(hess,do.call(secondDerivs,values))
    }


    hessInverse <- solve(hess)
    gradient <- do.call(firstDerivs,values)
    x <- val - (hessInverse%*%gradient)

    if(dist(t(cbind(x,val))) < tol){
      break
    }

  }

  print(x)
  return(x)

}
