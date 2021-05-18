newton_optimization<-function(f,x=0,tol=1e-10,iter = 1000){

  f1 <- Deriv(f,'x')
  f2 <- Deriv(f1,'x')
  x1 <- x - (f1(x)/f2(x))

  k <- 0
  while(abs(x1-x)>tol ){

    k <- k+1


    x <- x1
    x1 <- x - (f1(x)/f2(x))

    if(k>iter| f(x) == 0){
      break
    }

  }


  return(x1)
}
