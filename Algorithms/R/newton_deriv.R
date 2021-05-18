newton_deriv<-function(f,f1,x=0,tol=1e-10,iter = 1000){


  x1 <- x - (f(x)/f1(x))

  k <- 0
  while(abs(f(x1)-f(x))>tol ){

    k <- k+1


    x <- x1
    x1 <- x - (f(x)/f1(x))
    print(x1)
    if(k>iter| f(x) == 0){
      break
    }

  }

  print(x1)
  return(x1)
}
