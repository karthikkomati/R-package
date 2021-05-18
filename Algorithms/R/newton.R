newton <- function(f,x=0,tol=1e-10,iter = 100){


  f1 <- D(f,'x')

  x1 <- eval(f)/eval(f1)

  k <- 0
  while(abs(x1-x)>tol ){

    k <- k+1


    x <- x1
    x1 <- x - eval(f)/eval(f1)
    print(x1)
    if(k>iter| eval(f) == 0){
      break
    }

  }

  #print(x1)
  return(x1)

}
