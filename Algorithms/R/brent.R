brent <- function(f,a,b,tol = 1e-5){
  count = 0
  if(f(a)*f(b) >=0 ){

    stop("Numbers need to have different signs")
  }

  if (!is.function(f) | is.null(f)){
    stop("Argument 'f' must be a valid R function.")
  }

  if(abs(f(a))<abs(f(b))){
    ex <- a
    a <- b
    b <- ex
  }

  plot(f,a,b)
  #
  m <- FALSE

  c <- a
  s <- NULL
  d <- NULL
  while(f(b) !=0 &!(abs(b-a)<tol)){

    if(!is.null(s)){
      if(f(s)==0){
        break
      }
    }

    if((f(a)!=f(c))&(f(b)!=f(c))){

      s<- ((a*f(b)*f(c))/((f(a)-f(b))*(f(a)-f(c))))+ ((b*f(a)*f(c))/((f(b)-f(a))*(f(b)-f(c)))) + ((c*f(a)*f(b))/((f(c)-f(a))*(f(c)-f(b))))

    }
    else{
      s<- b-(f(b)*((b-a)/(f(b)-f(a))))
    }


    if(!is.null(d)){
      if((!between(s,((3*a)+b)/4,b))|(m&(abs(s-b)>=(abs(b-c)/2)))|(!m&(abs(s-b)>=(abs(c-d)/2)))){
        s <- (a+b)/2
        m <- TRUE
      }
      else{
        m <- FALSE
      }
    }
    else{

      if((!between(s,((3*a)+b)/4,b))|(m&(abs(s-b)>=(abs(b-c)/2)))){
        s <- (a+b)/2
        m <- TRUE
      }
      else{
        m <- FALSE
      }

    }
    d <- c
    c <- b

    if(f(a)*f(s)<0){
      b <- s
    }
    else{
      a <- s
    }

    if(abs(f(a))<abs(f(b))){
      x <- a
      a <- b
      b <- x
    }


  }



  return(list(b,s))

}

