gss <- function(f, a, b, tol = 1e-5){

  #checks if the function is valid
  if (!is.function(f) | is.null(f)){
    stop("Argument 'f' must be a valid R function.")
  }

  #sets the value of phi to equal the golden ratio
  phi <- (1+sqrt(5))/2

  a <- min(a, b)
  b <- max(a, b)

  plot(f,a,b)

  dis <- b - a

  if(dis<=tol){
    return(list(a,b))
  }

  #calculates the total number of iterations required
  n <- ceiling(log(tol / dis) / log(1/phi))

  c <- b - ((1/(phi)) * dis)
  d <- a + ((1/phi) * dis)
  fc <- f(c)
  fd <- f(d)

  for(k in 0:n-1){
    if(fc<fd){

      b <- d
      d <- c
      fd <- fc
      dis <- (1/phi) * dis

      c <- b - (1/(phi)) * dis
      fc <- f(c)
    }
    else{

      a <- c
      c <- d
      fc <- fd
      dis <- (1/phi) * dis
      d <- a + ((1/phi) * dis)
      fd <- f(d)
    }
  }

  if(fc<fd){

    return(list(a,d,f(a)))
  }
  else{

    return(list(c,b,f(c)))
  }

}

