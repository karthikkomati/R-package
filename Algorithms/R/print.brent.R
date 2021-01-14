print.brent <- function(a){
  if (length(a) == 3){
    print(brent(a[[1]],a[[2]],a[[3]]))
  }
  else{
    print(brent(a[[1]],a[[2]],a[[3]],a[[4]]))
  }

}
