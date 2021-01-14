print.gss <- function(a){
  if (length(a) == 3){
    print(gss(a[[1]],a[[2]],a[[3]]))
  }
  else{
    print(gss(a[[1]],a[[2]],a[[3]],a[[4]]))
  }

}
