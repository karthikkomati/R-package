summary.brent <- function(a){

  print("function:")
  print(a[[1]])

  print("bounds:")
  print(a[[2]])
  print(a[[3]])

  print("root between:")
  print(brent(a[[1]],a[[2]],a[[3]])[[1]])
  print(brent(a[[1]],a[[2]],a[[3]])[[2]])

}
