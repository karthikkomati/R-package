summary.gss <- function(a){

  print("function:")
  print(a[[1]])

  print("bounds:")
  print(a[[2]])
  print(a[[3]])

  print("minimum between:")
  print(gss(a[[1]],a[[2]],a[[3]])[[1]])
  print(gss(a[[1]],a[[2]],a[[3]])[[2]])

  print("minimum:")
  print(gss(a[[1]],a[[2]],a[[3]])[[3]])
}
