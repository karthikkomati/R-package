summaru.newton_optimization_hd <- function(a){

  print("function:")
  print(a[[1]])

  print("minimum:")
  print(newton(a[[1]],a[[2]]))
}
