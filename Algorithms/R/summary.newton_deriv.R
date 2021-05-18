summary.newton_deriv <- function(a){
  print("function:")
  print(a[[1]])

  print("derivative")
  print(a[[2]])

  print("root:")
  print(newton(a[[1]],a[[2]],a[[3]]))
}
