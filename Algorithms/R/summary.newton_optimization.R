summary.newton_optimization<- function(a){

  print("function:")
  print(a[[1]])

  print("minimum:")
  print(newton_optimization(a[[1]],a[[2]]))
}
