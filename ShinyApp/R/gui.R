#' shiny app
#' This app allows you to visualize the functions implemented in the algorithms package.
#' @export

library(shiny)

ui <- fluidPage(
  tabsetPanel(type = "tabs",tabPanel("Gss/Brent",
  selectInput("method", label = "method", choices = list('gss','brent')),
  fluidRow(column(width = 2,
  numericInput("x5", label = "x^5",value = 0, width = "60px")),
  column(width = 2,
  numericInput("x4", label = "x^4",value = 0, width = "60px")),
  column(width = 2,
  numericInput("x3", label = "x^3",value = 0, width = "60px")),
  column(width = 2,
  numericInput("x2", label = "x^2",value = 0, width = "60px")),
  column(width = 2,
  numericInput("x1", label = "x^1",value = 0, width = "60px")),
  column(width = 2,
  numericInput("c", label = "c",value = 0, width = "60px")),
  column(width = 2,
         numericInput("low", label = "lowerbound",value = 0, width = "60px")),
  column(width = 2,
         numericInput("up", label = "upperbound",value = 0, width = "60px")),
  ),
  plotOutput("graph"),
  verbatimTextOutput("points"),
  ),
  tabPanel("newton",
           selectInput("met", label = "method", choices = list('newton','newton-optimization')),
           selectInput("func", label = "function", choices = list('x^4-3x^2+x-2','-x^4+3x^2-x+2')),
           plotOutput("graph2"),
           verbatimTextOutput("points2"),

           )

  ),




)

server <- function(input, output, session) {


  output$graph <- renderPlot(

    curve(input$x5*x^5+input$x4*x^4+input$x3*x^3+input$x2*x^2+input$x1*x+input$c, from=input$low, to=input$up,  xlab="x", ylab="y"),

  )

  output$points <- renderText(

    if(input$method == 'gss'){
      get_gss(input$x5,input$x4,input$x3,input$x2,input$x1,input$c,input$low,input$up)
    }else{
      get_brent(input$x5,input$x4,input$x3,input$x2,input$x1,input$c,input$low,input$up)
    }

  )

  output$graph2 <- renderPlot(

    if(input$func == 'x^4-3x^2+x-2'){
      curve(x^4-3*x^2+x-2, from = -5, to = 5,ylim = c(-6,6), n = 100)
    }
    else if(input$func == '-x^4+3x^2-x+2'){
      curve(-x^4+3*x^2-x+2, from = -5, to = 5,ylim = c(-6,6), n = 100)
    }


  )

  output$points2 <- renderText(

    if(input$met == 'newton' & input$func == 'x^4-3x^2+x-2'){
      paste("root:",newton(expression(x^4-3*x^2+x-2)))
    }
    else if(input$met == 'newton'& input$func == '-x^4+3x^2-x+2'){
      paste("root:",newton(expression(-x^4+3*x^2-x+2)))
    }
    else if(input$met == 'newton-optimization'& input$func == 'x^4-3x^2+x-2'){
      newton_optimization(function(x){x^4-3*x^2+x-2})
    }
    else if(input$met == 'newton-optimization'& input$func == '-x^4+3x^2-x+2'){
      newton_optimization(function(x){-x^4+3*x^2-x+2})
    }

  )

}

get_gss <- function(x5,x4,x3,x2,x1,c,low,up){
  s <- "~x5 * x^5 + x4 * x^4 + x3 * x^3 + x2 * x^2 + x1 * x + c"
  s <- gsub("x5",x5,s)
  s <- gsub("x4",x4,s)
  s <- gsub("x3",x3,s)
  s <- gsub("x2",x2,s)
  s <- gsub("x1",x1,s)
  s <- gsub("c",c,s)
  form <- as.formula(s)

  allvars <- all.vars(form)
  DD <- deriv(expr = form, namevec = allvars)
  aDD <- as.character(DD)
  nvars<-length(allvars)
  func<-function(){
  }
  functext<-paste0("func<-function(",allvars[1])
  if(nvars==2){
    functext<-paste0(functext,",",allvars[2])}
  if(nvars>2){
    for(i in 2:nvars){
      functext<-paste0(functext,",",allvars[i])}}
  functext<-paste0(functext,"){ ")
  functext <- paste0(functext, substr(x = aDD, start = 2, stop = nchar(aDD)))

  eval(parse(text = functext))
  g <- gss(func,low,up)

  st <- "lowerbound:"
  st<- paste(st,g[[1]])
  st<- paste(st," upperbound:")
  st<- paste(st,g[[2]])
  st<- paste(st," value:")
  st<- paste(st,g[[3]])
  return(st)
}


get_brent <- function(x5,x4,x3,x2,x1,c,low,up){
  s <- "~x5 * x^5 + x4 * x^4 + x3 * x^3 + x2 * x^2 + x1 * x + c"
  s <- gsub("x5",x5,s)
  s <- gsub("x4",x4,s)
  s <- gsub("x3",x3,s)
  s <- gsub("x2",x2,s)
  s <- gsub("x1",x1,s)
  s <- gsub("c",c,s)
  form <- as.formula(s)

  allvars <- all.vars(form)
  DD <- deriv(expr = form, namevec = allvars)
  aDD <- as.character(DD)
  nvars<-length(allvars)
  func<-function(){
  }
  functext<-paste0("func<-function(",allvars[1])
  if(nvars==2){
    functext<-paste0(functext,",",allvars[2])}
  if(nvars>2){
    for(i in 2:nvars){
      functext<-paste0(functext,",",allvars[i])}}
  functext<-paste0(functext,"){ ")
  functext <- paste0(functext, substr(x = aDD, start = 2, stop = nchar(aDD)))

  eval(parse(text = functext))
  g <- brent(func,low,up)

  st <- "lowerbound:"
  st<- paste(st,g[[1]])
  st<- paste(st," upperbound:")
  st<- paste(st,g[[2]])

  return(st)
}



shinyApp(ui, server)
