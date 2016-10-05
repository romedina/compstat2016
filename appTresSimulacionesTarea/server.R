

library(shiny)


function(input, output, session) {
  
  callModule(funInv, "funInv")
  callModule(intMC, "intMC")
  callModule(arRe, "arRe")
  
}
