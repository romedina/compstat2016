# Funcion Inversa UI function
intMCUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Integración con Montecarlo"),
    numericInput(
      inputId = ns("desde"),
      label = "Desde:",
      value = "100"
    ),
    numericInput(
      inputId = ns("hasta"),
      label = "Hasta",
      value = "1000"
    ),
    numericInput(
      inputId = ns("salto"),
      label = "De, __  en __:",
      value = "100"
    ),
    plotOutput(ns("histograma"))
    
  )
}
