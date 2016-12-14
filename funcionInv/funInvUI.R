

# Funcion Inversa UI function
funInvUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3(class="titulo","Método de la Funcion Inversa"),
    
    numericInput(
      inputId = ns("lambda"),
      label = "Valor de Lambda",
      value = "3.0"
    ),
    numericInput(
      inputId = ns("simulaciones"),
      label = "# de Simulaciones",
      value = "1000"
    ),
    actionButton(ns("btn"), "Verificar"),
    p(textOutput(ns("res1"))),
    p(textOutput(ns("res2"))),
    htmlOutput(ns("summary")),
    plotOutput(ns("histograma"))
  )
}
