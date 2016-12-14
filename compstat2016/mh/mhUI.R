mhUI <- function(id)
{
  ns <- NS(id)
  tagList(
    h1('Metropolis Hasting MCMC'),
    fileInput(ns('file1'), label = 'Seleccionar archivo csv', accept = c('text/csv','text/comma-separated-values')),
    tabsetPanel(
      tabPanel("Datos", tagList(
        fluidRow(
          column(6,uiOutput(ns('vdui'))),
          column(6,uiOutput(ns('viui')))
        ),
        tableOutput(ns('data'))
      )),
      tabPanel("Datos seleccionados", tagList(
        plotOutput(ns('varpt'))
      )),
      tabPanel("Aprioris", tagList(
        br(),
        p('Regresion y = a + X*b + eps '),
        plotOutput(ns('apra')),
        plotOutput(ns('aprb')),
        plotOutput(ns('aprg'))
      )),
      tabPanel('Simulacion', tagList(
        numericInput(ns('cadenas'), label = 'Longutud de cadena', value = 1000),
        br(),
        p('Resultados simulacion'),
        dataTableOutput(ns('sim')),
        textOutput(ns('test'))
      )),
      tabPanel('Graficas simulaciones', tagList(
        br(),
        plotOutput(ns('histos'))
      ))
    )
  )
}