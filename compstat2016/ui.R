source("funcionInv/funInvUI.R")
source("funcionInv/funInv.R")
source("intMC/intMC.R")
source("intMC/intMCUI.R")
source("mh/mh.R")
source("mh/mhUI.R")

library(shiny)

shinyUI(
  fluidPage( theme="estilos.css",
    titlePanel("Estadística Computacional 2016"),
   # theme = "../estilos.css",
    # Sidebar
    sidebarLayout(sidebarPanel(
      radioButtons(
        "tarea",
        label = "Selecciona la opción que prefieras",
        choices = c(
          "Funcion Inversa" = "funInv",
          "Metropolis-Hastings" = "mh",
          "Integral de Monte Carlo" = "intMC"
        ),
        selected = "funInv"
      )
    ),
    
    
    #Panel principal
    mainPanel(
      conditionalPanel(
        condition = "input.tarea=='funInv'", funInvUI("funInv")),
      conditionalPanel(
        condition = "input.tarea=='intMC'", intMCUI("intMC")),
      conditionalPanel(
        condition = "input.tarea=='mh'", mhUI("mh"))
      )
    )
  ))
