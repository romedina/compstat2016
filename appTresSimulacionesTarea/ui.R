source("funcionInv/funInvUI.R")
source("funcionInv/funInv.R")
source("intMC/intMC.R")
source("intMC/intMCUI.R")
source("acepRech/ar.R")
source("acepRech/arUI.R")

library(shiny)

shinyUI(
  fluidPage(
    
    titlePanel("Estadistica Computacional 2016"),
   # theme = "../estilos.css",
    # Sidebar
    sidebarLayout(sidebarPanel(
      radioButtons(
        "tarea",
        label = "Selecciona la opción que prefieras",
        choices = c(
          "Funcion Inversa" = "funInv",
          "Aceptacion Rechazo" = "arRe",
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
        condition = "input.tarea=='arRe'", arReUI("arRe"))
      )
    )
  ))
