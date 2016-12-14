require(MASS)
require("goftest")


funInv <- function(input, output, session) {
  observeEvent(input$btn, {
    output$summary <- renderPrint({
      verifica()
    })
  })
  
  verifica <- reactive({
    fit1 <- fitdistr(finv(), "exponential")
    
    #Prueba de Kolmogorov-Smirnoff
    ks_value <- ks.test(finv(), "pexp", fit1$estimate)$p.value
    if (ks_value > 0.05)
      output$res1 <-
      renderText("Segun la prueba de Kolmogorov-Smirnoff, SI se acepta la simulacion")
    else
      output$res1 <-
      renderText("NO se acepta la simulacion, segun la prueba de Kolmogorov-Smirnoff")
    
    #Prueba de Anderson-Darling
    ad_value <- ad.test(finv(), "pexp", fit1$estimate)$p.value
    if (ad_value > 0.05)
      output$res2 <-
      renderText("Segun la prueba de Anderson-Darling, SI se acepta la simulacion")
    else
      output$res2 <-
      renderText("NO se acepta la simulacion, segun la prueba de Anderson-Darling")
  })
  
  
  finv <- reactive({
    res <- (-log(1 - runif(input$simulaciones))) / input$lambda
  })
  
  output$histograma <- renderPlot({
    hist(main = "Histograma de Simulaciones" , finv())
  })
  
  
}