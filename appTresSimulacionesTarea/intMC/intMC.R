require(MASS)
require("goftest")
require(plyr) # To handle data efficiently.


intMC <- function(input, output, session) {
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
  
  # ==============  ==============  ==============  ==============  ==============  =========
  # ==============  ==============  ==============  ==============  ==============  =========
  # ==============  ==============  ==============  ==============  ==============  =========
  
  # Integracion con MC ============
  
  mc.intervals <- function(Phi, N, X.dens=runif, alpha=0.05){
    
    
    results.list <- lapply(N, function(nsim){
      
      # MonteCarlo step
      
      X <- sapply(FUN=X.dens, nsim) # N samples of the density of X
      
      PhiX <- sapply(X, Phi) # Evaluate phi at each X_i
      
      estim <- mean(PhiX) # Estimate of int_a^b \phi(x)f(x)df=E[phi(X_i)]
      
      S2 <- var(PhiX) # Estimate of the variance of phi(X_i)
      
      quant <- qnorm(alpha/2, lower.tail=FALSE) # Right quantile for alpha/2
      
      int.upper <- estim + sqrt(S2/nsim)*quant # Upper confidence interval
      
      int.lower <- estim - sqrt(S2/nsim)*quant # Lower confidence interval
      
      return(data.frame(N=nsim, Estimate=estim, LI=int.lower, UI=int.upper))
      
      # -------
      
    })
    
    #
    
    results.table <- ldply(results.list) # Assembles list in data.frame
    
    return(results.table)
    
  }
  
  set.seed(110104)
  
  Phi <- function(x) 2*sqrt(4-x^2)
  
  X.dens <- function(nsim) runif(nsim, 0, 2)
  
  N <- seq(from=100, to=1000, by=100)
  
  data <- mc.intervals(Phi=Phi, N=N, X.dens=X.dens)
  
  data
  
  
  
  require(ggplot2)
  
  
  
  # ==============  ==============  ==============  ==============  ==============  =========
  # ==============  ==============  ==============  ==============  ==============  =========
  
  output$histograma <- renderPlot({
    ggplot(data, aes(x=N)) +
      
      geom_ribbon(aes(ymin=LI, ymax=UI), fill="grey", alpha=.4) +
      
      geom_line(aes(y=Estimate), colour="blue") +
      
      geom_hline(aes(yintercept=pi) , colour="red", linetype="dotted", size=1)
  })
  
  
}


