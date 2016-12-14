require(MASS)
require("goftest")


arRe <- function(input, output, session) {
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
  
  # =======================
  
  
  alpha = 10 # Some random values for this example.
  
  beta = 5
  
  f <- function(x, alpha, beta){
    
    gamma(alpha + beta)/(gamma(alpha)*gamma(beta))*x^(alpha-1)*(1-x)^(beta-1)
    
  }
  
  g <- function(x) {1}
  
  M <- f((alpha-1)/(alpha+beta-2), alpha, beta)
  
  ggplot(data.frame(x=c(0,1)), aes(x)) +
    
    stat_function(fun = f, arg=list(alpha=alpha, beta=beta), aes(colour = "f")) +
    
    stat_function(fun = function(x) g(x)*M, aes(colour = "M*g")) +
    
    scale_colour_manual("Function", values = c("red", "blue")) + ylab("density")
  
  set.seed(999)
  
  nsim <- 2000
  
  rBeta.aux <- function(alpha, beta, verbose=FALSE){
    d <- data.frame() # To keep track of acceptance and rejection.
    
    accepted <- FALSE
    
    max.iter <- 10e6 # When we use whiles, we set a max.iter paramater for security.
    
    iter <- 1
    
    while(!accepted & iter<=max.iter){
      
      U <- runif(1)
      
      Y <- runif(1) # In our case, g is uniform [0,1]. So this is sampling from g.
      
      if(U <= f(Y, alpha, beta)/(g(Y)*M)){
        
        accepted <- TRUE
        
        X <- Y
        
      }
      
      d <- rbind(d, data.frame(Y=round(Y,4), U=round(U,4),
                               
                               f.over.Mg=round(f(Y, alpha, beta)/(g(Y)*M),4), Accepted=accepted))
      
      iter <- iter + 1
      
    }
    
    if(verbose==TRUE){
      
      return(d) # If we indicate verbose we return a table with outcomes.
      
    } else{
      
      return(X)
      
    }
    
  }
  
  rBeta <- function(nsim, alpha, beta){
    
    replicate(nsim, rBeta.aux(alpha=alpha, beta=beta))
  }
  
  hist(rBeta(nsim, alpha, beta), breaks=20, main="", prob=TRUE)
  rBeta.aux(alpha, beta, verbose=TRUE)
  
  
  
  # =======================
  
  output$histograma <- renderPlot({
    hist(main = "Histograma de Simulaciones" , finv())
  })
  
  
}