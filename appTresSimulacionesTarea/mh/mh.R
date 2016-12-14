library(invgamma)

require(MASS)
require("goftest")


mh <- function(input, output, session)
{
  df <- reactive({
    f = input$file1
    if(is.null(f))
      NULL
    else
      read.csv(f$datapath)
  })
  
  data <- reactive({
    f = input$file1
    if(is.null(f))
      NULL
    else
      as.matrix(read.csv(f$datapath))
  })
  
  output$data <- renderTable({
    data()
  })
  
  output$vdui <- renderUI({
    selectInput(session$ns('vd'), label = 'Variable dependiente', names(df()))
  })
  
  output$viui <- renderUI({
    selectInput(session$ns('vi'), label = 'Variable dependiente', names(df()))
  })
  
  output$varpt <- renderPlot({
    if(!is.null(df))
    {
      y <- df()[[input$vd]]
      x <- df()[[input$vi]]
      plot(x,y)
    }
  })
  
  nreg <- reactive({
    equis<-df()[[input$vi]]
    ye <- df()[[input$vd]]
    sp<-data.frame(equis,ye)
    splm<-lm(ye~equis,data=sp)
    summary_splm<-summary(splm)
    betas<-coefficients(summary_splm)
    list('betas' = betas, 'summary' = summary_splm)
  })
  
  ndist <- reactive({
    x <- seq(-100, 100, length=100)
    dnorm(x,round(nreg()$betas[1,1],digits=2),round(nreg()$betas[1,2],digits=2))
  })
  
  gdist <- reactive({
    x <- seq(-100, 100, length=100)
    dinvgamma(x,13.5,round(25*nreg()$summary$sigma,digits=2))
  })
  
  output$apra <- renderPlot({
    x <- seq(-100, 100, length=100)
    plot(x, ndist(), type="l", lty=2, xlab="x value",ylab="Density", main=paste('A priori a'))
  })
  
  output$aprb <- renderPlot({
    x <- seq(-100, 100, length=100)
    plot(x, ndist(), type="l", lty=2, xlab="x value",ylab="Density", main=paste('A priori b'))
  })
  
  output$aprg <- renderPlot({
    x <- seq(-100, 100, length=100)
    plot(x, gdist(), type="l", lty=2, xlab="x value",ylab="Density", main=paste('A priori eps'))
  })
  
  
  likelihood <- function(param){
    b1= param[1]
    b0 = param[2]
    sigma2 = param[3]
    pred = b1*df()[[input$vi]] + b0
    singlelikelihoods = dnorm(df()[[input$vd]], mean = pred, sd = sigma2**.5, log = T)
    sumll = sum(singlelikelihoods)
    return(sumll)
  }
  
  prior <- function(param){
    b1 = param[1]
    b0 = param[2]
    sigma2 = param[3]
    b1prior = dnorm(b1, mean=round(nreg()$betas[1,1],digits=2), sd=round(nreg()$betas[1,2]**.5,digits=2), log = T)
    b0prior = dnorm(b0, mean=round(nreg()$betas[2,1],digits=2), sd=round(nreg()$betas[2,2]**.5,digits=2), log = T)
    sigma2prior = dinvgamma(sigma2,14,round(25*nreg()$summary$sigma,digits=2),log = T)
    return(b1prior+b0prior+sigma2prior)
  }
  
  posterior <- function(param){
    return (likelihood(param) + prior(param))
  }
  
  #Metropolis
  
  proposalfunction <- function(param){
    return(rnorm(3,mean = param, sd= c(0.1,0.5,0.3)))
  }
  
  run_metropolis_MCMC <- function(startvalue, iterations){
    chain <- array(dim = c(iterations+1,3))
    chain[1,] <- startvalue
    for (i in 1:iterations){
      proposal <- proposalfunction(chain[i,])
      
      logprobab =posterior(proposal) - posterior(chain[i,])
      if (log(runif(1)) <= logprobab){
        chain[i+1,] = proposal
      }else{
        chain[i+1,] = chain[i,]
      }
    }
    return(chain)
  }
  
  mcmc <- reactive({
    startvalue = c(rnorm(1,0,1),rnorm(1,0,1),rinvgamma(1,1,1))
    chain = run_metropolis_MCMC(startvalue, input$cadenas)
    data.frame(b1=chain[,1],b0=chain[,2],s2=chain[,3])
  })
  
  output$sim <- renderDataTable({
    mcmc()
  })
  
  output$histos<-renderPlot({
    burnIn = input$cadenas*.20
    acceptance = 1-mean(duplicated(mcmc()[-(1:burnIn),]))
    par(mfrow = c(2,3))
    hist(mcmc()[-(1:burnIn),1],nclass=30,  main="Posterior of b1", xlab="Parametro" )
    abline(v = mean(mcmc()[-(1:burnIn),1]))
    hist(mcmc()[-(1:burnIn),2],nclass=30, main="Posterior of b0", xlab="Parametro")
    abline(v = mean(mcmc()[-(1:burnIn),2]))
    hist(mcmc()[-(1:burnIn),3],nclass=30, main="Posterior of sigma^2", xlab="Parametro")
    abline(v = mean(mcmc()[-(1:burnIn),3]) )
    plot(mcmc()[-(1:burnIn),1], type = "l", xlab="Iteraciones" , main = "Chain values of b1" )
    plot(mcmc()[-(1:burnIn),2], type = "l", xlab="Iteraciones" , main = "Chain values of b0")
    plot(mcmc()[-(1:burnIn),3], type = "l", xlab="Iteraciones" , main = "Chain values of sigma^2")
  })
  
}