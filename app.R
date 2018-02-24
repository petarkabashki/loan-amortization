#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

ui <- fluidPage(
  
  # Application title
  titlePanel("Mortgage overpayment calculator"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      numericInput("balance", "Current mortgage balance:", min = 5000, value=500000),
      sliderInput("years", "Remaining term (years):", min = 1, max = 30, value = 20),
      sliderInput("months", "Months:", min = 0, max = 11, value = 0),
      sliderInput("apr", "Rate:", min = 0, max = 6, value = 4, step = 0.25),
      
      numericInput("over", "Regular monthly overpayment:", min=0, value = 500)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("chart")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  payment  <- function(loan, apr, months) {
    rate <- 1 + apr / 100 / 12
    loan * rate^months * (rate - 1) / (rate^months - 1)
  }
  
  amortize <- function(loan, payment, apr, months) {
    rate <- 1 + apr / 100 / 12
    month <- 0:months
    balance <- loan * rate^month - payment * (rate^month - 1) / (rate - 1)
    complete  <- match(TRUE, balance <= 0)
    balance <- ifelse(month < month[complete], balance, 0)
    principal <- loan - balance
    interest  <- payment * month - principal
    interest  <- ifelse(month < month[complete], interest, 
                        interest[complete-1])
    amrt <- list(month = month[-1], balance=balance[-1], 
                 principal = principal[-1], interest = interest[-1],
                 paid = principal[-1] + interest[-1], loan=loan,
                 payment=payment, apr=apr)
    class(amrt) <- "amortization"
    return(amrt)
    
  }
  
  # summary.amortization <- function(amrt) {
  #   cat("  loan amount: ", amrt$loan, "\n")
  #   cat("          APR: ", amrt$apr, "\n")
  #   cat("      payment: ", amrt$payment, "\n")
  #   cat("       months: ", match(TRUE, amrt$balance <= 0), "\n")
  #   cat("interest paid: ", max(amrt$interest), "\n")
  #   cat("   total paid: ", max(amrt$paid), "\n")
  # }
  
  # 
  # plot.amortization <- reactive({
  #   year <- amrt$month / 12
  #   plot(year, amrt$balance, type="l", ylab="Dollars ($)",
  #        main=paste("Amortization ($", format(amrt$loan, big.mark=","),
  #                   ", ", amrt$apr, "%, $", format(amrt$payment, big.mark=",",
  #                                                  nsmall=2, digits=2), "/mo)", sep=""), ylim=c(0,max(amrt$paid)),
  #        xlab="Year")
  #   lines(year, amrt$interest, col="red", lty=2)
  #   lines(year, amrt$principal, col="green", lty=2)
  #   lines(year, amrt$paid, col="blue", lty=2)
  #   legend(x=1, y=max(amrt$paid),
  #          legend=c("balance", "interest", "principal", "paid"), 
  #          col=c("black","red","green","blue"), lty=c(1,2,2,2))
  # })
  pmtR <- reactive({
    payment(input$balance, input$apr, 12*input$years + input$months)  
  });
  
  amrtR <- reactive({
    amortize(input$balance, pmtR() + input$over, input$apr, 12*input$years + input$months)
  });
  
  output$chart <- renderPlot({
      amrt <- amrtR()
      year <- amrt$month / 12
      plot(year, amrt$balance, type="l", ylab="Pounds (£)",
           main=paste("Amortization (£", format(amrt$loan, big.mark=","),
                      ", ", amrt$apr, "%, £", format(amrt$payment, big.mark=",",
                                                     nsmall=2, digits=2), "/mo)", sep=""), 
           ylim=c(0,max(amrt$paid)),
           xlab="Year",
           lwd = 3
           )
      lines(year, amrt$interest, col="red", lwd = 3, lty = 3)
      lines(year, amrt$principal, col="green", lwd=3, lty = 3)
      lines(year, amrt$paid, col="blue", lwd=3, lty = 3)
      legend(x=1, y=max(amrt$paid),
             legend=c("balance", "interest", "principal", "paid"),
             col=c("black","red","green","blue"), lty=c(1,3,3,3))
    
    # plot(rnorm(100))
    # ggplot(data = amtdf) +
    #   geom_line(mapping = aes(x = years, y = amtBalances), color = "blue") +
    #   geom_point(mapping = aes(x = years, y = amtBalances), color = "blue") +
    #   geom_line(mapping = aes(x = years, y = ovBalancesPos), color = "green") +
    #   geom_point(mapping = aes(x = years, y = ovBalancesPos), color = "green")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

