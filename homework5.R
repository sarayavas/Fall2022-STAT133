# Title: Basic Investment Simulator
# Description: This is a shiny app for the "Investment Simulations" tutorial
# available in bCourses (link below)
# https://bcourses.berkeley.edu/courses/1516876/files/folder/readings?preview=84322485
#
# Author: Sara Yavas


# ------------------------
# Required packages
# ------------------------
library(shiny)
library(tidyverse)
library(plotly)


# Define UI
ui <- fluidPage(
  
  # Application title
  titlePanel("$5000 Investment Simulator"),
  fluidRow(
    # Inputs for initial deposit
    column(width = 3,
           h4("Initial Amount & Periodic Contributions"),
           numericInput(inputId = "initial_amount", 
                        label = "initial amount", 
                        value = 1000),
           numericInput(inputId = "per_cont", 
                        label = "Periodic Contributions", 
                        value = 360)
    ),
    
    # Inputs for mean and standard deviation of annual return rates
    column(width = 3,
           h4("Target Amount & Number of Years"),
           numericInput(inputId = "target_amt", 
                        label = "Target Amount", 
                        value = 5000),
           sliderInput(inputId = "num_years", 
                       label = "number of years", 
                       value = 10, 
                       min = 1, 
                       max = 30)
    ),
    
    # Inputs for number of simulations, and random seed
    column(width = 3,
           h4("Portfolio Composition"),
           sliderInput(inputId = "portfolio", 
                       label = "Proportion of Stocks", 
                       value = 50, 
                       min = 0, 
                       max = 100,
                       step=5)
    ),
    
    # Inputs for number of simulations, and random seed
    column(width = 3,
           h4("Simulation Parameters"),
           sliderInput(inputId = "num_simulations", 
                       label = "number of simulations", 
                       value = 50, 
                       min = 10, 
                       max = 100),
           numericInput(inputId = "seed", 
                        label = "random seed", 
                        value = 123)
    )
  ),
  
  hr(),
  h4('Graph of Simulations'),
  plotlyOutput('plot_sim', height = 900, width=1200),
  
  hr(),
  h4('Graph of Probabilities'),
  plotOutput('plot_targ'),
  
  hr(),
  fluidRow(
    column(width = 6,
           h4('Summary Statistics Table 1'),
           p(' Saving Statistics per year'),
           tableOutput('table1')
    ),
    column(width = 6,
           h4('Summary Statistics Table 2'),
           p('Simulations that have surpassed target amount ($5000)'),
           tableOutput('table2')
    )
  )
)


stocks<- c(0, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, 90, 95, 100)
avg <- c(5.09, 5.40, 5.70, 6.00, 6.29, 6.57, 6.84, 7.11, 7.37, 7.62, 7.87, 8.11, 8.34, 8.56, 8.78, 8.99, 9.19, 9.38, 9.57, 9.74, 9.91)
sds <- c(4.03, 3.99, 4.11, 4.37, 4.74, 5.21, 5.74, 6.32, 6.94, 7.59, 8.25, 8.94, 9.64, 10.35, 11.07, 11.80, 12.54, 13.28, 14.03, 14.79, 15.55)
portfolio_table <- data.frame(stocks, (avgs/100), (sds/100))



# Define server
server <- function(input, output) {
  
  # ------------------------
  # matrix of simulated data
  # ------------------------
  balance_mat = reactive({
    balance = matrix(0, nrow = input$num_years + 1, ncol = input$num_simulations)
    balance[1, ] = input$initial_amount
    set.seed(input$seed)

    for (sim in 1:input$num_simulations) {
      aver=portfolio_table[portfolio_table$stocks == input$portfolio, 2]
      stand=portfolio_table[portfolio_table$stocks == input$portfolio, 3]
      return_rates = rnorm(input$num_years, mean = aver, sd = stand)
      for (year in 1:input$num_years) {
        balance[year+1,sim] = (balance[year,sim] * (1 + return_rates[year])) + input$per_cont
      }
    }
    
    colnames(balance) = paste0("sim", 1:input$num_simulations)
    balance
  })
  
  
  # -------------------------------------------------------
  # reshape table into long format (for ggplot convenience)
  # -------------------------------------------------------
  dat_sim = reactive({
    tbl = as.data.frame(balance_mat())
    tbl$year = 0:input$num_years
    
    dat = pivot_longer(
      data = tbl, 
      cols = starts_with("sim"), 
      names_to = "simulation",
      values_to = "amount")
    
    dat
  })
  
  
  # --------------------------------
  # Output: timelines of simulations
  # --------------------------------
  output$plot_sim <- renderPlotly({
    gp<- ggplot(data = dat_sim(), aes(x = factor(year), y = amount)) +
      geom_line(alpha=.2, aes(group = simulation, color=factor(simulation))) + 
      labs(title="50 simulations of a 10-year investment period",  y="balance ($)", x = "year", color="random simulation") + 
      geom_hline(yintercept = input$target_amt, na.rm=T, alpha=.2, color="blue", linetype="dashed") +
      geom_vline(xintercept = mean(dat_sim()$year[dat_sim()$amount >= input$target_amt]), alpha=.2, color="blue", linetype="dashed") +
      annotate(geom="text", x = 1.5, y=(input$target_amt)+10, label="Target amount", color="black") +
      annotate(geom="text", x = (mean(dat_sim()$year[dat_sim()$amount >= input$target_amt])), y=100, label="Average years to target", color="black") 
    ggplotly(gp)
  })
  


  
  output$plot_targ <- renderPlot({
    dat_sim() %>%
      group_by(year) %>%
      summarise(
        count = sum(amount > input$target_amt),
        proportion = count / input$num_simulations
      ) %>%
      ggplot(aes(x = factor(year), y = proportion)) + geom_col() + 
      labs(title="Probability of Reaching Target",  y="Proportion of simulations that have reached target amount ($5000)", x = "year")
  })
  
  
  
  # ---------------------------
  # Output: Summary Statistics 1
  # ---------------------------
  # Saving Statistics per year
  output$table1 <- renderTable({
    dat_sim() %>% 
      #filter(year %in% seq(0, input$num_years, by = 2)) %>%
      group_by(year) %>%
      summarise(
        min = min(amount),
        max = max(amount),
        median = median(amount),
        avg = mean(amount)
      )
  })

    
  # ---------------------------
  # Output: Table2
  # ---------------------------
  output$table2 <- renderTable({
    dat_sim() %>% 
      group_by(year) %>%
      summarise(
        count = sum(amount > input$target_amt),
        proportion = count / input$num_simulations
      )
  })
  
}

# Run the application 
shinyApp(ui = ui, server = server)
