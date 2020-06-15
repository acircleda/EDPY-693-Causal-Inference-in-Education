#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(pwr)

data <- expand.grid(list(
  n = seq(1,1000,1),
  ES = seq(0.1,1,.01)
)) %>%
  mutate(
    power = round(pwr.t.test(n=n, d=ES, sig.level = .05)$power, digits=3)
  )

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    h1("Shiny Effect Size Estimator (T-Test only - draft)"),
               p("Select your expected power and sample size. The graph will plot the power curve and highlight the possible effect size. "),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          sliderInput("samplesize", label = "Sample Size", 
                      min = 10, 
                      max = 1000, value = 100, step=10),
          sliderInput("power", label = "Expected Power", 
                      min = .10, 
                      max = .9, value = .80, step=.1)
          
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$plot<-renderPlot({
    data %>%
      filter(power == input$power) %>%
      ggplot()+
      geom_line(aes(x=ES, y=n), size=1)+
      geom_point(data=(
        data %>% filter(n == input$samplesize) %>%
                   slice(which.min(abs(power - input$power)))),
                 aes(x=ES, y=input$samplesize), size=3, color="red")+
      geom_text(data=(
        data %>% filter(n == input$samplesize) %>%
          slice(which.min(abs(power - input$power)))),
                aes(x=ES, y=input$samplesize, label=paste0("d = ", ES)), size=3, color="red", hjust=-.5)+
      scale_x_continuous(breaks=seq(.1,1,.1))})
  }

# Run the application 
shinyApp(ui = ui, server = server)
