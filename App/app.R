#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(tidyverse)

# I only selected the variables from our previous assignment but we can change this to include all the others maybe
# just picking these for now
dig <- read.csv("DIG.csv") %>%
  select(ID, TRTMT, AGE, SEX, BMI, KLEVEL, CREAT, DIABP, SYSBP,
         HYPERTEN, CVD, WHF, DIG, HOSP, HOSPDAYS, DEATH, DEATHDAY)

# this is how I labelled the factor variables in the previous assignment
dig <-dig %>%
  mutate(
    TRTMT    = factor(TRTMT, levels = c(0, 1), labels = c("Placebo", "Digoxin")),
    SEX      = factor(SEX, levels = c(1, 2), labels = c("Male", "Female")),
    HYPERTEN = factor(HYPERTEN, levels = c(0, 1), labels = c("No", "Yes")),
    CVD      = factor(CVD,levels = c(0, 1), labels = c("No", "Yes")),
    WHF      = factor(WHF,levels = c(0, 1), labels = c("No", "Yes")),
    DIG      = factor(DIG,levels = c(0, 1), labels = c("No", "Yes")),
    HOSP     = factor(HOSP,levels = c(0, 1), labels = c("No", "Yes")),
    DEATH    = factor(DEATH,levels = c(0, 1), labels = c("Alive", "Dead"))
  )




# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("DIG Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)





# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- DIG[, 3]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'Black', border = 'white',
             xlab = 'Age',
             main = 'Histogram of Age')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
