

library(shiny)
library(tidyverse)
library(DT)
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









# UI
ui <- navbarPage("DIG Data",
                 
                 tabPanel("Data Info Tab",
                          sidebarLayout(
                            sidebarPanel(
                              p("This app can be used to explore the DIG dataset. The DIG dataset contains data...")
                            ),
                            mainPanel(
                              DT :: dataTableOutput("dataDisplay")
                            )
                          )
                 ),
                 
                 #summary statistics tab
                 tabPanel("Summary Statistics",
                          selectInput("summary_var", "Select Variable:", choices = names(dig)),
                          DTOutput("SummaryStatsTable")
                          
                 ),
                 
                 

    # Application title
    tabPanel("Variable Distributions",

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            selectInput("HistogramVariable", "Variable:",
                        choices = names(dig)[sapply(dig, is.numeric)])
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
          plotOutput("distPlot")
        )
    )
),





#second tab
tabPanel("Box Plot",
         mainPanel(plotOutput("SysBPvsCVD"))
  
)



)




# Server
server <- function(input, output) {

    output$distPlot <- renderPlot({
      req(input$HistogramVariable)
        x    <- dig[[input$HistogramVariable]]
        bins <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'Purple', border = 'white',
             xlab = input$HistogramVariable,
             main = 'Histogram of Chosen Variable')
    })
    
    output$SysBPvsCVD <- renderPlot({
      ggplot(dig, aes(x = CVD, y = SYSBP))+
        geom_boxplot(colour = "purple")
    })
    
    
    
    output$dataDisplay <- DT :: renderDataTable({
      datatable(
        dig,
        options = list(
          pageLength = 5,      # Set rows per page
          autoWidth = TRUE,    # Auto-adjust column widths
          searching = TRUE,    # Enable data filtering
          ordering = TRUE    # Enable column sorting,
      ))
      
      
    })
    
    output$SummaryStatsTable <- renderDT({
      selected_var <- input$summary_var
      summary_data <- summary(dig[[selected_var]])
      summary_df <- data.frame(Statistic = names(summary_data), Value = as.numeric(summary_data))
      datatable(summary_df, options = list(pageLength = 5))
    })
    
    
}













# Run the application 
shinyApp(ui = ui, server = server)
