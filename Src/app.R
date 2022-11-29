#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyjs)
library(shinydashboard)
library(ggplot2)

data <- read.csv("E:\\Visualizatio\\Tokyo_Olympics\\tokyo_2021.csv")
df <- as.data.frame(data)
ggplot2(data, aes(x))
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Tokyo Olympics 2020"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("Top",
                        "Number of Countries",
                        min = 1,
                        max = 93,
                        value = 5)
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
        y    <- df[,3]
        x <- df[input$bins,1]

        # draw the histogram with the specified number of bins
        hist(x,y, col = 'darkgray', border = 'white', ylim = input$bins,
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
