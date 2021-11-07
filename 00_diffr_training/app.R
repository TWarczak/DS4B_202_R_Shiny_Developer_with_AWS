library(shiny)
library(tidyverse)

ui <- fluidPage(
    h1("Matt's Fluid Page"),
    p("We're going to test your ability to detect differences.")
)

server <- function(input, output, server) {
    
}

shinyApp(ui, server)
