#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

if (interactive()) {
  
  ui <- fluidPage(
    checkboxGroupInput("variable", "Variables to show:",
                       c("Cylinders" = "cyl",
                         "Transmission" = "am",
                         "Gears" = "gear")),
    tableOutput("data")
  )
  
  server <- function(input, output, session) {
    output$data <- renderTable({
      mtcars[, c("mpg", input$variable), drop = FALSE]
    }, rownames = TRUE)
  }
  
  shinyApp(ui, server)
  
  ui <- fluidPage(
    checkboxGroupInput("icons", "Choose icons:",
                       choiceNames =
                         list(icon("calendar"), icon("bed"),
                              icon("cog"), icon("bug")),
                       choiceValues =
                         list("calendar", "bed", "cog", "bug")
    ),
    textOutput("txt")
  )
  
  server <- function(input, output, session) {
    output$txt <- renderText({
      icons <- paste(input$icons, collapse = ", ")
      paste("You chose", icons)
    })
  }
  
  shinyApp(ui, server)
}