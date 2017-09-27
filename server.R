
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(DT)

shinyServer(function(input, output, session) {
  
  #DIPLAY THE DATA
  data <- reactive({
    req(input$file1)
    read.csv(input$file1$datapath,na.strings = c("NA","."), header = input$header,sep = input$sep)
    
  })
  
  filtereddata <- eventReactive({
    #validate(need(input$dataset != "","Please select a data set in csv format only!!!"))#
    input$update
    data()
  },  {
    req(data())
    if(is.null(input$select) || input$select == "")
      data() else 
        data()[, colnames(data()) %in% input$select]
  })
  
  observeEvent(data(), {
    updateSelectInput(session, "select", choices=colnames(data()))
  })
  
  
  output$contents <- renderDataTable(filtereddata())
  
})
