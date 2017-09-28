server <- shinyServer(function(input, output, session) {
  # added "session" because updateSelectInput requires it
  
  
  data <- reactive({ 
    req(input$file1) ## ?req #  require that the input is available
    
    inFile <- input$file1 
    
    # tested with a following dataset: write.csv(mtcars, "mtcars.csv")
    # and                              write.csv(iris, "iris.csv")
    df <- read.csv(inFile$datapath, header = input$header, sep = input$sep)
    df[is.na(df)] <- 0
    
    # Update inputs (you could create an observer with both updateSel...)
    # You can also constraint your choices. If you wanted select only numeric
    # variables you could set "choices = sapply(df, is.numeric)"
    # It depends on what do you want to do later on.
    #updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
     #                 choices = names(df), selected = names(df)[1])
    
    #updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
    #                  choices = names(df), selected = names(df))
    return(df)
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
  observeEvent(filtereddata(), {
  updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
                    choices = names(filtereddata()))
  
  updateSelectInput(session, inputId = 'ycol', label = 'Y Variable',
                    choices = names(filtereddata()))
  })
  
  output$contents <- renderDataTable(filtereddata())

  
  output$MyPlot <- renderPlot({
    gat <- gather( filtereddata(), rep, num, -one_of(input$xcol) )
    x <- ggplot(gat, aes(x = factor(input$xcol), y= num, col = "red")) + 
      geom_point(size = 0.4, position = position_dodge(width = 0.3))
    plot(x)
    
  })
})
