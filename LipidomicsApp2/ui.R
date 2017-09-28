library(shiny)
library(datasets)
library(DT)
library(tidyr)
library(dplyr)

ui <- shinyUI(fluidPage(
  titlePanel("Column Plot"),
  tabsetPanel(
    tabPanel("Upload File",
             titlePanel("Uploading Files"),
             sidebarLayout(
               sidebarPanel(
                 fileInput('file1', 'Choose CSV File',
                           accept=c('text/csv', 
                                    'text/comma-separated-values,text/plain', 
                                    '.csv')),
                 
                 # added interface for uploading data from
                 # http://shiny.rstudio.com/gallery/file-upload.html
                 tags$br(),
                 checkboxInput('header', 'Header', TRUE),
                 radioButtons('sep', 'Separator',
                              c(Comma=',',
                                Semicolon=';',
                                Tab='\t'),
                              ','),
                 selectInput("select", "Select columns to display",c(), multiple = TRUE),
                 helpText(em("Note: Use delete button to de-select columns")),
                 actionButton("update", "Update Data Set", class = "btn-primary",style='padding:4px; font-size:120%')
                 
                 
               ),
               mainPanel(
                 dataTableOutput('contents')
               )
             )
    ),
    tabPanel("First Type",
             pageWithSidebar(
               headerPanel('Basic scatter plot'),
               sidebarPanel(
                 
                 # "Empty inputs" - they will be updated after the data is uploaded
                 selectInput('xcol', 'X Variable', ""),
                 selectInput('ycol', 'Y Variable', "", selected = "")
                 
               ),
               mainPanel(
                 plotOutput('MyPlot')
               )
             )
    )
    
  )
)
)
