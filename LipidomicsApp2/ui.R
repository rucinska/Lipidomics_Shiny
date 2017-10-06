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
                 actionButton("update", "Update Data Set", class = "btn-primary",style='padding:4px; font-size:120%'),
                 helpText(em("First column will be used as a x axis in the plot!!!"))
                 
               ),
               mainPanel(
                 dataTableOutput('contents')
               )
             )
    ),
    tabPanel("Lipid Abundance",
             pageWithSidebar(
               headerPanel('Lipid Abundance'),
               
               sidebarPanel(
                 helpText(em("Note: Select velues that are not experimanetal conditions.")),
                 # "Empty inputs" - they will be updated after the data is uploaded
                 selectInput('nonexp', "Not experimanetal conditions", "", multiple = TRUE),
                 #selectInput('xcol', 'X Variable', "", selected = ""),
                 textInput('xlab', 'X axis label', value = ""),
                 textInput('ylab', 'Y axis label', value = ""),
                 textInput('plotTitle', 'Plot title', value = ""),
                 selectInput('legendposition', label ='Legend Position',
                  choices=c("left", "right", "bottom", "top"),
                  multiple=FALSE, selectize=TRUE,selected="bottom")
                 
                
               ),
               mainPanel(
                 plotOutput('MyPlot')
               )
             )
    ) #end tabPanel
    
  )
)
)
