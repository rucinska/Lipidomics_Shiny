
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(dplyr)
library(tidyr)
library(DT)

# Define UI for application that plots the Lipidomics data 
ui <- fluidPage(
  # Application title
  titlePanel("Lipidomics"),
  
  sidebarLayout(
    
    sidebarPanel(
      tabsetPanel(
        tabPanel("Inputs",
                 fileInput("file1", "Choose CSV File",
                           accept = c(
                             "text/csv",
                             "text/comma-separated-values,text/plain",
                             ".csv")
                 ),
                 tags$hr(),
                 
                 checkboxInput("header", "Header", TRUE),
                 radioButtons("sep", "Separator",
                              choices = c(Comma = ",",
                                          Semicolon = ";",
                                          Tab = "\t"),
                              selected = ","),
                 
                 selectInput("select", "Select columns to display",c(), multiple = TRUE),
                 helpText(em("Note: Use delete button to de-select columns")),
                 actionButton("update", "Update Data Set", class = "btn-primary",style='padding:4px; font-size:120%')
                 
              ), #end tabPabel
        tabPanel("How To",
                 h5("1. Upload your data file in CSV format. Missing values will be replaced with 0!!. "),
                 h5("2. You can choose if you want a header to be displayed and what kind of seperator you have in csv"),                           
                 h5("3. You can select specific columns for your further analysis. To do that slect columns in dropdown menu. In case of mistake, you can de-select them with delete bottom. Click 'Update Data Set' bottom to work only with selected columns.")
                 ), # tabpanel 
        tabPanel("Plot all Data",
                 
                   headerPanel('Basic Plot'),
                   sidebarPanel(
                     
                     # "Empty inputs" - they will be updated after the data is uploaded
                     selectInput('xcol', 'X Variable', ""),
                     selectInput('ycol', 'Y Variable', "", selected = "")
                     
                   )
                  
                 )
      ) #end tabsetPanel
    ), #end sidebarPanel
    mainPanel(
      tabsetPanel(
        tabPanel('Plot', plotOutput('MyPlot')),
        tabPanel('Data', dataTableOutput( "contents") )
      ) #close tabsetPanel
    ) #close mainPanel
    
  ) #clouse sidebarLayout
) #close FluidPage
