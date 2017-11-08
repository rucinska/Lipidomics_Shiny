library(shiny)
library(datasets)
library(DT)
library(tidyr)
library(dplyr)
library(shinyjs)
library(ggrepel)
library(ggbiplot)

inline_ui <- function(tag) {
  div(style = "display: inline-block", tag)
}

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
                 textInput('Legend', 'Legend', value = "Lipid Abundance"),
                 selectInput('legendposition', label ='Legend Position',
                             choices=c("left", "right", "bottom", "top"),
                             multiple=FALSE, selectize=TRUE,selected="bottom")
                
               ),
               mainPanel(
                 column(
                   8,
                 plotOutput('MyPlot'),
                 div(
                   id = "save_plot_area",
                   inline_ui(
                     textInput("save_plot_name", NULL, "",
                               placeholder = "Enter plot name to save")
                   ),
                   actionButton("save_plot_btn", "Save plot", icon = icon("star")),
                   shinyjs::hidden(
                     span(
                       id = "save_plot_checkmark",
                       icon("check")
                     )
                   )
                 )
                 )
             ) #end mainPanel
    )
    ),#end tabPanel
    tabPanel("PCA",
             pageWithSidebar(
               headerPanel('Principal Component Analysis'),
               
               sidebarPanel(
                 helpText(em("Note: Select velues that are not experimanetal conditions.")),
                 # "Empty inputs" - they will be updated after the data is uploaded
                 selectInput('nonexp2', "Not experimanetal conditions", "", multiple = TRUE),
                 textInput('xlab2', 'X axis label', value = "PC1"),
                 textInput('ylab2', 'Y axis label', value = "PC2"),
                 textInput('plotTitle2', 'Plot title', value = "Principal Componet Analysis")
                 
                 
                 
                 
               ),
               mainPanel(
                 column(
                   8,
                   plotOutput('PCA'),
                   div(
                     id = "save_plot_area",
                     inline_ui(
                       textInput("save_plot_name_pca", NULL, "",
                                 placeholder = "Enter plot name to save")
                     ),
                     actionButton("save_plot_btn_pca", "Save plot", icon = icon("star")),
                     shinyjs::hidden(
                       span(
                         id = "save_plot_checkmark_pca",
                         icon("check")
                       )
                     )
                   )
                 )
               ) #end mainPanel
             )
      
      
    ),#END tabPanel
    
    tabPanel("Box Plot",
             pageWithSidebar(
               headerPanel('Box Plot'),
               
               sidebarPanel(
                 helpText(em("Note: Select for which type you would like to make a boxplot.")),
                 # "Empty inputs" - they will be updated after the data is uploaded
                 radioButtons('type', 'Type',
                              c(DB ='DB',
                                Length='length',
                                Class='WT'),
                              'DB'),
                 textInput('xlab3', 'X axis label', value = ""),
                   textInput('ylab3', 'Y axis label', value = "Lipid Abundance [%/mol]"),
                 textInput('plotTitle3', 'Plot title', value = "")
                 
                 
                 
                 
               ),
               mainPanel(
                 column(
                   8,
                   plotOutput('BoxPlot'),
                   div(
                     id = "save_plot_area",
                     inline_ui(
                       textInput("save_plot_name_boxplot", NULL, "",
                                 placeholder = "Enter plot name to save")
                     ),
                     actionButton("save_plot_btn_boxplot", "Save plot", icon = icon("star")),
                     shinyjs::hidden(
                       span(
                         id = "save_plot_checkmark_boxplot",
                         icon("check")
                       )
                     )
                   )
                 )
               ) #end mainPanel
             )
             
             
    ),#END tabPanel
    
    
    
    
    
    tabPanel("Export",
      conditionalPanel(
        condition = "!output.saved_plots_exist",
        h2("You do not have any saved plots to export")
      ),
      conditionalPanel(
        condition = "output.saved_plots_exist",
        fluidRow(
          column(
            4,
            h2("Export Options"),
            div(
              id = "exporting_plots_options",
              selectInput("export_file_type", "File type",
                          c("PDF" = "pdf", "JPEG" = "jpeg", "PNG" = "png", "EPS" = "eps")),
              conditionalPanel(
                condition = "input.export_file_type == 'pdf'",
                selectInput("export_pdf_orientation", "Page orientation",
                            c("Portrait (8.5\" x 11\")" = "portrait",
                              "Landscape (11\" x 8.5\")" = "landscape",
                              "Custom dimensions" = "custom")
                ),
                conditionalPanel(
                  condition = "input.export_pdf_orientation == 'custom'",
                  numericInput("export_pdf_width", "Page width (inches)",
                               value = 8.5, min = 1, max = 50, step = 0.5),
                  numericInput("export_pdf_height", "Page height (inches)",
                               value = 11, min = 1, max = 50, step = 0.5)
                )
              ),
              conditionalPanel(
                condition = "input.export_file_type != 'pdf'",
                numericInput("export_file_width", "Image width (pixels)",
                             value = 480, min = 100, max = 2000),
                numericInput("export_file_height", "Image height (pixels)",
                             value = 480, min = 100, max = 2000)
              ),
              checkboxInput("export_multiple", "Multiple plots per page"),
              conditionalPanel(
                condition = "input.export_multiple",
                selectInput("export_arrangement", NULL,
                            c("Arrange plots by row" = "byrow",
                              "Arrange plots by column" = "bycol")),
                numericInput("export_nrow", "Rows per page",
                             value = 1, min = 1, max = 20),
                numericInput("export_ncol", "Columns per page",
                             value = 1, min = 1, max = 20)
                
              ),
              uiOutput("export_btn_ui")
            )
          ),
          column(
            8,
            h2("Preview"),
            strong("Remove plot"), br(),
            inline_ui(uiOutput("plots_remove_ui")),
            actionButton("remove_plot_btn", "Remove"),
            uiOutput("plots_order_ui"),
            div(
              id = "preview_plots_options",
              uiOutput("plots_select_page_ui"),
              plotOutput("plot_preview", height = "auto")
            )
          )
        )
      )
    )#end tabPanel
  
    
) #end tabsetPanel
)
)
