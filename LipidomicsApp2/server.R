#https://github.com/smouksassi/ggplotwithyourdata/blob/master/miniapp_exportplots/app.css
#https://github.com/smouksassi/ggplotwithyourdata
#https://pharmacometrics.shinyapps.io/ggplotwithyourdata/
#https://github.com/isop-phmx/GGplot-Shiny/blob/master/GGPLOTSHINY.Rmd

server <- shinyServer(function(input, output, session) {
  # added "session" because updateSelectInput requires it
  values <- reactiveValues(
    plots = list()
  )
  
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
  updateSelectInput(session, inputId = 'nonexp', label = 'Not experimanetal conditions',
                    choices = names(filtereddata()))
  
  updateSelectInput(session, inputId = 'nonexp2', label = 'Not experimanetal conditions',
                        choices = names(filtereddata()))
  #updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
  #                choices = names(filtereddata()))
  })
  
  output$contents <- renderDataTable(filtereddata())

  plot_object <- reactive({
    gat <- gather(filtereddata(), rep, num, -one_of(input$nonexp) )
    #gat$rep<-sub("\\d$","",gat$rep)
    #x <- ggplot(gat, aes(x = rep, y = num)) + geom_point() + theme(axis.text.x=element_text(angle=90,hjust=1))
    # gat <- gather(filtereddata(), rep, num, gather_cols = input$ycol, -one_of(input$xcol) )
    
    gat[,1] <- factor(gat[,1], levels=unique(gat[,1]))
    ggplot(gat, aes(x = gat[,1], y= num, col = "Lipid Abundance")) + 
      geom_point(size = 0.4, position = position_dodge(width = 0.3)) +
      stat_summary(fun.y= "mean", aes( group=1, colour = "Mean"), geom="point",size = 0.5, show.legend = TRUE )+ 
      theme_bw() +
      theme(legend.position=input$legendposition, 
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.x=element_text(angle=90,hjust=1),
            text = element_text(size=10),
            axis.ticks.length=unit(0.5,"cm"),
            legend.text=element_text(size=10)) +
      labs(title = input$plotTitle, x = input$xlab, y = input$ylab, color = " ")+
      scale_color_manual(labels = c(input$Legend, "Mean"), values = c("red", "black"))
    
  })
  
  PCA <- reactive({
    gat_pca <- gather(filtereddata(),  replicate, membr_perc, -one_of(input$nonexp2) )
    gat_pca <- select(gat_pca,-c(2,3))
    
    
    wt_spread <- spread(gat_pca, WT, membr_perc)
    #rownames(lip_db_gather) <- lip_db_gather$con_type
    #lip_temp_gather<- lip_temp_gather%>% separate(con_type, into = c("type", "con"),sep =  "_")
    wt_spread$replicate <- make.names(wt_spread$replicate, unique=TRUE)
    rownames(wt_spread) <- wt_spread$replicate
    wt_spread <- select(wt_spread,-replicate)
    
    
    
    wt.rep <- rownames(wt_spread)
    wt.rep<-sub("^X","",wt.rep)
    wt.rep<-sub("\\d{1}$","",wt.rep)
    wt.rep<-sub("\\.$","",wt.rep) #remomve dot at the end
    
    
    prin_comp_wttwmp <- prcomp(wt_spread, scale. = T)
    
    
    ggbiplot(prin_comp_wttwmp, 
             obs.scale = 1, 
             var.scale = 1,
             groups = wt.rep ,
             var.axes = FALSE,
             ellipse = FALSE) +
      #label = wt.rep)
      geom_point(aes( colour=wt.rep), size = 1) +
      geom_text_repel(aes(label = wt.rep, 
                          col =wt.rep),
                      size = 2,
                      segment.color = 'grey50')+
      theme_bw()+  
      theme( legend.position = "none") +
      labs(title = input$plotTitle2, x = input$xlab2, y = input$ylab2, color = " ")
    
    

  })
  
  BoxPlot <- reactive({
    if (input$type == "DB") {
      bx_data <- filtereddata() %>% select( -c(WT,length))
      bx_data_gat <- bx_data %>% gather(rep, num, -one_of("DB"))
      
      
    } else if(input$type == "length") {
      bx_data <- filtereddata() %>% select( -c(WT,DB))
      bx_data_gat <- bx_data %>% gather(rep, num, -one_of("length"))
    } else {
      bx_data <- filtereddata() %>% select( -c(DB,length))
      bx_data<-bx_data%>%separate(WT, into = c("WT", "bal"),sep =  " ")
      bx_data <- bx_data %>% select(-bal)
      bx_data_gat <- bx_data %>% gather(rep, num, -one_of("WT"))
      bx_data_gat$WT <-  sub("^m", "", bx_data_gat$WT )
      bx_data_gat$WT <-  sub("DIP", "DIPs", bx_data_gat$WT )
      #wt_class_gat_sum <-wt_class_gat %>% group_by(rep, class) %>% summarise_all(funs(sum(.)))
    }
    #gat_pca <- gather(filtereddata(),  replicate, membr_perc, -one_of(input$nonexp2) )
  
    bx_data_gat_sum <- bx_data_gat %>%
      group_by(rep) %>%                     
      dplyr::mutate(sumper=num/sum(num))%>%
      group_by_("rep",input$type)%>%
      dplyr::summarise(Per=sum(sumper)*100)
    
    
    #wt_class_gat_sum$rep<-sub("X","",wt_class_gat_sum$rep)
    bx_data_gat_sum$rep<-sub("\\d$","",bx_data_gat_sum$rep)
    bx_data_gat_sum$rep<-sub("\\.$","",bx_data_gat_sum$rep)
    
    
    
    gs <- c("early", "mid", "late", "stat")
    bx_data_gat_sum$groups <- ifelse(grepl("TX",bx_data_gat_sum$rep), "TX", 
                                     ifelse(grepl("4",bx_data_gat_sum$rep),"Temperature", 
                                            ifelse(grepl("13",bx_data_gat_sum$rep),"Temperature", 
                                                   ifelse(grepl("30",bx_data_gat_sum$rep),"Temperature",
                                                          ifelse(grepl("20",bx_data_gat_sum$rep),"Temperature",
                                                                 ifelse(grepl(paste(gs, collapse = "|"), bx_data_gat_sum$rep),"Growth stage",
                                                                        ifelse(grepl("Na",bx_data_gat_sum$rep), "NaCl",
                                                                               ifelse(grepl("Met",bx_data_gat_sum$rep), "MetOH",
                                                                                      "Other"))))))))
    if(input$type == "DB"){
      bx_data_gat_sum$DB <- factor(bx_data_gat_sum$DB, levels = c("1","2","3","4"))
    } else if(input$type == "length"){                                                                                
      bx_data_gat_sum$length <- factor(bx_data_gat_sum$length, levels = c("32","34","36","64","68", "70", "72"))
    } else {
      bx_data_gat_sum$WT <- factor(bx_data_gat_sum$WT, levels = c("PG","PE","PC","CL","DIPs"))
    }
    
    
    fill <- "#4271AE"
    lines <- "#1F3552"
    ggplot(bx_data_gat_sum, aes_string(x =input$type, y= "Per", col = "groups")) +
      stat_boxplot(geom ='errorbar') +
      geom_boxplot(colour = "grey", fill= "white")+
      geom_jitter(size = 0.4, position = position_dodge(width = 0.5))+
      theme(axis.text.x=element_text(angle=90,hjust=1), text = element_text(size=5)) +  
      theme_bw()  + 
      coord_fixed(ratio = 1/10) +
      labs(title = input$plotTitle3, x = input$xlab3, y = input$ylab3, color = " ")
    

    
     })
  
  
  output$MyPlot <- renderPlot({
    plot_object()
    
  })
  
  output$PCA <- renderPlot({
    PCA()
    
  })
  
  output$BoxPlot <- renderPlot({
    BoxPlot()
    
  })
  
  # When the save button is clicked, add the plot to a list and clear the input
  observeEvent(input$save_plot_btn, {
    plot_name <- trimws(input$save_plot_name)
    
    if (plot_name %in% names(values$plots)) {
      showModal(
        modalDialog(
          "You already have a plot saved with the same name. Saving this plot will override the existing plot.",
          footer = tagList(
            modalButton("Cancel"),
            actionButton("save_plot_duplicate_confirm", "OK",
                         class = "btn-primary")
          ),
          size = "m"
        )
      )
    } else {
      save_plot()
    }
  })
  
  observeEvent(input$save_plot_btn_pca, {
    plot_name <- trimws(input$save_plot_name_pca)
    
    if (plot_name %in% names(values$plots)) {
      showModal(
        modalDialog(
          "You already have a plot saved with the same name. Saving this plot will override the existing plot.",
          footer = tagList(
            modalButton("Cancel"),
            actionButton("save_plot_duplicate_confirm_pca", "OK",
                         class = "btn-primary")
          ),
          size = "m"
        )
      )
    } else {
      save_plot_pca()
    }
  })
  
  observeEvent(input$save_plot_duplicate_confirm, {
    save_plot()
    removeModal()
  })
  observeEvent(input$save_plot_duplicate_confirm_pca, {
    save_plot_pca()
    removeModal()
  })
  save_plot <- function() {
    shinyjs::show("save_plot_checkmark")
    values$plots[[trimws(input$save_plot_name)]] <- plot_object() 
    updateTextInput(session, "save_plot_name", value = "")
    shinyjs::delay(
      1000,
      shinyjs::hide("save_plot_checkmark", anim = TRUE, animType = "fade")
    )
  }
  
  # Disable the "save" button if the plot name input is empty
  observe({
    shinyjs::toggleState("save_plot_btn",
                         condition = nzchar(trimws(input$save_plot_name)))
    
  })
  save_plot_pca <- function() {
    shinyjs::show("save_plot_checkmark_pca")
    values$plots[[trimws(input$save_plot_name_pca)]] <- PCA()
    updateTextInput(session, "save_plot_name_pca", value = "")
    shinyjs::delay(
      1000,
      shinyjs::hide("save_plot_checkmark_pca", anim = TRUE, animType = "fade")
    )
  }
  
  # Disable the "save" button if the plot name input is empty
  observe({
    shinyjs::toggleState("save_plot_btn_pca",
                         condition = nzchar(trimws(input$save_plot_name)))
  })
  
  # ----------- Export tab -----------
  
  # Create a server variable that we can use in the UI for a conditionalPanel
  output$saved_plots_exist <- reactive({
    length(values$plots) > 0
  })
  outputOptions(output, 'saved_plots_exist', suspendWhenHidden = FALSE)
  
  countNoun <- function(num, noun) {
    if (num == 1) paste0(num, " ", noun)
    else paste0(num, " ", noun, "s")
  }
  
  output$export_btn_ui <- renderUI({
    btn_text <- paste0("Download ",
                       countNoun(length(input$plots_order), "plot"),
                       " (", countNoun(export_num_pages(), "page"), ")")
    downloadButton("export_btn", btn_text)
  })
  
  # Select the plots and the order of the plots to export
  output$plots_order_ui <- renderUI({
    selectizeInput("plots_order", "Plots to export (drag to reorder)",
                   choices = names(values$plots), selected = names(values$plots),
                   multiple = TRUE, options = list(
                     plugins = list('drag_drop','remove_button')))
  })
  
  # If no plots are chosen to export, don't show all the export options
  observe({
    shinyjs::toggle(selector = "#exporting_plots_options, #preview_plots_options",
                    condition = length(input$plots_order) > 0)
  })
  
  # Show a dropdown to select which page to show
  output$plots_select_page_ui <- renderUI({
    num_pages <- export_num_pages()
    
    # Try to remain on the same page even when the dropdown changes
    isolate({
      if (!is.null(input$plots_select_page) &&
          as.numeric(input$plots_select_page) <= num_pages) {
        selected <- input$plots_select_page
      } else {
        selected <- 1
      }
    })
    selectInput("plots_select_page", "Page to preview",
                choices = seq(num_pages), selected = selected)
  })
  
  # Calculate the number of pages to export
  export_num_pages <- reactive({
    if (input$export_multiple) {
      plots_per_page <- input$export_nrow * input$export_ncol
      pages <- ceiling(length(input$plots_order) / plots_per_page)
    } else {
      pages <- length(input$plots_order)
    }
    pages
  })
  
  # print a specific page of plots (either 1 plot/page or multiple rows/cols)
  export_print_page <- function(page) {
    page <- as.numeric(page)
    if (!input$export_multiple) {
      plot_name <- input$plots_order[page]
      values$plots[plot_name]
    } else {
      plots_per_page <- input$export_nrow * input$export_ncol
      idx_start <- (page - 1) * plots_per_page + 1
      idx_end <- min(length(input$plots_order), page * plots_per_page)
      if (idx_start > idx_end) {
        return()
      }
      plot_names <- input$plots_order[idx_start:idx_end]
      plots <- values$plots[plot_names]
      
      gridExtra::grid.arrange(
        grobs = plots,
        nrow = input$export_nrow,
        ncol = input$export_ncol,
        as.table = (input$export_arrangement == "byrow")
      )
    }
  }
  
  # Show a dropdown to select a plot to remove in the Export tab
  output$plots_remove_ui <- renderUI({
    selectInput("plots_remove", NULL, names(values$plots))
  })
  
  # Preview a plot in the Export tab
  output$plot_preview <- renderPlot({
    if (is.null(input$plots_select_page)) {
      return()
    }
    export_print_page(input$plots_select_page)
  },
  width = function() { plot_preview_width() },
  height = function() { plot_preview_height() })
  
  # Return the dimensions of the PDF page selected in the Export tab
  pdf_page_dim <- reactive({
    if (input$export_pdf_orientation == "landscape") {
      width <- 11
      height <- 8.5
    } else if (input$export_pdf_orientation == "portrait") {
      width <- 8.5
      height <- 11
    } else {
      width <- input$export_pdf_width
      height <- input$export_pdf_height
    }
    list(width = width, height = height)
  })
  
  # Calculate the dimensions of the plot preview
  plot_preview_dim <- reactive({
    # If it's PDF, the units are inches and default resolution is 72 px/inch
    if (input$export_file_type == "pdf") {
      width <- pdf_page_dim()$width * 72
      height <- pdf_page_dim()$height * 72
    } else {
      width <- input$export_file_width
      height <- input$export_file_height
    }
    
    # Keep the aspect ratio, but make the max dimensions 500
    ratio <- height/width
    if (ratio > 1) {
      height <- 500
      width <- height/ratio
    } else {
      width <- 500
      height <- ratio*width
    }
    
    list(width = width, height = height)
  })
  plot_preview_width <- reactive({
    plot_preview_dim()$width
  })
  plot_preview_height<- reactive({
    plot_preview_dim()$height
  })
  
  # Remove the currently selected plot from the saved plots list
  observeEvent(input$remove_plot_btn, {
    values$plots[[input$plots_remove]] <- NULL
  })
  
  # Determine the file name of the exported plots file.
  # If there's only one plot or using PDF, export it in its raw format.
  # Multiple plots in non-PDF format are zipped together.
  export_file_name <- reactive({
    if (export_num_pages() == 1 || input$export_file_type == "pdf") {
      paste0("export_plots", ".", input$export_file_type)
    } else {
      paste0("export_plots", ".zip")
    }
  })
  
  # Download the saved plots
  output$export_btn <- downloadHandler(
    filename = function() {
      export_file_name()
    },
    content = function(file) {
      tryCatch({
        file_type <- input$export_file_type
        
        # If saving as PDF, save all pages in one file
        if (file_type == "pdf") {
          width <- pdf_page_dim()$width
          height <- pdf_page_dim()$height
          
          file_names <- "export_plots.pdf"
          grDevices::pdf(file = file_names, width = width, height = height,
                         title = file_names, onefile = TRUE)
          
          if (!input$export_multiple) {
            plots <- values$plots[input$plots_order]
            invisible <- lapply(plots, print)
          } else {
            num_pages <- export_num_pages()
            for (page in seq(num_pages)) {
              suppressMessages(print(export_print_page(page)))
            }
          }
          
          grDevices::dev.off()
        }
        # If saving as raw images, save each page as a separate file
        else {
          num_pages <- export_num_pages()
          for (page in seq(num_pages)) {
            export_print_page(page)
          }
          file_names <- lapply(seq(num_pages), function(page) { 
            file_name <- paste0("export_plots_p", page, ".", file_type)
            export_params <- list(file_name,
                                  width = input$export_file_width,
                                  height = input$export_file_height)
            do.call(file_type, export_params)
            print(export_print_page(page))
            grDevices::dev.off()
            file_name
          })
          file_names <- unlist(file_names)
        }
        
        # If there's a single file, download the file. If multiple files, zip
        if (length(file_names) == 1) {
          file.copy(file_names, file, overwrite = TRUE)
        } else {
          zip(file, file_names)
        }
        
        # Remove the generated files so that we don't run out of disk space :)
        
        file.remove(file_names)
      },
      error = function(err) {
        stop(err$message)
      })
    }
  )
 
  
})
