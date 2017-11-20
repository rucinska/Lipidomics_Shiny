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
    updateSelectInput(session, "select", choices=colnames(data()), selected = c("class", "length", "DB"))
    
  })
  #observeEvent(filtereddata(), {
  #updateSelectInput(session, inputId = 'nonexp', label = 'Not experimanetal conditions',
   #                 choices = names(filtereddata()))
  
  #updateSelectInput(session, inputId = 'nonexp2', label = 'Not experimanetal conditions',
   #                     choices = names(filtereddata()))
  #updateSelectInput(session, inputId = 'xcol', label = 'X Variable',
  #                choices = names(filtereddata()))
  #})
  
  output$contents <- renderDataTable(filtereddata())

  plot_object <- reactive({
    gat <- gather(filtereddata(), rep, num, -one_of("class", "length", "DB") )
    #gat$rep<-sub("\\d$","",gat$rep)
    #x <- ggplot(gat, aes(x = rep, y = num)) + geom_point() + theme(axis.text.x=element_text(angle=90,hjust=1))
    # gat <- gather(filtereddata(), rep, num, gather_cols = input$ycol, -one_of(input$xcol) )
    
    gat$class <- factor(gat$class, levels=(unique(gat$class)))
    ggplot(gat, aes(x = class, y= num, col = "Lipid Abundance")) + 
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
  
  output$MyPlot <- renderPlot({
    plot_object()
    
  })
  
  PCA <- reactive({
    gat_pca <- gather(filtereddata(),  replicate, membr_perc, -one_of("class", "length", "DB") )
    gat_pca <- select(gat_pca,-c(length, DB))
 
    wt_spread <- spread(gat_pca, class, membr_perc)
    #rownames(lip_db_gather) <- lip_db_gather$con_type
    #lip_temp_gather<- lip_temp_gather%>% separate(con_type, into = c("type", "con"),sep =  "_")
    wt_spread$replicate <- make.names(wt_spread$replicate, unique=TRUE)
    rownames(wt_spread) <- wt_spread$replicate
    wt_spread <- select(wt_spread,-replicate)
    
    wt.rep <- rownames(wt_spread)
    wt.rep<-sub("^X","",wt.rep)
    wt.rep<-sub("\\d{1}$","",wt.rep)
    wt.rep<-sub("\\.$","",wt.rep) #remomve dot at the end
    wt.rep<- factor(wt.rep)
    
    prin_comp_wttwmp <- prcomp(wt_spread)
    
    ggbiplot(prin_comp_wttwmp, 
             obs.scale = 1, 
             var.scale = 1,
             groups = wt.rep ,
             var.axes = FALSE,
             ellipse = FALSE) +
      #label = wt.rep)
      #geom_point(aes( colour=wt.rep), size = 1) +
      geom_text_repel(label = wt.rep,
                   size = 2,
                   segment.color = 'grey50')+
      theme_bw()+  
      theme( legend.position = "none") +
      labs(title = input$plotTitle2, x = input$xlab2, y = input$ylab2, color = " ")
    
    

  })
  
  output$PCA <- renderPlot({
    PCA()
    
  })
  
  
  BoxPlot <- reactive({
    if (input$type == "DB") {
      bx_data <- filtereddata() %>% 
        filter(!(class %in% c("DIP","mDIP") | DB == '0' )) %>%
        select( -c(class,length))
      bx_data_gat <- bx_data %>% gather(rep, num, -one_of("DB"))
      
      
    } else if(input$type == "length") {
      bx_data <- filtereddata() %>% 
        filter(!(class %in% c("DIP","mDIP")| DB == '0' )) %>%
        select( -c(class,DB))
      bx_data_gat <- bx_data %>% gather(rep, num, -one_of("length"))
    } else {
      bx_data <- filtereddata() %>% select( -c(DB,length))
      bx_data<-bx_data%>%separate(class, into = c("class", "bal"),sep =  " ")
      bx_data <- bx_data %>% select(-bal)
      bx_data_gat <- bx_data %>% gather(rep, num, -one_of("class"))
      bx_data_gat$class <-  sub("^m", "", bx_data_gat$class )
      bx_data_gat$class <-  sub("DIP", "DIPs", bx_data_gat$class )
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
      bx_data_gat_sum$DB <- factor(bx_data_gat_sum$DB, levels = c(unique(bx_data_gat_sum$DB)))
    } else if(input$type == "length"){                                                                                
      bx_data_gat_sum$length <- factor(bx_data_gat_sum$length, levels = c(unique(bx_data_gat_sum$length)))
    } else {
      bx_data_gat_sum$class <- factor(bx_data_gat_sum$class, levels = c(unique(bx_data_gat_sum$class)))
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
      labs(title = input$plotTitle3, x = input$xlab3, y = input$ylab3, color = " ")+
      ylim(0,100)
     })
  
  output$BoxPlot <- renderPlot({
    BoxPlot()
    
  })
  
  ### SD
  SD <- reactive({
  sd_lipid <- filtereddata() %>% select( -c(DB,length))
  
  #SD for total
  sd_lipid <- sd_lipid %>% 
    select(-class) %>%
    rowwise() %>% 
    do(data.frame( sd_total = sd(unlist(.)))) %>% 
    bind_cols(sd_lipid, .)
  
  #SD for TX
  sd_lipid <- sd_lipid %>% 
    select(starts_with("TX")) %>%
    rowwise() %>% 
    do(data.frame(sd_TX = sd(unlist(.)))) %>% 
    bind_cols(sd_lipid, .)
  
  #SD for gs
  sd_lipid <- sd_lipid %>% 
    select(starts_with("early"),starts_with("mid"),starts_with("late"),starts_with("stat")) %>%
    rowwise() %>% 
    do(data.frame(sd_gs = sd(unlist(.)))) %>% 
    bind_cols(sd_lipid, .)
  
  #SD for temp
  sd_lipid <- sd_lipid %>% 
    select(starts_with("13"),starts_with("20"), starts_with("early"),starts_with("4") )%>%
    rowwise() %>% 
    do(data.frame(sd_temp = sd(unlist(.))))%>% 
    bind_cols(sd_lipid, .)
  
  #SD for NaCl
  sd_lipid <- sd_lipid %>% 
    select(starts_with("Na")) %>%
    rowwise() %>% 
    do(data.frame(sd_NaCl = sd(unlist(.)))) %>% 
    bind_cols(sd_lipid, .)
  
  #SD for MetOh
  sd_lipid <- sd_lipid %>% 
    select(starts_with("Met")) %>%
    rowwise() %>% 
    do(data.frame(sd_Met = sd(unlist(.)))) %>% 
    bind_cols(sd_lipid, .)
  
  sd_all <- sd_lipid %>% 
    select(starts_with("sd"), class)
  
  sd_all_gat <- sd_all %>% gather(sd, mean, -one_of("class")) %>% tbl_df()
  
  if(input$conditions == "gs") {
    sd_all <- sd_all %>% group_by(class) %>%
      mutate(mean_gs = mean(sd_gs))
    
    
    
     ggplot(sd_all, aes(x =reorder(class, sd_gs), y= sd_gs)) + 
      geom_point(aes(colour = cut(sd_gs, c(-Inf, mean_gs[1], Inf))),
                 size = 1) +
      coord_flip(ylim = c(0,input$obs)) +
      geom_hline(aes(yintercept = mean_gs,linetype = "Mean")) +
      scale_linetype_manual(name = " ", values = 1 )  +
      
      theme_bw()+
      theme(axis.text.x = element_text(angle = 0, hjust = 1)
      )+
      labs(title = input$plotTitle4, x = input$xlab4, y = input$ylab4, color = " ") +
      scale_color_manual(name = " ",
                         values = c( "black",
                                     "red"),
                         labels = c("below mean", "over mean"))
  } else if(input$conditions == "sd_total"){
    sd_all <- sd_all %>% group_by(class) %>%
      mutate(sd_tot = mean(sd_total))
    #order variables base on number
    sd_all <- sd_all[with(sd_all, order(sd_total)), ]
    
     ggplot(sd_all, aes(x =reorder(class, sd_total), y= sd_total)) + 
      geom_point(aes(colour = cut(sd_total, c(-Inf, sd_tot[1], Inf))),
                 size = 1) +
      coord_flip(ylim = c(0,input$obs)) +
      geom_hline(aes(yintercept = sd_tot,linetype = "Mean")) +
      scale_linetype_manual(name = " ", values = 1 )  +
      
      theme_bw()+
      theme(axis.text.x = element_text(angle = 0, hjust = 1)
      )+
       labs(title = input$plotTitle4, x = input$xlab4, y = input$ylab4, color = " ") +
      scale_color_manual(name = " ",
                         values = c( "black",
                                     "red"),
                         labels = c("below mean", "over mean"))
  }else if(input$conditions == "temp"){
    sd_all <- sd_all %>% group_by(class) %>%
      mutate(mean_temp = mean(sd_temp))
    
   ggplot(sd_all, aes(x =reorder(class, sd_temp), y= sd_temp)) + 
      geom_point(aes(colour = cut(sd_temp, c(-Inf, mean_temp[1], Inf))),
                 size = 1) +
      coord_flip(ylim = c(0,input$obs)) +
      geom_hline(aes(yintercept = mean_temp,linetype = "Mean")) +
      scale_linetype_manual(name = " ", values = 1 )  +
      
      theme_bw()+
      theme(axis.text.x = element_text(angle = 0, hjust = 1)
      )+
      labs(title = input$plotTitle4, x = input$xlab4, y = input$ylab4, color = " ") +
      scale_color_manual(name = " ",
                         values = c( "black",
                                     "red"),
                         labels = c("below mean", "over mean"))
  }else if(input$conditions == "salt"){
    sd_all <- sd_all %>% group_by(class) %>%
      mutate(mean_NaCl = mean(sd_NaCl))

     ggplot(sd_all, aes(x =reorder(class, sd_NaCl), y= sd_NaCl)) + 
      geom_point(aes(colour = cut(sd_NaCl, c(-Inf, mean_NaCl[1], Inf))),
                 size = 1) +
      coord_flip(ylim = c(0,input$obs)) +
      geom_hline(aes(yintercept = mean_NaCl,linetype = "Mean")) +
      scale_linetype_manual(name = " ", values = 1 )  +
      
      theme_bw()+
      theme(axis.text.x = element_text(angle = 0, hjust = 1)
      )+
      labs(title = input$plotTitle4, x = input$xlab4, y = input$ylab4, color = " ") +
      scale_color_manual(name = " ",
                         values = c( "black",
                                     "red"),
                         labels = c("below mean", "over mean"))
  }else if(input$conditions == "met"){
    sd_all <- sd_all %>% group_by(class) %>%
      mutate(mean_Met = mean(sd_Met))
     
    ggplot(sd_all, aes(x =reorder(class, sd_Met), y= sd_Met)) + 
      geom_point(aes(colour = cut(sd_Met, c(-Inf, mean_Met[1], Inf))),
                 size = 1)+
      coord_flip(ylim = c(0,input$obs)) +
      geom_hline(aes(yintercept = mean_Met,linetype = "Mean")) +
      scale_linetype_manual(name = " ", values = 1 )  +
      
      theme_bw()+
      theme(axis.text.x = element_text(angle = 0, hjust = 1)
      )+
      labs(title = input$plotTitle4, x = input$xlab4, y = input$ylab4, color = " ") +
      scale_color_manual(name = " ",
                         values = c( "black",
                                     "red"),
                         labels = c("below mean", "over mean"))
  }else if(input$conditions == "tri"){
    sd_all <- sd_all %>% group_by(class) %>%
      mutate(mean_TX = mean(sd_TX))
    
    ggplot(sd_all, aes(x = reorder(class, sd_TX), y= sd_TX)) + 
      geom_point(aes(colour = cut(sd_TX, c(-Inf, mean_TX[1], Inf))),
                 size = 1) +
    coord_flip(ylim = c(0,input$obs)) +
      geom_hline(aes(yintercept = mean_TX,linetype = "Mean")) +
      scale_linetype_manual(name = " ", values = 1 )  +
      
      theme_bw()+
      theme(axis.text.x = element_text(angle = 0, hjust = 1)
      )+
      labs(title = input$plotTitle4, x = input$xlab4, y = input$ylab4, color = " ") +
      scale_color_manual(name = " ",
                         values = c( "black",
                                     "red"),
                         labels = c("below mean", "over mean"))
  } else {
      mean_sd <-sd_lipid %>% 
        select(starts_with("sd"), class) %>%
        group_by(class) %>% 
        summarise_all(funs(mean(.) ))
      names(mean_sd)[names(mean_sd) == "sd_TX"] <- "Triton"
      names(mean_sd)[names(mean_sd) == "sd_gs"] <- "Growth Stage"
      names(mean_sd)[names(mean_sd) == "sd_temp"] <- "Temperature"
      names(mean_sd)[names(mean_sd) == "sd_NaCl"] <- "Salt"
      names(mean_sd)[names(mean_sd) == "sd_Met"] <- "Methanol"
      names(mean_sd)[names(mean_sd) == "sd_total"] <- "Total"
      
      mean_sd_gat <- mean_sd %>% gather(sd, mean, -one_of("class"))
      
      ggplot(mean_sd_gat, aes(x = sd, y= mean)) + 
        geom_bar(stat = "identity", position = "dodge") +
        theme(axis.text.x=element_text(size=5, angle=90)) +
        theme_bw()+
        labs(title = input$plotTitle4, x = input$xlab4, y = input$ylab4, color = " ")

    }
  
  
  })
  
  
  output$SD <- renderPlot({
    SD()
    
  })
  
  Detail <- reactive({
    #if(is.null(input$hg)){
      if (input$feat == "Double Bonds") {
        temp_DB<-filtered_data()
        if(!input$hg){
          temp_DB <- select(temp_DB, -c(length, class))
          temp_DB_gat <- temp_DB %>% gather(rep, num, -one_of("DB"))
          temp_DB_gat$rep<-sub("\\d$","",temp_DB_gat$rep)
          temp_DB_gat$rep<-sub("\\.$","",temp_DB_gat$rep)
          
          temp_DB_gat <- temp_DB_gat %>% filter()
          #wt_DB_gat_sum <-wt_DB_gat %>% group_by(rep, DB) %>% summarise_all(funs(sum(.)))
         
           temp_DB_gat_sum <-
            temp_DB_gat%>%
            group_by(rep) %>%                     
            dplyr::mutate(sumper=num/sum(num))%>%group_by(rep,DB)%>%
            dplyr::summarise(Per =sum(sumper)*100, sd = sd(sumper))
          
          temp_DB_gat_sum$DB <- factor(temp_DB_gat_sum$DB, levels = c( unique(temp_DB_gat_sum$DB) ) )

          ggplot(temp_DB_gat_sum, aes(x = rep, y = Per)) + 
            geom_point(aes(colour = DB), size = 5) +
            geom_line(aes(colour = DB, group = DB)) +
            geom_errorbar(aes(ymin=Per-sd, ymax=Per+sd), width=.01) +
            theme_bw()+
            #theme(legend.position = "none")+
            coord_equal(ratio =1/20)+
            ylim(0,100) 
      }
          else {
            temp_DB <- select(temp_DB, -c(length))
            temp_DB_gat <- temp_DB %>% gather(rep, num, -one_of("DB","class"))
            temp_DB_gat$rep<-sub("\\d$","",temp_DB_gat$rep)
            temp_DB_gat$rep<-sub("\\.$","",temp_DB_gat$rep)
            
            temp_byclass_gat_sum <- temp_DB_gat %>%
              group_by(rep,class) %>%                     
              dplyr::mutate(sumper=num/sum(num))%>%
              group_by(rep,class,DB)%>%
              dplyr::summarise(Per=sum(sumper)*100, sd=sd(sumper))
            
            temp_byclass_gat_sum$DB <- factor(temp_byclass_gat_sum$DB, levels = c( unique(temp_byclass_gat_sum$DB) ) )
            temp_byclass_gat_sum$class <- factor(temp_byclass_gat_sum$class, levels = c( unique(temp_byclass_gat_sum$class) ) )
            
            ggplot(temp_byclass_gat_sum, aes(x = rep, y = Per), fill = "gray") + 
              geom_point(aes(colour = DB), size = 5) +
              geom_line(aes(colour = DB, group = DB)) +
              facet_grid(. ~ class) +
              geom_errorbar(aes(ymin=Per-sd, ymax=Per+sd), width=.01) +
              theme_bw() + 
              coord_equal(ratio =1/20) +
              ylim(0,100) 
          }
        }else if(input$feat == "Length") {
          
          temp_LEN<-filtered_data()
          if(!input$hg){
              temp_LEN <- select(temp_LEN, -class, -DB)
              temp_len_gat <- temp_LEN %>% gather(rep, num, -one_of("length"))
              temp_len_gat$rep<-sub("\\d$","",temp_len_gat$rep)
              temp_len_gat$rep<-sub("\\.$","",temp_len_gat$rep)
              temp_len_gat <- temp_len_gat %>% filter()
              #wt_DB_gat_sum <-wt_DB_gat %>% group_by(rep, DB) %>% summarise_all(funs(sum(.)))
              temp_len_gat_sum <-
                temp_len_gat%>%
                group_by(rep) %>%                     
                dplyr::mutate(sumper=num/sum(num))%>%group_by(rep,length)%>%
                dplyr::summarise(Per =sum(sumper)*100, sd = sd(sumper))
              
              temp_len_gat_sum$length <- factor(temp_len_gat_sum$length, levels = c(unique(temp_len_gat_sum$length)) )
              
              ggplot(temp_len_gat_sum, aes(x = rep, y = Per)) + 
                geom_point(aes(color = length), size = 5) +
                geom_line(aes(colour = length, group = length)) +
                #geom_dl(aes(label=length),method="last.points")
                #geom_text_repel(data = last_text, aes(x=rep, y = Per, label = length), hjust = 0, vjust = 0.35)+
                geom_errorbar(aes(ymin=Per-sd, ymax=Per+sd), width=.01) +
                theme_bw()+
                #theme(legend.position = "none")+
                coord_equal(ratio =1/20)+
                labs(title = "LENGTH - only temp") +
                ylim(0,100)}
        else {
                temp_LEN <- select(temp_LEN,  -DB)
                temp_len_gat <- temp_LEN %>% gather(rep, num, -one_of("length","class"))
                temp_len_gat$rep<-sub("\\d$","",temp_len_gat$rep)
                temp_len_gat$rep<-sub("\\.$","",temp_len_gat$rep)
                
                LEN_byclass_gat_sum <- temp_len_gat %>%
                  group_by(rep,class) %>%                     
                  dplyr::mutate(sumper=num/sum(num))%>%
                  group_by(rep,class,length)%>%
                  dplyr::summarise(Per=sum(sumper)*100, sd=sd(sumper))
                
                LEN_byclass_gat_sum$length <- factor(LEN_byclass_gat_sum$length, levels = c(unique(LEN_byclass_gat_sum$length)) )
                LEN_byclass_gat_sum$class <- factor(LEN_byclass_gat_sum$class, levels = c(unique(LEN_byclass_gat_sum$class)) )
                
                ggplot(LEN_byclass_gat_sum, aes(x = rep, y = Per)) + 
                  geom_point(aes(colour = length), size = 5) +
                  geom_line(aes(colour = length, group = length)) +
                  facet_grid(. ~ class) +
                  geom_errorbar(aes(ymin=Per-sd, ymax=Per+sd), width=.01) +
                  theme_bw() + 
                  coord_equal(ratio =1/20) +
                  ylim(0,100)
              }
          } else {
   
                    temp_byclass<-filtered_data()
                    temp_byclass<-temp_byclass%>%separate(class, into = c("class", "bal"),sep =  " ")
                    temp_byclass <- temp_byclass %>% select(-bal, - length, -DB)
                    temp_byclass$class <-  sub("^m", "", temp_byclass$class )
                    temp_byclass$class <-  sub("DIP", "DIPs", temp_byclass$class )
                    temp_byclass_gat <- temp_byclass %>% gather(rep, num, -one_of("class"))
                    temp_byclass_gat$rep<-sub("\\d$","",temp_byclass_gat$rep)
                    temp_byclass_gat$rep<-sub("\\.$","",temp_byclass_gat$rep)
                    temp_byclass_gat <- temp_byclass_gat %>% filter()
                    #wt_DB_gat_sum <-wt_DB_gat %>% group_by(rep, DB) %>% summarise_all(funs(sum(.)))
                    temp_byclass_gat_sum <-
                      temp_byclass_gat%>%
                      group_by(rep) %>%                     
                      dplyr::mutate(sumper=num/sum(num))%>%group_by(rep,class)%>%
                      dplyr::summarise(Per =sum(sumper)*100, sd = sd(sumper))
                    
                    temp_byclass_gat_sum$class <- factor(temp_byclass_gat_sum$class, levels = c(unique(temp_byclass_gat_sum$class)) )
                    
 
                    p<- ggplot(temp_byclass_gat_sum, aes(x = rep, y = Per)) + 
                      geom_point(aes(color = class), size = 5) +
                      geom_line(aes(colour = class, group = class)) +
                      geom_errorbar(aes(ymin=Per-sd, ymax=Per+sd), width=.01) +
                      theme_bw()+
                      #theme(legend.position = "none")+
                      coord_equal(ratio =1/20)+
                      ylim(0,100)
                    if(input$hg){
                      p <- p + facet_grid(. ~ class)
                    }
                    
                    p
          }
          
    
 
  })
  observeEvent(input$reset_input, {
    shinyjs::reset("side-panel")
    shinyjs::reset("feat")
    
  })
  observeEvent(input$feat, {
    updateSelectInput(session, "feat_gh", choices=unique(filter_class()$class), selected = input$feat_gh)
    updateSelectInput(session, "feat_db", choices=unique(filtered_data()$DB), selected = input$feat_db )
    updateSelectInput(session, "feat_len", choices=unique(filtered_data()$length), selected = input$feat_len)
    #updateSelectInput(session, "feat_len", choices=unique(filtereddata()$length[filtereddata()$DB==input$feat_db]),selected=unique(filtereddata()$length[filtereddata()$DB%in%input$feat_db]))
    #updateSelectInput(session, "feat_gh", choices=unique(filtereddata()$class[filtereddata()$DB==input$feat_db]), selected=unique(filtereddata()$class[filtereddata()$DB%in%input$feat_db]))
    
  })
 
  observeEvent(data(), {
    updateSelectInput(session, "feat_gh", choices=unique(filter_class()$class), selected = unique(filter_class()$class))
    updateSelectInput(session, "feat_db", choices=unique(filtereddata()$DB), selected =unique(filtereddata()$DB)  )
    updateSelectInput(session, "feat_len", choices=unique(filtereddata()$length), selected =unique(filtereddata()$length) )
    #updateSelectInput(session, "feat_db", choices=unique(filtereddata()$DB[filtereddata()$class %in% input$feat_gh]), selected=unique(filtereddata()$DB[filtereddata()$class==input$feat_gh]))
    #updateSelectInput(session, "feat_len", choices=unique(filtereddata()$length[filtereddata()$class%in%input$feat_gh]),selected=unique( filtereddata()[is.element(filtereddata()$class, feat_gh),]$length))
  })
  observeEvent(input$reset_input, {
    updateSelectInput(session, "feat_gh", choices=unique(filter_class()$class), selected = unique(filter_class()$class))
    updateSelectInput(session, "feat_db", choices=unique(filtereddata()$DB), selected =unique(filtereddata()$DB)  )
    updateSelectInput(session, "feat_len", choices=unique(filtereddata()$length), selected =unique(filtereddata()$length) )
    #updateSelectInput(session, "feat_db", choices=unique(filtereddata()$DB[filtereddata()$class %in% input$feat_gh]), selected=unique(filtereddata()$DB[filtereddata()$class==input$feat_gh]))
    #updateSelectInput(session, "feat_len", choices=unique(filtereddata()$length[filtereddata()$class%in%input$feat_gh]),selected=unique( filtereddata()[is.element(filtereddata()$class, feat_gh),]$length))
  })
  
  #try to make it seperately - if feat_hg selected do stufff blabal
  filter_class <- reactive({
    filtereddata() %>%
      mutate(class = str_replace(class, "m","")) %>%
      mutate(class = str_replace(class, "\\<DIP\\>", "DIPs")) %>%
      separate(class, into = c("class", "bal"),sep =  " ") %>% 
      select(-bal)
    
  }) 

  filtered_data <- eventReactive({
    input$update_input
  },  {
    
    if(is.null(input$feat_gh)  && is.null(input$feat_db) && is.null(input$feat_len))
    { filtereddata()
      }else {
          #if (!is.null(input$feat_gh)) {
                filter_class() %>% filter(class %in% input$feat_gh,DB %in% input$feat_db, length %in% input$feat_len)
        #}else if(!is.null(input$feat_db)){ 
              #filter(filtereddata(), DB %in% input$feat_db, length %in% input$feat_len)
          
        #} else if(!is.null(input$feat_len)){
         #     filter(filtereddata(), length %in% input$feat_len)}

      }
    #https://stackoverflow.com/questions/30001211/filter-data-frame-for-use-in-multiple-renderplot-functions 
    })
  output$Detail <- renderPlot({
    Detail()
    
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
  
  observeEvent(input$save_plot_btn_boxplot, {
    plot_name <- trimws(input$save_plot_name_boxplot)
    
    if (plot_name %in% names(values$plots)) {
      showModal(
        modalDialog(
          "You already have a plot saved with the same name. Saving this plot will override the existing plot.",
          footer = tagList(
            modalButton("Cancel"),
            actionButton("save_plot_duplicate_confirm_boxplot", "OK",
                         class = "btn-primary")
          ),
          size = "m"
        )
      )
    } else {
      save_plot_boxplot()
    }
  })
  
  observeEvent(input$save_plot_btn_sd, {
    plot_name <- trimws(input$save_plot_name_sd)
    
    if (plot_name %in% names(values$plots)) {
      showModal(
        modalDialog(
          "You already have a plot saved with the same name. Saving this plot will override the existing plot.",
          footer = tagList(
            modalButton("Cancel"),
            actionButton("save_plot_duplicate_confirm_boxplot", "OK",
                         class = "btn-primary")
          ),
          size = "m"
        )
      )
    } else {
      save_plot_sd()
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
  observeEvent(input$save_plot_duplicate_confirm_boxplot, {
    save_plot_boxplot()
    removeModal()
  })
  observeEvent(input$save_plot_duplicate_confirm_boxplot, {
    save_plot_sd()
    removeModal()
  })
  
  ### LIPID ABUNDANCE
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
  ### PCA
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
  ### BOX PLOT
  save_plot_boxplot <- function() {
    shinyjs::show("save_plot_checkmark_boxplot")
    values$plots[[trimws(input$save_plot_name_boxplot)]] <- BoxPlot()
    updateTextInput(session, "save_plot_name_boxplot", value = "")
    shinyjs::delay(
      1000,
      shinyjs::hide("save_plot_checkmark_boxplot", anim = TRUE, animType = "fade")
    )
  }
  
  # Disable the "save" button if the plot name input is empty
  observe({
    shinyjs::toggleState("save_plot_btn_boxplot",
                         condition = nzchar(trimws(input$save_plot_name)))
  })
  ### SD
  save_plot_sd <- function() {
    shinyjs::show("save_plot_checkmark_sd")
    values$plots[[trimws(input$save_plot_name_sd)]] <- SD()
    updateTextInput(session, "save_plot_name_sd", value = "")
    shinyjs::delay(
      1000,
      shinyjs::hide("save_plot_checkmark_sd", anim = TRUE, animType = "fade")
    )
  }
  
  # Disable the "save" button if the plot name input is empty
  observe({
    shinyjs::toggleState("save_plot_btn_sd",
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
