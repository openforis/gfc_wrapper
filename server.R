####################################################################################
####### GFC WRAPPER
####### SEPAL shiny application
####### FAO Open Foris SEPAL project
####### remi.dannunzio@fao.org
####################################################################################

####################################################################################
# FAO declines all responsibility for errors or deficiencies in the database or
# software or in the documentation accompanying it, for program maintenance and
# upgrading as well as for any # damage that may arise from them. FAO also declines
# any responsibility for updating the data and assumes no responsibility for errors
# and omissions in the data provided. Users are, however, kindly asked to report any
# errors or deficiencies in this product to FAO.
####################################################################################

####################################################################################
## Last update: 2020/03/12
## gfc-wrapper / server
####################################################################################


####################################################################################
####### Start Server

shinyServer(function(input, output, session) {
  ####################################################################################
  ##################### Choose language option             ###########################
  ####################################################################################
  output$chosen_language <- renderPrint({
    if (input$language == "English") {
      source("scripts/text_english.R",
             local = TRUE,
             encoding = "UTF-8")
      #print("en")
    }
    if (input$language == "") {
      source("scripts/text_english.R", 
             local = TRUE, 
             encoding = "UTF-8")
      #print("fr")
    }
    
  })
  
  ##################################################################################################################################
  ############### Stop session when browser is exited
  
  session$onSessionEnded(stopApp)
  
  ##################################################################################################################################
  ############### Show progress bar while loading everything
  options(echo = T)
  progress <- shiny::Progress$new()
  progress$set(message = "Loading data", value = 0)
  
  ####################################################################################
  ####### Step 0 : read the map file and store aoi_message    ###########################
  ####################################################################################
  
  ##################################################################################################################################
  ############### Find volumes
  osSystem <- Sys.info()["sysname"]
  
  volumes <- list()
  media <- list.files("/media", full.names = T)
  names(media) = basename(media)
  volumes <- c(media)
  aoi_vol <- setNames(paste0(normalizePath("~"),"/gfc_wrapper/data/aoi/"),"AOI")
  
  volumes <- c(aoi_vol,
               'Home' = Sys.getenv("HOME"),
               volumes
               )
  
  my_zip_tools <- Sys.getenv("R_ZIPCMD", "zip")
  
  
  ##################################################################################################################################
  ############### GET A REACTIVE VALUE
  v <- reactiveValues(threshold = FALSE,
                      country   = FALSE)
  
  
  ##################################################################################################################################
  ############### Insert the Customized AOI button
  output$aoi_select_custom <- renderUI({
    req(input$aoi_type == "custom")
    
    shinyFilesButton(id = 'aoi_custom_file',
                     label =   "Area of interest", #TO TRY TO IMPLEMENT
                     title = "Browse", #htmlOutput('select_a_file'),
                     multiple = FALSE)
    
    
    
  })
  
  
  ##################################################################################################################################
  ############### Select input file 
  shinyFileChoose(
    input,
    'aoi_custom_file',
    filetype = c(
      'shp',
      'sqlite',
      'gdb',
      'kml'
    ),
    roots = volumes,
    session = session,
    restrictions = system.file(package = 'base')
  )
  
  
  ##################################################################################################################################
  ############### Insert the GADM AOI button
  output$aoi_select_gadm <- renderUI({
    req(input$aoi_type == "gadm")
    
    selectizeInput(
      'country_code',
      textOutput('text_choice_country'),
      choices = setNames(getData('ISO3')[,1],
                         getData('ISO3')[,2]),
      options = list(
        placeholder = '',#Please select a country from the list below',#htmlOutput('t6_b2_button1_field'),
        onInitialize = I('function() { this.setValue(""); }')
      )
    )
  })
  
  ##################################################################################################################################
  ############### Insert the leaflet
  output$leafmap <- renderLeaflet({
    #validate(need(the_basename(), "define the area of interest"))
    req(input$aoi_type == "draw")
    
    leaflet() %>%
      setView(0,0,2) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addDrawToolbar(#editOptions = editToolbarOptions(),
        circleOptions = F,
        polylineOptions = F,
        markerOptions = F,
        circleMarkerOptions = F,
        singleFeature = T)
  })
  
    # Listen for draw_all_features which is called anytime features are created/edited/deleted from the map
  observeEvent(input$leafmap_draw_all_features, {
    print("All Features")
    print(input$leafmap_draw_all_features)
    
  })
  
  ##################################################################################################################################
  ############### Store the drawn geometry as reactive
  drawn <- reactive({
    validate(need(input$leafmap_draw_all_features, "Draw area of interest"))
    list <- input$leafmap_draw_all_features
  })
  
  ##################################################################################################################################
  ############### Spatialize the drawn feature
  drawn_geom <- reactive({
    validate(need(input$leafmap_draw_all_features, "Draw area of interest"))
    list <- input$leafmap_draw_all_features
    
    poly_type   <- list$features[[1]]$properties$feature_type
    nb_vertices <- length(list$features[[1]]$geometry$coordinates[[1]])
    
    lp <- list()
    
    poly <- Polygons(list(Polygon(cbind(
      sapply(1:nb_vertices,function(x){list$features[[1]]$geometry$coordinates[[1]][[x]][[1]]}),
      sapply(1:nb_vertices,function(x){list$features[[1]]$geometry$coordinates[[1]][[x]][[2]]})
    )
    )),
    list$features[[1]]$properties$`_leaflet_id`)
    lp <- append(lp,list(poly))
    
    ## Transform the list into a SPDF 
    spdf <- SpatialPolygonsDataFrame(
      SpatialPolygons(lp,1:length(lp)),
      data.frame(list$features[[1]]$properties$`_leaflet_id`),
      match.ID = F
    )
    proj4string(spdf) <- CRS("+init=epsg:4326")
    
    spdf
  })
  
  
  ##################################################################################################################################
  ############### Plot the geometry (check only)
  output$leafmap_drawn <- renderPlot({
    req(input$leafmap_draw_all_features)
    plot(drawn_geom())
  })
  
  
  ##################################################################################################################################
  ############### Print the geometry type (check)
  output$leafmap_message <- renderTable({
    req(drawn())
    list <- drawn()
    poly_type   <- list$features[[1]]$properties$feature_type
    print(poly_type)
  }
  )
  
  ################################# Display the file info
  aoi_info <- reactive({
    req(input$aoi_type)
    
    if(input$aoi_type == "custom"){
      validate(need(input$aoi_custom_file, "Missing input: select the AOI file"))
      df <- parseFilePaths(volumes, input$aoi_custom_file)
      file_info <- as.character(df[, "datapath"])
      nofile <- as.character("No file selected")
      if (is.null(file_info)) {
        cat(nofile)
      } else{
        cat(file_info)
      }
      
    }
    if(input$aoi_type == "gadm"){
      file_info <- input$country_code
    }
    
    if(input$aoi_type == "draw"){
      file_info   <- paste0(drawn()$features[[1]]$properties$feature_type," ",
                            drawn()$features[[1]]$properties$`_leaflet_id`)
    }
    
    file_info
    
  })
  
  ##################################################################################################################################
  ############### DISPLAY THE AOI FILE PATH
  output$aoi_message_ui <- renderText({
    req(input$aoi_type)
    aoi_info()
  })
  
  
  ##################################################################################################################################
  ############### Insert the START button
  output$ProcessButton <- renderUI({
    req(input$aoi_type)
    validate(need(the_basename(), "Define the area of interest"))
    actionButton('ProcessButton', textOutput('process_button'))
  })
  
  
  ##################################################################################################################################
  ############### Insert the DISPLAY MAP button
  output$DisplayMapButton <- renderUI({
    validate(need(the_basename(), "Define the area of interest"))
    req(input$aoi_type)
    actionButton('DisplayMapButton', textOutput('display_map_button'))
  })
  
  
  ##################################################################################################################################
  ############### Insert the STATISTICS button
  output$runGFCButton <- renderUI({
    validate(need(the_basename(), "Define the area of interest"))
    req(input$aoi_type)
    actionButton('runGFC', textOutput('gfc_start_button'))
  })
  
  
  ##################################################################################################################################
  ############### Text to explain
  output$explainGFCrun <- renderUI({
    validate(need(the_basename(), "Define the area of interest"))
    req(input$aoi_type)
    textOutput('gfc_explanation')
  })
  
  ##################################################################################################################################
  ############### GET THE BASENAME
  the_basename <- reactive({
    req(input$aoi_type)
    
    if(input$aoi_type == "custom"){
      aoi_file_path  <- aoi_info()
      base           <- basename(as.character(aoi_file_path))
      the_basename   <- paste0(substr(base,1,nchar(base)-4))
    }
    
    if(input$aoi_type == "gadm"){
      the_basename   <- paste0("aoi_",input$country_code)
    }
    
    if(input$aoi_type == "draw"){
      list <- drawn()
      the_basename <- paste0("aoi_manual_",unlist(list$features[[1]]$properties$`_leaflet_id`))
    }
    
    the_basename
  })
  
  
  ##################################################################################################################################
  ############### Make the AOI reactive
  make_aoi <- reactive({
    req(input$aoi_type)
    req(the_basename())
    validate(need(the_basename(), "Define the area of interest"))
    the_basename <- the_basename()
    
    if(input$aoi_type == "custom"){
      aoi_file_path  <- aoi_info()
      source("scripts/b0_custom_aoi_app.R",  local=T, echo = TRUE)
    }
    
    if(input$aoi_type == "gadm"){
      countrycode <- as.character(input$country_code)
      source("scripts/b0_gadm_aoi_app.R",  local=T, echo = TRUE)
    }
    
    if(input$aoi_type == "draw"){
      drawn_aoi   <- drawn_geom()
      source("scripts/b0_draw_aoi_app.R",  local=T, echo = TRUE)
    }
    
    v$country <- aoi_shp
    
  })
  
  
  ##################################################################################################################################
  ############### PROCESSING CHAIN
  process <- eventReactive(input$runGFC,
                           {
                             req(input$runGFC)
                             req(input$aoi_type)
                             validate(need(the_basename(), "Define the area of interest"))
                             
                             threshold    <- input$threshold
                             the_basename <- the_basename()
                             #progress_file <- progress_file()
                             
                             aoi_name   <- paste0(aoi_dir,the_basename)
                             aoi_shp    <- paste0(aoi_name,".shp")
                             aoi_field <-  "id_aoi"
                             
                             aoi <- readOGR(make_aoi())
                             (bb    <- extent(aoi))
                             
                             #system(paste0('echo "Preparing data..." > ', progress_file))
                             
                             
                             withProgress(message = paste0('Download GFC data'),
                                          value = 0,
                                          {if(!file.exists(paste0(gfc_dir,"gfc_",the_basename,"_",types[4],".tif"))){
                                            source("scripts/b1_download_merge.R",  local=T, echo = TRUE)
                                            incProgress(.25)
                                          }
                                          })
                             
                             
                             
                             withProgress(message = paste0('Combine layers into a map'),
                                          value = 0,{
                                            if(!file.exists(paste0(gfc_dir,"gfc_",the_basename,"_",threshold,"_map_clip_pct.tif"))){
                                              source("scripts/b2_make_map_threshold.R",  local=T,echo = TRUE)
                                              setProgress(.25)
                                              incProgress(.25)
                                            }
                                          })
                             
                             
                             withProgress(message = paste0('Compute statistics'),
                                          value = 0,{
                                            if(!file.exists(paste0(stt_dir,"stats_",the_basename,"_",threshold,".txt"))){
                                              source("scripts/b3_compute_areas.R",  local=T,echo = TRUE)
                                              setProgress(.5)
                                              incProgress(.25)
                                            }
                                          })
                             
                             
                             withProgress(message = paste0('Generate fragmentation mask'),
                                          value = 0,{
                                            if(!file.exists(paste0(gfc_dir,"mask_mspa_gfc_",the_basename,"_",threshold,".tif"))){
                                              source("scripts/b4_make_mspa_ready_mask.R",  local=T,echo = TRUE)
                                              setProgress(.75)
                                              incProgress(.25)
                                            }
                                          })
                             
                             list.files(gfc_dir)
                           })
  
  ############# Define the forest mask as a reactive
  fmask <- reactive({
    req(process())
    paste0(gfc_dir,"gfc_",the_basename(),"_",input$threshold,"_map_clip_pct.tif")
    })
  
  
  ############# Define the forest mask as a reactive
  fmask_geo <- reactive({
    req(process())
    paste0(gfc_dir,"gfc_",the_basename(),"_",input$threshold,"_map_clip_pct_geo.tif")
  })
  
  ############# Create the raw statistics table
  raw_stats <- reactive({
    req(input$runGFC)
    req(process())
    threshold   <- input$threshold
    the_basename <- the_basename()
    
    print('Check: Display the stats')
    
    t <- read.table(paste0(stt_dir,"stats_",the_basename,"_",threshold,".txt"))
    
    names(t) <- c("class","pixel","area")
    
    t
  })
  
  
  ############# Create the statistics table
  stats <- reactive({
    req(raw_stats())
    
    t <- raw_stats()
    
    tt <- t[t$class > 20,]
    tt <- rbind(tt,colSums(t[t$class %in% 1:20,]))
    tt[nrow(tt),1] <- 45
    
    codes <- data.frame(cbind(c(30,40,45,50,51),
                              c("Non-Forest","Forest","Loss","Gain","Gain-Loss")))
    names(codes) <- c("class","class_name")
    
    ttt <- merge(tt,codes,by.x="class",by.y="class",all.x=T)
    ttt <- arrange(ttt,class)
    
    tttt <- ttt[,c("class_name","area")]
    names(tttt) <- c("Class","Area (ha)")
    
    tttt
    
  })
  
  
  
  ############### Display the STATS
  output$display_stats <- renderTable({
    req(stats())
    stats()
  })
  
  ############### Display the STATS
  output$display_loss_graph <- renderPlot({
    req(raw_stats())
    t <- raw_stats()
    tt <- t[t$class < 20 & t$class > 0,]
    tt$year <- 2000+tt$class
    barplot(area ~ year,tt,xlab = "Year",ylab ="Tree cover loss (Ha)")
  })
  
  ############### Display the results as map
  output$display_res <- renderPlot({
    #req(input$DisplayMapButton)
    
    threshold   <- input$threshold
    the_basename <- the_basename()
    
    print('Check: Display the map')
    
    gfc <- raster(fmask_geo())
    # aoi <- spTransform(readOGR(make_aoi()),proj4string(gfc))
    aoi <- readOGR(make_aoi())
    
    plot(gfc)
    plot(aoi,add=T,border="yellow")
    
  })
  
  ##################################################################################################################################
  ############### Button to download the stats file (csv)
  output$ui_download_stats <- renderUI({
    req(stats())
    downloadButton('download_stats',
                   label = textOutput('download_csv_button'))
  })
  
  ##################################################################################################################################
  ############### Enable to download the stats (csv)
  output$download_stats <- downloadHandler(
    filename = function() {
      paste0("stats_",the_basename(), ".csv")
    },
    content  = function(xx) {
      to_export <- raw_stats()
      write.csv(to_export, xx, row.names = FALSE)
    }
  )

  
  ##################################################################################################################################
  ############### Button to download the tif file
  output$ui_download_gfc_map <- renderUI({
    req(fmask())
    #req(input$DisplayMapButton)
    downloadButton('download_gfc_map',
                   label = textOutput('download_map_button'))
  })

  ##################################################################################################################################
  ############### Enable to download the map (.tif)
  output$download_gfc_map <- downloadHandler(
    filename = function() {
      paste0(the_basename(), ".tif")
    },
    content  = function(xx) {
      to_export <- raster(paste0(gfc_dir,"gfc_",the_basename(),"_",input$threshold,"_map_clip_pct_geo.tif"))
      writeRaster(to_export, xx)
    }
  )
  
  
  


  
  
  ##################################################################################################################################
  ############### Parameters title as a reactive
  parameters <- reactive({
    req(process())
    
    mspa1 <- as.numeric(input$option_FGconn)
    mspa2 <- as.numeric(input$option_EdgeWidth)
    mspa3 <- as.numeric(input$option_Transition)
    mspa4 <- as.numeric(input$option_Intext)
    mspa5 <- as.numeric(input$option_dostats)
    
    paste(mspa1,mspa2,mspa3,mspa4,mspa5,sep=" ")
    
  })
  
  ##################################################################################################################################
  ############### Parameters suffix as a reactive
  parameters_u <- reactive({
    req(process())
    
    mspa1 <- as.numeric(input$option_FGconn)
    mspa2 <- as.numeric(input$option_EdgeWidth)
    mspa3 <- as.numeric(input$option_Transition)
    mspa4 <- as.numeric(input$option_Intext)
    mspa5 <- as.numeric(input$option_dostats)
    
    paste(mspa1,mspa2,mspa3,mspa4,sep="_")
    
  })
  
  
  ##################################################################################################################################
  ############### MSPA start button
  output$mspaStartButton <- renderUI({
    validate(need(the_basename(), "Define the area of interest"))
    req(process())
    actionButton('mspaStartButton', textOutput('mspa_start_button'))
  })
  
  ##################################################################################################################################
  ############### Text to explain
  output$explainMSPArun <- renderUI({
    validate(need(the_basename(), "Define the area of interest"))
    req(input$aoi_type)
    textOutput('mspa_explanation')
  })
  
  ##################################################################################################################################
  ############### Run MSPA
  mspa_res <- eventReactive(input$mspaStartButton,
                            {
                              req(process())
                              req(input$mspaStartButton)
                              req(the_basename())
                              
                              the_basename <- the_basename()
                              parameters   <- parameters()
                              parameters_u <- parameters_u()
                              threshold    <- input$threshold
                              
                              time_start   <- Sys.time()
                              if(!file.exists(paste0(msp_dir,"mspa_",the_basename,"_",threshold,"_",parameters_u,"_proj.tif"))){
                              
                                file.copy(paste0(scriptdir,"MSPA/"),
                                        tmp_dir,
                                        recursive = T,
                                        overwrite = T)
                              
                              dir.create(paste0(tmp_dir,"MSPA/input/"), showWarnings = F)
                              dir.create(paste0(tmp_dir,"MSPA/output/"),showWarnings = F)
                              dir.create(paste0(tmp_dir,"MSPA/tmp/"),   showWarnings = F)
                              
                              file.copy(paste0(gfc_dir,"mask_mspa_gfc_",the_basename,"_",threshold,".tif"),
                                        paste0(tmp_dir,"MSPA/input/input.tif"),overwrite = T)
                              
                              write(parameters,paste0(tmp_dir,"MSPA/input/mspa-parameters.txt"))
                              
                              system(sprintf("chmod 755 %s/mspa_lin64",
                                             paste0(tmp_dir,"MSPA")
                                             ))
                              
                              print("have a break")
                              
                              withProgress(message = paste0('Processing MSPA for ',the_basename),
                                           value = 0,
                                           {
                                             setwd(paste0(tmp_dir,"MSPA"))
                                             system(sprintf("bash %s/sepal_mspa",
                                                            paste0(tmp_dir,"MSPA")
                                                            ))
                                             setwd(rootdir)
                                           })
                              
                              process_time <- Sys.time() - time_start
                              
                              write(process_time,paste0(tmp_dir,"MSPA/output/mspa-process.txt"))
                              
                              file.copy(paste0(tmp_dir,"MSPA/output/input_",parameters_u,".tif"),
                                        paste0(msp_dir,"mspa_",the_basename,"_",threshold,"_",parameters_u,"_proj.tif"),
                                        overwrite = T)
                              
                              file.copy(paste0(tmp_dir,"MSPA/output/input_",parameters_u,"_stat.txt"),
                                        paste0(msp_dir,"mspa_",the_basename,"_",threshold,"_",parameters_u,"_stat.txt"),
                                        overwrite = T)
                              }

                              
                              raster(paste0(msp_dir,"mspa_",the_basename,"_",threshold,"_",parameters_u,"_proj.tif"))
                            })
  
  
  
  ############### Display the results as map
  output$display_mspa <- renderPlot({
    req(mspa_res())
    print('Check: Display the map')
    
    the_basename <- the_basename()
    parameters_u <- parameters_u()
    threshold    <- input$threshold
    
    mspa_res <- mspa_res()
    if(file.exists(paste0(msp_dir,"mspa_",the_basename,"_",threshold,"_",parameters_u,"_proj.tif"))){
    plot(mspa_res, axes = FALSE)}else{NULL}
  })
  
  ##################################################################################################################################
  ############### Display parameters
  output$parameterSummary <- renderText({
    req(parameters())
    print(paste0("Parameters are : ",parameters()))
  })
  
  ##################################################################################################################################
  ############### Display time
  output$mspa_summary <- renderTable({
    req(mspa_res())
    
    the_basename <- the_basename()
    parameters   <- parameters()
    parameters_u <- parameters_u()
    threshold    <- input$threshold
    
    time <- readLines(paste0(tmp_dir,"MSPA/output/mspa-process.txt"))
    
    if(file.exists(paste0(msp_dir,"mspa_",the_basename,"_",threshold,"_",parameters_u,"_stat.txt"))){
      
      res  <- readLines(paste0(msp_dir,"mspa_",the_basename,"_",threshold,"_",parameters_u,"_stat.txt"))
    # info <- res[11:15]
    # table <- data.frame(cbind(str_split_fixed(info," : ",2)[,1],
    #                str_split_fixed(str_split_fixed(info," : ",2)[,2]," ",2)[,1]))
    # names(table) <- c("class","pix")
    # pix <- res(raster(paste0(gfc_dir,"gfc_",the_basename,"_",threshold,"_map_clip_pct.tif")))[1]
    # table$area <- as.numeric(table$pix) * pix * pix / 10000
    # table$percent <- table$area/sum(table[1:4,]$area)*100
    # out <- table[,c("class","area","percent")]
    # names(out) <- c("Class","Area (ha)","Weight (%)")
    
    info <- res[20:26]
    out  <- data.frame(cbind(str_split_fixed(info,": ",2)[,1],
                             paste0(
                               round(
                                 as.numeric(
                                   str_split_fixed(
                                     str_split_fixed(info,": ",2)[,2],
                                     " %",2)[,1]),
                                 1),
                               " %")
                             ))
    names(out) <- c("Class","Proportion")
    
    out}else{NULL}
  })
  
  
  
  ##################################################################################################################################
  ############### Button to download the tif file
  output$ui_download_mspa <- renderUI({
    req(mspa_res())
    #req(input$DisplayMapButton)
    downloadButton('download_mspa_map',
                   label = textOutput('download_mspa_button'))
  })
  
  ##################################################################################################################################
  ############### Button to download the tif file
  output$ui_download_mspa_stat <- renderUI({
    req(mspa_res())
    #req(input$DisplayMapButton)
    downloadButton('download_mspa_stat',
                   label = textOutput('download_mspa_stat_button'))
  })
  ##################################################################################################################################
  ############### Enable to download the map (.tif)
  output$download_mspa_stat <- downloadHandler(
    filename = function() {
      paste0("stats_mspa_",the_basename(),"_",input$threshold,"_",parameters_u(),".txt")
    },
    content  = function(xx) {
      to_export <- readLines(paste0(msp_dir,"mspa_",the_basename(),"_",input$threshold,"_",parameters_u(),"_stat.txt"))
      writeLines(to_export, xx)
    }
  )
  ##################################################################################################################################
  ############### Turn off progress bar
  
  progress$close()
  ################## Stop the shiny server
  ####################################################################################
  
})
