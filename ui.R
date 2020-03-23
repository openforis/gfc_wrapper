####################################################################################
#######          PRIMS point                                    ####################
#######    contributors:  Remi d'Annunzio                       ####################
#######              FAO Open Foris SEPAL project               ####################
#######    remi.dannunzio@fao.org                               ####################
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
## GFC_WRAPPER / ui
####################################################################################


print("Starting the process")

options(stringsAsFactors=FALSE)
options(shiny.launch.browser=T)

source("scripts/packages.R",  echo = TRUE)
source("scripts/app_config.R",echo = TRUE)


####################################################################################
####### Start User Interface

shinyUI(
  
  dashboardPage(
    skin='green',
    
    ####################################################################################
    #######       General title of the application            ##########################
    dashboardHeader(
      title= textOutput('title'),
      titleWidth = 350),
    
    ####################################################################################
    #######       Side Bar definition with all TABS           ##########################
    dashboardSidebar(
      width = 350,
      sidebarMenu(
        
        
        menuItem(textOutput('t1_title',inline=T), tabName = "aoi_tab",   icon = icon("fas fa-globe-africa")),
        menuItem(textOutput('t2_title',inline=T), tabName = "mask_tab",  icon = icon("cog", lib = "glyphicon")),
        menuItem(textOutput('t3_title',inline=T), tabName = "mspa_tab",  icon = icon("dashboard")),
        menuItem(textOutput('t0_title',inline=T), tabName = "intro_tab", icon = icon("question")),
        hr(),
        br(),
        br(),
        menuItem(textOutput('source_code',inline=T), icon = icon("file-code-o"),href = "https://github.com/openforis/gfc_wrapper"),
        menuItem(textOutput('bug_reports',inline=T), icon = icon("bug")        ,href = "https://github.com/openforis/gfc_wrapper/issues")
      )
    ),
    
    ####################################################################################
    #######       Body structure of the Dashboard: tabItems   ##########################
    dashboardBody(
      tabItems(
        
        ####################################################################################
        # New Tab
        tabItem(tabName = "intro_tab",
                fluidRow(
                  
                  # ####################################################################################
                  # Change style of the CSS style of the tabBox, making the color green
                  tags$style(".nav-tabs-custom .nav-tabs li.active {border-top-color: #00994d;}"),
                  
                  ## CSS format for errors, making the message in purple
                  tags$head(tags$style(HTML(".shiny-output-error-validation {color: #cc00ff;font-family:courier;font-size: 120%;}"))),
                  
                  ####################################################################################
                  # New box
                  box(
                    title= textOutput('t1_b1_title'), width=12,status = "success", solidHeader= TRUE,
                    
                    tabsetPanel(
                           tabPanel(title= textOutput('overview_title'),
                                    #uiOutput("chosen_language")   ,
                                    htmlOutput('overview_body')
                           ),

                           tabPanel(textOutput('language_title'),
                                    selectInput(
                                      'language','',choices = c("English"))
                                                                  
                                    
                           ),  # end tabPanel
                           
                           tabPanel(title= textOutput('gfc_back_title'), 
                                    htmlOutput('gfc_background') 
                                    
                           ),  # end tabPanel
                           
                           tabPanel(title= textOutput('mspa_back_title'), 
                                    htmlOutput('mspa_background'),
                                    img(src="thumbnails/mspa.jpg", height = 355, width = 623)
                                    
                           )  # end tabPanel
                    )
                    
                  )
                    # End of Box
                   
                    
                  ),
                  ####################################################################################
                  # End of the fluid row
                  
                  
                  fluidRow(
                    ####################################################################################
                    # New box
                    box(title=textOutput('title_disclaimer'),width=12,status = "success", solidHeader= TRUE,
                        br(),
                        htmlOutput('body_disclaimer'),
                        br(),
                        br(),
                        img(src="thumbnails/sepal-logo-EN-white.jpg", height = 100, width = 210),
                        img(src="thumbnails/UNREDD_LOGO_COLOUR.jpg",  height = 80,  width = 100),
                        img(src="thumbnails/Open-foris-Logo160.jpg",  height = 70,  width = 70),
                        br()
                    )
                    ####################################################################################
                    # End of the Box
                    
                  )
                  ####################################################################################
                  # End of the fluid row
                  
                ),
                ####################################################################################
                # End of the tabItem 
                
                ####################################################################################
                # New Tab
                tabItem(tabName = "aoi_tab",
                        fluidRow(
                          
                          ####################################################################################
                          # New box
                          box(
                            title= textOutput('title_aoi'), width=9,status="success" , solidHeader= TRUE,
                            uiOutput("chosen_language")   ,
                            htmlOutput('body_description'),
                            
                            selectInput('aoi_type',
                                        textOutput('aoi_type_choice'),
                                        choices=setNames(c("draw","gadm","custom"), 
                                                         c("Draw a shape",
                                                           "Country boundaries",
                                                           "Select file"
                                                         ))
                            ),
                            
                            uiOutput("aoi_select_gadm"),
                            uiOutput("aoi_select_custom"),
                            textOutput('aoi_message_ui'),
                            
                            sliderInput('threshold',
                                        textOutput('text_choice_threshold'),
                                        min = 0,
                                        max=100,
                                        step = 5,
                                        value=30
                            ),
                            
                            leafletOutput("leafmap")
                            
                          )
                          ####################################################################################
                          # End of the Box
                          
                        )
                        ####################################################################################
                        # End of the fluid row
                        
                ),
                ####################################################################################
                # End of the tabItem 
                
                
                ####################################################################################
                # New Tab
                tabItem(tabName = "mask_tab",
                        
                        fluidRow(
                          ####################################################################################
                          # New box
                          box(title=textOutput('title_process'),width=6,status = "success", solidHeader= TRUE,
                              #uiOutput("ProcessButton"),
                              uiOutput("runGFCButton"),
                              #uiOutput("explainGFCrun"),
                              tableOutput("display_stats"),
                              plotOutput("display_loss_graph"),
                              uiOutput("ui_download_stats")
                          ),
                          
                          ####################################################################################
                          # New box
                          box(title= textOutput('title_results'),width=6, status = "success", solidHeader= TRUE,
                              #uiOutput("DisplayMapButton"),
                              plotOutput("display_res"),
                              uiOutput("ui_download_gfc_map")
                          )
                          ####################################################################################
                          # End of the Box
                          
                        )
                        ####################################################################################
                        # End of the fluid row
                        
                ),
                ####################################################################################
                # End of the tabItem 
                
                ####################################################################################
                # New Tab
                tabItem(tabName = "mspa_tab",
                        
                        fluidRow(
                          
                          ####################################################################################
                          # New box
                          
                          box(title= textOutput('title_param_mspa'),width=6, status = "success", solidHeader= TRUE,
                              htmlOutput('body_opt_dir'),
                              selectInput(inputId = 'option_FGconn',
                                          label = "Foreground connectivity",
                                          choices = c(4,8),
                                          multiple = FALSE,
                                          selected = 8
                              ),
                              selectInput(inputId = 'option_EdgeWidth',
                                          label = "Edge Width",
                                          choices = 1:100,
                                          multiple = FALSE,
                                          selected = 1
                              ),
                              selectInput(inputId = 'option_Transition',
                                          label = "Transition Core - Loop/Bridge",
                                          choices = setNames(c(0,1),c("No","Yes")),
                                          multiple = FALSE,
                                          selected = 1
                              ),
                              selectInput(inputId = 'option_Intext',
                                          label = "Separate internal from external features ?",
                                          choices = setNames(c(0,1),c("No","Yes")),
                                          multiple = FALSE,
                                          selected = 1
                              ),
                              selectInput(inputId = 'option_dostats',
                                          label = "Compute statistics ?",
                                          choices = setNames(c(0,1),c("No","Yes")),
                                          multiple = FALSE,
                                          selected = 1
                              ),
                              
                              
                              textOutput("parameterSummary")
                              
                          )
                          
                          
                          # End of Box
                          ####################################################################################
                          
                          ,
                          
                          ####################################################################################
                          # New box
                          
                          box(title= textOutput('title_mspa'),width=6, status = "success", solidHeader= TRUE,
                              uiOutput("mspaStartButton"),
                              #uiOutput("explainMSPArun"),
                              plotOutput("display_mspa"),
                              tableOutput("mspa_summary"),
                              uiOutput("ui_download_mspa")
                          )
                          
                          # End of Box
                          ####################################################################################
                          
                          
                          
                          
                        )
                        ####################################################################################
                        # End of the fluid row
                        
                )
                ####################################################################################
                # End of the tabItem 
                
        )
        ####################################################################################
        # End of the tabItem list
        
      )
      ####################################################################################
      # End of the Dashboard Body
      
    )
    ####################################################################################
    # End of the Dashboard Page 
    
  )
  ####################################################################################
  # End of the User Interface