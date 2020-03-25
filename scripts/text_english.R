############################ Text boxes ENGLISH version

## t == TAB
## b == BOX
## p == PARAGRAPH
## w == WARNING


############################ TITLES
output$title    <- reactive({  "Base forest map" })

output$t0_title <- reactive({  "About" })
output$t1_title <- reactive({  "1: Area of interest" })
output$t2_title <- reactive({  "2: Forest change map" })
output$t3_title <- reactive({  "3: Fragmentation map" })

output$source_code <- reactive({  "Source code" })
output$bug_reports <- reactive({  "Bug reports" })

############################ Titles
output$title_aoi         <- reactive({"Area of interest"})
output$title_process     <- reactive({"Process"})
output$title_results     <- reactive({"Forest change map"})
output$title_mspa        <- reactive({"Fragmentation map"})
output$title_param_mspa  <- reactive({"Parameters"})

############################ BUTTONS
output$download_csv_button    <- reactive({'Download as tabular data (.csv)'})
output$download_map_button    <- reactive({'Download as GeoTiff data (.tif)'})
output$download_mspa_button   <- reactive({'Download as GeoTiff data (.tif)'})
output$download_mspa_stat_button   <- reactive({'Download as tabular data (.txt)'})

output$mspa_start_button      <- reactive({"Run"})
output$mspa_explanation      <- reactive({"Click run to start generating the fragmentation map"})

output$gfc_start_button       <- reactive({"Run"})
output$gfc_explanation      <- reactive({"Click run to start downloading the GFC data and creating the base map"})

output$aoi_type_choice        <- reactive({'Select an AOI'})

output$text_choice_country    <- reactive({'Choose country name'})
output$text_choice_threshold  <- reactive({'Choose canopy cover threshold'})

output$process_button         <- reactive({"Download GFC data, merge tiles, clip to boundaries, generate map"})
output$map_button             <- reactive({"Generate map"})
output$display_map_button     <- reactive({"Display map"})


# ############################ BASENAME FIELDS
# output$basename_area_field     <- reactive({"Basename of area file to export"})
# output$basename_sampling_field <- reactive({"Basename of csv to export"})
# output$basename_export_field   <- reactive({"Basename of sampling design files to export"})


#################################################################################### 
############################ INTRODUCTION TAB
#################################################################################### 
# output$welcome_title <- reactive({"About"})
# 
# output$welcome_message <- reactive({
#   HTML(paste0(
#     "This interactive tool generates a base forest change map from the GFC dataset (Hansen et al. 2013) and a forest integrity map from Guidos Toolbox.
#     For support ask",a(href="http://www.openforis.org/support"," Open Foris support forum",target="_blank")
#   ))
# })


############################ INTRODUCTION TAB - BOX 0
output$language_title <- reactive({"Language"})

############################ INTRODUCTION TAB - BOX 1
output$overview_title <- reactive({"Description"})

output$overview_body  <- reactive({
  HTML(paste0(
    " This application allows the user to: <br/>
    - define an area of interest <br/>
    - retrieve tree cover change data from the Hansen et al., (2013) dataset.  <br/>
    - combine the layers to produce a forest change map, for a given canopy cover threshold <br/>
    - use a fragmentation tool (Vogt & Ritter, 2017) to measure the integrity of forests <br/><br/>
    For support ask",
    a(href="http://www.openforis.org/support"," Open Foris support forum",target="_blank")
  ))})

############################ INTRODUCTION TAB - BOX 2
output$gfc_back_title <- reactive({"GFC background"})

output$gfc_background  <- reactive({
  HTML(paste0(
    "The Global Forest Change dataset (GFC, V1.6) provides global layers of information on tree cover and tree cover change since 2000, at 30m spatial resolution and consists of: <br/>
- Tree canopy cover for the year 2000 (treecover2000) <br/>
- Global forest cover gain during 2000–2012 (gain) <br/>
- Year of gross forest cover loss event during 2001-2018 (lossyear) <br/>
<br/>",
    "Citation for GFC: Hansen, M. C., P. V. Potapov, R. Moore, M. Hancher, S. A. Turubanova, A. Tyukavina, D. Thau, S. V. Stehman, S. J. Goetz, T. R. Loveland, A. Kommareddy, A. Egorov, L. Chini, C. O. Justice, and J. R. G. Townshend. 2013. “High-Resolution Global Maps of 21st-Century Forest Cover Change.” Science 342 (15 November): 850–53. <br/> <br/>",
    "The data is ", 
    a(href="http://earthenginepartners.appspot.com/science-2013-global-forest","available on-line",target="_blank"),
    " and updated every year."
    ))
  })


############################ INTRODUCTION TAB - MSPA
output$mspa_back_title <- reactive({"MSPA background"})

output$mspa_background  <- reactive({
  HTML(paste0(
    
    "MSPA (Morphological Spatial Pattern Analysis) is a customized sequence of mathematical morphological operators targeted at the description of the geometry and connectivity of the image components. 
<br/>
Based on geometric concepts only, this methodology can be applied at any scale and to any type of digital images in any application field.
<br/>
The foreground area of a binary image is divided into seven generic MSPA classes: ",
    tags$ol(
      tags$li("Core"),
      tags$li("Islet"),
      tags$li("Perforation"),
      tags$li("Edge"), 
      tags$li("Loop"),
      tags$li("Bridge"),
      tags$li("Branch ")),
    "This segmentation results in mutually exclusive classes which, when merged, exactly correspond to the initial foreground area.<br/>",
    
    "Adapted from the",
    a(href="http://forest.jrc.ec.europa.eu/download/software/guidos/mspa/"," Guidos Toolbox developed by JRC",target="_blank"),
    "<br/> <br/>",
    
    "Citation for GUIDOS: Vogt P, Riitters, K, 2017. GuidosToolbox: universal digital image object analysis. 
European Journal of Remote Sensing, 50, 1, pp. 352-361, ",
    a(href="https://dx.doi.org/10.1080/22797254.2017.1330650","DOI",target="_blank"),
    "<br/>",
    "Citation for MSPA:   Soille P, Vogt P, 2008. Morphological segmentation of binary patterns. Pattern Recognition Letters 30, 4:456-459, ", 
    a(href="https://dx.doi.org/10.1016/j.patrec.2008.10.015","DOI",target="_blank"),
    "<br/>"
  ))
  
})

############################ INTRODUCTION TAB - BOX 3
output$t1_b3_title <- reactive({"How to use the tool ?"})
output$t1_b3_body  <- reactive({
  HTML(paste(
    "You have to go through all the steps in the left panel, in this order:", 
    tags$ol(
      tags$li("Select the map data which will be assessed. The required input is either vector (.shp and .sqlite supported) or raster (.tif, .img, .pix, .rst, .jpeg2000, .grd and .hdf supported)"), 
      tags$li("Compute the areas of each strata"), 
      tags$li("Select the expected accuracies of the strata"),
      tags$li("Compute the sampling size"),
      tags$li("Draw the sampling points and export as a Collect Earth file")
    )
    ,sep = '<br/>'))
})

############################ INTRODUCTION TAB - BOX 4
output$title_disclaimer <- reactive({"Disclaimer"})

output$body_disclaimer  <- reactive({
  HTML(paste0(
    "FAO declines all responsibility for errors or deficiencies in the database 
    or software or in the documentation accompanying it for program maintenance and 
    upgrading as well as for any damage that may arise from them.<br/>
      FAO also declines any responsibility for updating the data and assumes 
    no responsibility for errors and omissions in the data provided.<br/>
      Users are, however, kindly asked to report any errors or deficiencies in this product to FAO."
  ))})

output$t1_b4_p2_title <- reactive({"Reference and Documents"})

#################################################################################### 
############################ MAP TAB
#################################################################################### 

############################ MAP TAB - BOX 1
output$t2_b1_title    <- reactive({"Data type"})

output$t2_b1_body  <- reactive({
  HTML(paste0(
    "First choose the type of data used for the stratification - the map
    <br/>
      The map can be in raster or vector format. 
    The map area will be calculated in the next tab.<br/>
      The input map can represent a single time or multiple times change made from satellite images<br/>
      It can also be any acquired from available map data of land cover or land use."
  ))})

#


############################ MAP TAB - BOX 2
output$t2_b2_title <- reactive({"Download test data"})

############################ MAP TAB - BOX 3
output$t2_b3_title <- reactive({"Output folder"})

output$t2_b3_body  <- reactive({HTML(paste0(
  
  "All products of the random stratified sampling design will be stored here: areas of the map, sampling sizes, point file"
  
))})

############################ MAP TAB - BOX 4
output$t2_b4_title <- reactive({"Manual selection of areas ?"})

output$t2_b4_body  <- reactive({HTML(paste0(
  
  "The map classes will be used as strata in the design of the sampling"
  
))})

############################ MAP TAB - BOX 5
output$t2_b5_title  <- reactive({"View table data"})

output$t2_b5_body   <- reactive({HTML(paste0(
  "Select columns to view in a data table. <br/> 
      The columns are read from the shapefile database or the CSV with the raster areas"
))})

#################################################################################### 
############################ AREA TAB
#################################################################################### 

############################ AREA TAB - BOX 1
output$t3_b1_title  <- reactive({"Area calculation"})

output$t3_b1_body   <- reactive({HTML(paste0(
  "Map areas are calculated by counting the frequency of the pixels for 
    each map class or by summing the areas of all the polygons.<br/>
      If using raster data the map area can be calculated using R or Open Foris Geospatial Toolkit (OFT).<br/>
      R is compatible with all systems and OFT is only compatible with Linux.<br/>
      Area calculations of large raster files using R will take some time."
))})


############################ AREA TAB - BOX 2
output$t3_b2_title  <- reactive({"Legend and Areas"})

output$t3_b2_body  <- reactive({HTML(paste0(
  "The areas for each of the map categories need to be calculated in order to calculate the overall and stratified sample size.
    <br/>
      Make sure to click on the submit legend button to load the map area table."
))})

############################ AREA TAB - BOX 3
output$t3_b3_title  <- reactive({"Legend labeling"})

output$t3_b3_body  <- reactive({HTML(paste0(
  "The legend classes need to be specified and submitted. Please wait for the map values to appear. 
    Then type the names of the classes and submit the legend.<br/>
      After submitting the legend the table with the map classes and area will appear.
    The legend names can be modified at any time in this tab.<br/>"
  
))})



############################ AREA TAB - BOX 4
output$t3_b4_title  <- reactive({"Display map "})

#################################################################################### 
############################ CLASSES TAB
####################################################################################

############################ Classes TAB - BOX 1
output$t4_b1_title  <- reactive({"What are the expected accuracies?"})

output$t4_b1_body  <- reactive({HTML(paste0(
  "Some classes are identified easier than other classes. <br/>
      Usually common classes, which occupy the majority of the map, are the easiest to identify. <br/>
      Rare classes, such as land change classes, which occupy a small portion of the map area, 
    can be very difficult to identify.
    This measure will influence the overall sample size. <br/>
      More classes with lower confidence will increase the overall sample size"
))})

############################ Classes TAB - BOX 2
output$t4_b2_title  <- reactive({"Choose classes expected user's accuracies"})

############################ Classes TAB - BOX 3
output$t4_b3_title  <- reactive({"Expected User's Accuracy (EUA) values for specific classes"})

output$t4_b3_heua   <- reactive({"High expected user accuracy"})
output$t4_b3_leua   <- reactive({"Low expected user accuracy"})

#################################################################################### 
############################ SAMPLING SIZE TAB
####################################################################################

############################  SIZE TAB - BOX 1
output$t5_b1_title  <- reactive({"Sampling size"})

output$t5_b1_body   <- reactive({HTML(paste0(
  'In the sampling design, the sample size for each map category is chosen to ensure that 
the sample size is large enough to produce sufficiently precise estimates of the area of the class (GFOI, 2013)'
))})

output$t5_b1_seeoa  <- reactive({"Standard error of expected overall accuracy"})
output$t5_b1_mss    <- reactive({"Minimum sample size per strata"})
output$t5_b1_modify <- reactive({"Do you want to modify the sampling size?"})

############################ SIZE TAB - BOX 2
output$t5_b2_title  <- reactive({"Distribution of samples"})

############################ SIZE TAB - BOX 3
output$t5_b3_title  <- reactive({"Formula to calculate the overall sample size"})

output$t5_b3_body   <- reactive({HTML(paste0(
  "The equation below calculates an adequate overall sample size for stratified
    random sampling that can then be distributed among the different strata.",
  br(),
  tags$ul(
    tags$li("N is number of units in the area of interest (number of overall pixels if the
                                                         spatial unit is a pixel, number of polygons if the spatial unit is a polygon)"),
    tags$li("S(O) is the standard error of the estimated overall accuracy that we would like to achieve"),
    tags$li("Wi is the mapped proportion of area of class i"),
    tags$li("Si is the standard deviation of stratum i."))
))})

#################################################################################### 
############################ ALLOCATION TAB
####################################################################################

############################ ALLOCATION TAB - BOX 1
output$t6_b1_title  <- reactive({"Create a stratified random sample on the map"})
output$t6_b1_body   <- reactive({HTML(paste0(
  "Points are randomly distributed for each of the map classes.
    <br/>
      The number of points per class is from the 'adjusted' column in the Sample Size tab"
))})

############################ ALLOCATION TAB - BOX 2
output$t6_b2_title  <- reactive({"Create a Collect Earth Project file (.cep) to start validation work"})
