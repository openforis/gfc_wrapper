####################################################################################################

gfcdwn_dir <- paste0(normalizePath("~"),"/downloads/gfc/2018/")
rootdir    <- paste0(normalizePath("~"),"/gfc_wrapper/")

scriptdir   <- paste0(rootdir,"scripts/")
data_dir    <- paste0(rootdir,"data/")
tmp_dir     <- paste0(rootdir,"tmp/")
gfc_dir     <- paste0(data_dir,"gfc/")
aoi_dir     <- paste0(data_dir,"aoi/")
stt_dir     <- paste0(data_dir,"stat/")
msp_dir     <- paste0(data_dir,"mspa/")

dir.create(scriptdir,recursive=T,showWarnings = F)
dir.create(gfcdwn_dir,recursive=T,showWarnings = F)
dir.create(data_dir,showWarnings = F)
dir.create(gfc_dir,showWarnings = F)
dir.create(aoi_dir,showWarnings = F)
dir.create(stt_dir,showWarnings = F)
dir.create(tmp_dir,showWarnings = F)
dir.create(msp_dir,showWarnings = F)

####################################################################################################
#################### load hard-coded parameters
max_year    <- 18
spacing     <- 0.011
offset      <- 0.001
proj        <- '+proj=aea +lat_1=20 +lat_2=-23 +lat_0=0 +lon_0=25 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs '

## Set a range of sub-sampling (take a point every xx point)
classes <- c(100,50,40,30,20,10,5,4,3,2,1)

## SET THE LAYERS OF GFC TO USE
types   <- c("treecover2000","lossyear","gain","datamask")

####################################################################################################


#################### CREATE A LEGEND
my_classes <- c(0,
                1:max_year,
                30,
                40,
                50,
                51)

my_labels  <- c("no data",
                paste0("loss_",2000+1:max_year),
                "non forest",
                "forest",
                "gains",
                "gains+loss")

codes      <- data.frame(cbind(my_labels,my_classes))
#countrycode <- "custom"

#################### CREATE A COLOR TABLE FOR THE OUTPUT MAP
loss_col <- colorRampPalette(c("yellow", "darkred"))
nonf_col <- "lightgrey"
fore_col <- "darkgreen"
gain_col <- "lightgreen"
ndat_col <- "black"
gnls_col <- "purple"

my_colors  <- col2rgb(c(ndat_col,
                        loss_col(max_year),
                        nonf_col,
                        fore_col,
                        gain_col,
                        gnls_col))

pct <- data.frame(cbind(my_classes,
                        my_colors[1,],
                        my_colors[2,],
                        my_colors[3,]))

write.table(pct,paste0(gfc_dir,"color_table.txt"),row.names = F,col.names = F,quote = F)



####################################################################################################
################# PIXEL COUNT FUNCTION
pixel_count <- function(x){
  info    <- gdalinfo(x,hist=T)
  buckets <- unlist(str_split(info[grep("bucket",info)+1]," "))
  buckets <- as.numeric(buckets[!(buckets == "")])
  hist    <- data.frame(cbind(0:(length(buckets)-1),buckets))
  hist    <- hist[hist[,2]>0,]
}

################# Create a function that gives the estimate of loss for a given intensity and a given year
estimate <- function(x,y){
  nrow(df[
    df$lon_fact%%x == 0 & 
      df$lat_fact%%x == 0 &
      df$code == y
    ,])/
    nrow(df[
      df$lon_fact%%x == 0 & 
        df$lat_fact%%x == 0 &
        (df$code == 40 | (df$code > 0 & df$code < 30))
      ,])
}

################# Create a function that gives the estimate of loss for a given intensity and all years
all_estimate <- function(x){
  nrow(df[
    df$lon_fact%%x == 0 & 
      df$lat_fact%%x == 0  & 
      (df$code > 0 & df$code < 30)
    ,])/
    nrow(df[
      df$lon_fact%%x == 0 & 
        df$lat_fact%%x == 0 &
        (df$code == 40 | (df$code > 0 & df$code < 30))
      ,])
}
################# Create a function that gives the number of points corresponding to a given subsampling
nombre <- function(x){
  nrow(df[
    df$lon_fact%%x == 0 & 
      df$lat_fact%%x == 0 &
      (df$code == 40 | (df$code > 0 & df$code < 30))
    ,]
  )}
