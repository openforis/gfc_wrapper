

aoi_name   <- paste0(aoi_dir,the_basename)
aoi_shp    <- paste0(aoi_name,".shp")
aoi_field <-  "id_aoi"

if(!file.exists(aoi_shp)){
  aoi <- spTransform(readOGR(aoi_file_path),CRS('+init=epsg:4326'))
  aoi@data[,aoi_field] <- row(aoi)[,1]
  
  writeOGR(obj = aoi,
           dsn = aoi_shp,
           layer = aoi_name,
           driver = "ESRI Shapefile",
           overwrite_layer = T)
}