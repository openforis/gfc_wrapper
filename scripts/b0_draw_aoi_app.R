
aoi_name   <- paste0(aoi_dir,the_basename)
aoi_shp    <- paste0(aoi_name,".shp")
aoi_field <-  "id_aoi"

  aoi <- drawn_aoi
  aoi@data[,aoi_field] <- row(aoi)[,1]
  
  writeOGR(obj = aoi,
           dsn = aoi_shp,
           layer = aoi_name,
           driver = "ESRI Shapefile",
           overwrite_layer = T)

