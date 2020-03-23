aoi_name   <- paste0(aoi_dir,the_basename)
aoi_shp    <- paste0(aoi_name,".shp")
aoi_field <-  "id_aoi"

if(!file.exists(aoi_shp)){
  
  aoi   <- getData('GADM',
                   path=aoi_dir,
                   country= countrycode,
                   level=0)
  
  print(countrycode)
  print(proj4string(aoi))
  
  aoi <- spTransform(aoi,CRS('+init=epsg:4326'))
  (bb    <- extent(aoi))
  
  aoi@data[,aoi_field] <- row(aoi)[,1]
  
  writeOGR(obj = aoi,
           dsn = aoi_shp,
           layer = aoi_name,
           driver = "ESRI Shapefile",
           overwrite_layer = T)
}