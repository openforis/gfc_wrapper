#################### SKIP IF OUTPUTS EXISTS ALREADY
if(!file.exists(paste0(gfc_dir,"gfc_",the_basename,"_",threshold,"_map_clip_pct.tif"))){
  
  system(sprintf("rm -f %s",
                 paste0(tmp_dir,"tmp_gfc_map_",the_basename,".tif")))
  #################### COMBINATION INTO NATIONAL SCALE MAP
  system(sprintf("gdal_calc.py -A %s -B %s -C %s -D %s --co COMPRESS=LZW --outfile=%s --calc=\"%s\"",
                 paste0(gfc_dir,"gfc_",the_basename,"_",types[1],".tif"),
                 paste0(gfc_dir,"gfc_",the_basename,"_",types[2],".tif"),
                 paste0(gfc_dir,"gfc_",the_basename,"_",types[3],".tif"),
                 paste0(gfc_dir,"gfc_",the_basename,"_",types[4],".tif"),
                 paste0(tmp_dir,"tmp_gfc_map_",the_basename,".tif"),
                 
                 paste0("(A<=",threshold,")*((C==1)*50 + (C==0)*30)+", ### NON FOREST
                        "(A>", threshold,")*",
                        "((C==1)*(",
                        "(B>0)*  51 +",           ### GAIN+LOSS
                        "(B==0)* 50 )+",          ### GAIN
                        "(C==0)*(",
                        "(B>0)*B+",               ### LOSS
                        "(B==0)* 40 ))"           ### FOREST STABLE
                 )
  ))
  
  #############################################################
  ### CROP TO COUNTRY BOUNDARIES
  system(sprintf("oft-cutline_crop.py -v %s -i %s -o %s -a %s",
                 aoi_shp,
                 paste0(tmp_dir,"tmp_gfc_map_",the_basename,".tif"),
                 paste0(tmp_dir,"tmp_gfc_map_clip_",the_basename,".tif"),
                 aoi_field
  ))
  
  ###############################################################################
  ################### REPROJECT IN EA PROJECTION
  ###############################################################################
  system(sprintf("gdalwarp -t_srs \"%s\" -overwrite -ot Byte -multi -co COMPRESS=LZW %s %s",
                 proj,
                 paste0(tmp_dir,"tmp_gfc_map_clip_",the_basename,".tif"),
                 paste0(tmp_dir,"tmp_gfc_map_clip_prj_",the_basename,".tif")
  ))
  
  
  ################################################################################
  #################### Add pseudo color table to result
  ################################################################################
  system(sprintf("(echo %s) | oft-addpct.py %s %s",
                 paste0(gfc_dir,"color_table.txt"),
                 paste0(tmp_dir,"tmp_gfc_map_clip_prj_",the_basename,".tif"),
                 paste0(tmp_dir,"tmp_gfc_map_clip_prj_pct",the_basename,".tif")
  ))
  
  ################################################################################
  #################### COMPRESS
  ################################################################################
  system(sprintf("gdal_translate -ot Byte -co COMPRESS=LZW %s %s",
                 paste0(tmp_dir,"tmp_gfc_map_clip_prj_pct",the_basename,".tif"),
                 paste0(gfc_dir,"gfc_",the_basename,"_",threshold,"_map_clip_pct.tif")
  ))
  
  ################################################################################
  #################### Add pseudo color table to result (GEO)
  ################################################################################
  system(sprintf("(echo %s) | oft-addpct.py %s %s",
                 paste0(gfc_dir,"color_table.txt"),
                 paste0(tmp_dir,"tmp_gfc_map_clip_",the_basename,".tif"),
                 paste0(tmp_dir,"tmp_gfc_map_clip_pct",the_basename,".tif")
  ))
  
  ################################################################################
  #################### COMPRESS
  ################################################################################
  system(sprintf("gdal_translate -ot Byte -co COMPRESS=LZW %s %s",
                 paste0(tmp_dir,"tmp_gfc_map_clip_pct",the_basename,".tif"),
                 paste0(gfc_dir,"gfc_",the_basename,"_",threshold,"_map_clip_pct_geo.tif")
  ))
  
}
