#################### SKIP IF OUTPUTS EXISTS ALREADY
if(
  !file.exists(paste0(gfc_dir,"mask_mspa_gfc_",the_basename,"_",threshold,".tif"))
   ){
  
  #############################################################
  ### CREATE A FOREST MASK FOR MSPA ANALYSIS
  system(sprintf("gdal_calc.py -A %s --co COMPRESS=LZW --outfile=%s --calc=\"%s\"",
                 paste0(gfc_dir,"gfc_",the_basename,"_",threshold,"_map_clip_pct.tif"),
                 paste0(gfc_dir,"mask_mspa_gfc_",the_basename,"_",threshold,".tif"),
                 paste0("(A==40)*2+((A>0)*(A<40)+(A>40))*1")
  ))
}




