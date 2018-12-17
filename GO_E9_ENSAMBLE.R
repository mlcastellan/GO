create_Directories_for_sen2cor<-function(){

  #### Check if folders exist. 
  # Sentinel Directories
  directoryExists("GO/sentinel_2_download")
  directoryExists("GO/Sentinel_1C")
  directoryExists("GO/Sentinel_2A")
  directoryExists("GO/NDVI/Full")
  directoryExists("GO/NDVI/Trim")
  directoryExists("GO/NDVI/Mean")
  directoryExists("GO/NDVI/SD")
  directoryExists("GO/Invariants/Rasters")
  directoryExists("GO/Invariants/Spatial_points")
    
}

###### PASO 1 DESCARGA SENTINEL

###### PASO 2 EJECUTAR SEN2COR
###### PASO 3 NDVI FULL
###### PASO 4 NDVI TRIM
###### PASO 5 NDVI MEAN
###### PASO 6 NDVI SD
###### PASO 7 INVARIANTES RASTER
###### PASO 8 INVARIANTES SHP





########################################################
#######################################################

directoryExists<-function(directory) {
  # check to see if there is a processing folder in workingDirectory 
  # and check that it is clear.
  if(dir.exists(directory)){
    message(paste("Nothing to do,",directory," exists"))
  }else{
    # if not the case, create it.
    
    dir.create(directory,recursive=TRUE)
  }

####################################################
####################################################
removeNDVIextremes<-function(r1){
  # remove values below -0.995, and make them -0.995
  r1[r1<-0.995]=-0.995
  # remove values above 0.995 and make them 0.995
  r1[r1>0.995]=0.995
  return(r1)
}