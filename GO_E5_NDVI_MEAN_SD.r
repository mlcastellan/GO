#############################################
### esta funcion calcula rasters de media ###
### y desvio estandar para una coleccion  ###
### de n rasters ndvi                     ###
### input: carpeta con rasters ndvi       ###
### output: raster media y raster sd      ###
#############################################
ndvi_dir <- "ndvi directory"


GO_E5_NDVI_MEAN_SD=function(ndvi_dir="NDVI/Trim/"){
  ##
  library(raster)
  library(sp)
  library(rgdal)
  ## genero una lista de los ndvi en ndvi_dir
  ndvi_files <- list.files(ndvi_dir,pattern=".tif$",ignore.case=TRUE,full.names=TRUE)
  ## genero un stack con los ndvi unibanda
  ndvi_stack=stack(ndvi_files)
  ## habilito procesamiento en paralelo
  cores <- 4
  beginCluster(cores, type='SOCK')
  ## calculo para ndvi_stack la media
  ndvi_mean=calc(ndvi_stack,fun=mean)
  ## calculo para ndvi_stack la SD
  ndvi_sd=calc(ndvi_stack,fun=sd)
  ## deabilito el cluster
  endCluster()
  ## genero un stack con mean y sd
  ndvi_mean_stack=stack(ndvi_mean,ndvi_sd)
  ## la funcion devuelve el raster stack
  return(ndvi_mean_stack)
  ## exporto la media y SD idividualmente para control
  ndvi_mean_out="NDVI/NDVI_mean.tif"
  ndvi_sd_out="NDVI/NDVI_sd.tif"
  writeRaster(ndvi_mean,filename=ndvi_mean_out,format="GTiff",overwrite=TRUE)
  writeRaster(ndvi_sd,filename=ndvi_sd_out,format="GTiff",overwrite=TRUE)
  
  }
############### fin de la funcion #########