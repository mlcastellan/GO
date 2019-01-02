#############################################
### esta funcion calcula rasters de media ###
### y desvio estandar para una coleccion  ###
### de n rasters ndvi                     ###
### input: carpeta con rasters ndvi       ###
### output: raster media y raster sd      ###
#############################################
GO_E5_NDVI_MEAN_SD=function(ndvi_dir="NDVI/Trim/",ndvi_mean_out="NDVI/Mean/NDVI_mean.tif",ndvi_sd_out="NDVI/Mean/NDVI_sd.tif"){
  ##
  library(raster)
  library(sp)
  library(rgdal)
  ## genero una lista de los ndvi en ndvi_dir
  ndvi_files <- list.files(ndvi_dir,pattern=".tif$",ignore.case=TRUE,full.names=TRUE)
  ## genero un stack con los ndvi unibanda
  ndvi_stack=raster::stack(ndvi_files)
  ## habilito procesamiento en paralelo
  #cores <- 4
  #beginCluster(cores, type='SOCK')
  ## calculo para ndvi_stack la media
  ndvi_mean=raster::calc(ndvi_stack,fun=mean,na.rm=TRUE)
  ## calculo para ndvi_stack la SD
  ndvi_sd=raster::calc(ndvi_stack,fun=sd)
  ## deabilito el cluster
  #endCluster()
  ## genero un stack con mean y sd
  ndvi_mean_stack=stack(ndvi_mean,ndvi_sd)
  ## exporto la media y SD idividualmente para control
  writeRaster(ndvi_mean,filename=file.path(getwd(),ndvi_mean_out), bylayer=TRUE,format="GTiff",overwrite=TRUE)
  writeRaster(ndvi_sd,filename=file.path(getwd(),ndvi_sd_out), bylayer=TRUE,format="GTiff",overwrite=TRUE)
  ## la funcion devuelve el raster stack
  return(ndvi_mean_stack)
  }
############### fin de la funcion #########