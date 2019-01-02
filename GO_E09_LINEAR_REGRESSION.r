################################################
## esta funcion extrae los valores de NDVI   ###
## en resolucion 10m para los invariantes    ###
## generados en pasos anteriores, luego      ###
## con esos datos se genera una regresion    ###
## lineal y con ello se transforma lineal    ###
##mente el shapfile original                 ###
################################################
## input: ndvi_sentinel_10m,ndvi_aereo,invs_shp
## output:ndvi_aereo_10m
################################################

################################################
GO_E9_LINEAR_REGRESSION=function(ndvi_sentinel_10m_file="NDVI/Mean/NDVI_mean.tif",invs_shp_file="Invariants/Spatial_points/ndvi_invariants.shp",ndvi_aereo_file="NDVI/Aereo/"){
  ##---cargo librerias
  library(raster)
  library(sp)
  library(rgdal)
  library(rgeos)
  ##-----------------
  ndvi_sentinel_10m=raster(ndvi_sentinel_10m_file)
  ndvi_aereo_file=list.files(ndvi_aereo_file,pattern="NDVI_mean.tif",ignore.case=TRUE,full.names=TRUE)
  ndvi_aereo=raster(ndvi_aereo_file[[1]])
  ##-----------------
  invs_shp=readOGR(invs_shp_file)
  ##-----------------
  ndvi_aereo_10m=projectRaster(ndvi_aereo,ndvi_sentinel_10m,res=10)
  ##-Reproyecto el shapefile al crs del raster
  invs_shp=spTransform(invs_shp,proj4string(ndvi_sentinel_10m))
  ##--En este paso extraigo los valores de pixel en las posic inv
  ##---uso un circular buffer de 20m
  ##---
  sentinel_values_ALTO <- extract(ndvi_sentinel_10m,         # raster layer
                      invs_shp[invs_shp$TIPO=="ALTO"],   # SPDF with centroids for buffer
                      buffer = 10,     # buffer size, units depend on CRS
                      fun=max,         # what to value to extract
                      df=TRUE) 
  sentinel_values_BAJO <- extract(ndvi_sentinel_10m,         # raster layer
                      invs_shp[invs_shp$TIPO=="BAJO"],   # SPDF with centroids for buffer
                                  buffer = 10,     # buffer size, units depend on CRS
                                  fun=min,         # what to value to extract
                                  df=TRUE) 
  ##---Combino los valores de Sentinel bajo y alto
  sentinel_values=bind(sentinel_values_ALTO,sentinel_values_BAJO)
  ##---combino los valores del dataframe
  aereo_10m_values_alto=extract(ndvi_aereo_10m,         # raster layer
                           invs_shp[invs_shp$TIPO=="ALTO"],   # SPDF with centroids for buffer
                           buffer = 10,     # buffer size, units depend on CRS
                           fun=max,         # what to value to extract
                           df=TRUE) 
  aereo_10m_values_bajo=extract(ndvi_aereo_10m,         # raster layer
                                invs_shp[invs_shp$TIPO=="BAJO"],   # SPDF with centroids for buffer
                                buffer = 10,     # buffer size, units depend on CRS
                                fun=min,         # what to value to extract
                                df=TRUE) 
  ##---Combino los valores de aereo bajo y alto
  aereo_10m_values=bind(aereo_10m_values_ALTO,aereo_10m_values_BAJO)
  ##---combino ambos dataframes
  combined_values=merge.data.frame(sentinel_values,aereo_10m_values,by="ID")
  names(combined_values)=(c("ID","sentinel_10m","aereo_10m"))
  ##---genero una regresion lineal
  ##--- en x tengo el valor de imagen aerea a 10m
  ##--- en y tengo el valor de sentinel a 10m
  linear_regression<- lm(sentinel_10m ~ aereo_10m, combined_values)
  ##### chequear si esta bien realziada la transformacion lineal y si no estan invertidos los ejes
  intercept=as.numeric(linear_regression$coefficients[1])
  slope=as.numeric(linear_regression$coefficients[2])
  ###☻---Exporto estos valores de la funcion
  linear_coef=c(intercept,slope)
  return(linear_coef)
  ###-----------------------
  }
###############---FIN DE LA FUNCION---################################
