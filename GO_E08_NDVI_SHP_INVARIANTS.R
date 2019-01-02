#################################################
### esta funcion extrae las coordenadas       ###
### de los puntos invariantes de un raster    ###
### sea el binario o el que contiene el valor ###
### de ndvi y genera un spatial points        ###
### con  esos puntos                          ###
### input: raster con invariantes(resto NA)   ###
### output: shapefile con coordenadas de invS ###
#################################################

GO_E8_NDVI_INVARIANTS_SHP=function(ndvi_invs){
  ##--------------
  library(raster)
  library(rgdal)
  library(sp)
  ##--------------
  if(names(ndvi_invs)[[1]]=="inv_alto"){
    inv_alto_spatial=rasterToPoints(ndvi_invs[[1]],fun=NULL,spatial=TRUE)
    inv_alto_spatial$TIPO="ALTO"
    #writeOGR(inv_alto_spatial, dsn=".",layer="inv_points_alto", driver = "ESRI Shapefile",overwrite=T)
    inv_bajo_spatial=rasterToPoints(ndvi_invs[[2]],fun=NULL,spatial=TRUE)
    inv_bajo_spatial$TIPO="BAJO"
    #writeOGR(inv_bajo_spatial, dsn=".",layer="inv_points_bajo", driver = "ESRI Shapefile",overwrite=T)
    ##--- merge both spatial points dataframe
    invs=bind(inv_alto_spatial,inv_bajo_spatial)
  }
  if(names(ndvi_invs[[1]])=="NDVI_mean"){
    invs=rasterToPoints(ndvi_invs[[1]],fun=NULL,spatial=TRUE)
  }
  ### projecto los puntos en lon lat
  invs=spTransform(invs,CRS("+init=epsg:4326"))
  ##--- exporto el shapefile
  writeOGR(invs, dsn="Invariants/Spatial_points",layer="ndvi_invariants", driver = "ESRI Shapefile",overwrite=TRUE)
  ##--------------------
}


