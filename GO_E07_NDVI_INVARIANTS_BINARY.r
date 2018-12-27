#################################################
### esta funcion convierte en una mascara     ###
### binaria el raster de invariantes de ndvi  ###
### funcion opcional por un comentaio de Pedro###
###input: raster_stack de invariantes         ###
###output: raster_layer de invariantes 0-1    ###
#################################################

ndvi_invs <- "seleccionarlo si no esta del paso anterior"

GO_E7_NDVI_INVARIANTS_BINARY=function(ndvi_invs){
  ### valores no nulos son 1
  ndvi_invs[!is.na(ndvi_invs)]=1.00
  ### valores sin datos (NA),son cero
  ndvi_invs[is.na(ndvi_invs)]=NA
  ## devuelvo el raster binario
  return(ndvi_invs)
  
}
############### fin de la funcion #########