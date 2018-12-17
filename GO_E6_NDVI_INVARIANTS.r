#################################################
### esta funcion lee stack de mean y sd       ###
### y calcula filtra pix por debajo de        ###
### un threshold, o con un percentil          ###
###input: raster_stack con media-sd percent   ###
###output: 2 rasters con invariantes  ndvi    ###
#################################################
setwd("C:/Users/mm/Documents/test_data/sentinel_mendoza_prueba/sentinel_mendoza_trimados")
mean=raster(file.choose())
sd=raster(file.choose())
percentil=0.99
ndvi_mean_stack=stack(mean,sd)
###
GO_E6_NDVI_INVARIANTS=function(ndvi_mean_stack=ndvi_mean_stack,percentil){
library(raster)
library(stringr)
#########
##---busco la resolucion del stack
res=as.numeric(str_split(res(mean),pattern=" ",n=1)[[1]])
##---calculo el factor para cambiar resolucion
new_res=10.00/res
ndvi_sd_10m=aggregate(ndvi_mean_stack,fact=new_res,fun=median)#####. cambio resolucion con mediana no con mean
##----calculo de ndvi, a posterior usar la funcion de ndvi que esta ya programada
#ndvi_10m=(stack_10m[[4]]-stack_10m[[3]])/(stack_10m[[3]]+stack_10m[[4]])
#########
##---Calculo de thresholds para filtrar el stack
ndvi_alto=raster::quantile(ndvi_sd_10m[[1]],probs=percentil)
##---
ndvi_10m_positivos=calc(ndvi_sd_10m[[1]],fun=function(x){ifelse(x>=0,x,NA)})
#---
ndvi_bajo=raster::quantile(ndvi_10m_positivos,1-percentil+0.01)##??? agregue un 0.01 para que no sea nulo
##------------------------------------------
sd_ndvi=raster::quantile(ndvi_sd_10m[[2]],0.7)#Uso un threshold de 70% para el desvio standard
#sd_ndvi_bajo=raster::quantile(ndvi_sd_10m[[2]],percentil)
##------------------------------------------
##---extraigo los puntos de ndvi alto
dir_out_1="GO/Invariants/Rasters/NDVI_alto_INV.tif"
inv_alto<-overlay(ndvi_sd_10m,fun=function(x,y){ifelse(x>=ndvi_alto&y<=sd_ndvi,x,NA)})
writeRaster(inv_alto,filename=dir_out_1,format="GTiff",overwrite=TRUE)
##---extraigo los puntos de ndvi bajo
dir_out_1="GO/Invariants/Rasters/NDVI_bajo_INV.tif"
inv_bajo<-overlay(ndvi_sd_10m,fun=function(x,y){ifelse(x<=ndvi_bajo&y<=sd_ndvi,x,NA)})
writeRaster(inv_bajo,filename=dir_out_2,format="GTiff",overwrite=TRUE)
##---- exporto los invariantes
ndvi_invs=stack(inv_alto,inv_bajo)
return(ndvi_invs)
}
############### fin de la funcion #########