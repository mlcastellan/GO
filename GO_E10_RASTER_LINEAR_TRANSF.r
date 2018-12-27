############################################
## Esta funcion realiza una             ####
## transformacion lineal de un raster   ####
## con los parametros de regresion      ####
## obtenidos del script anterior        ####
## la salida es un raster modificado    ####
## en base a slope & interception del lm####
############################################
############################################
linear_coef#vector del script anterior
ndvi_file=file.choose()#raster a transformar



GO_10_RASTER_LINEAR_TRANS=function(linear_coef,ndvi_file){
  ##---agrego las librerias
  library(raster)
  library(sp)
  library(rgdal)
  ##---cargo el raster
  r=raster(ndvi_file)
  ##---extraigo los valores del raster
  r_values=getValues(r)
  ## aplico transformacion y =ax+b
  a=linear_coef[2]
  b=linear_coef[1]
  r_values=a*r_values+b
  ##---estampo en el raster los nuevos valores
  r=setValues(r,r_values)
  return(r)
}