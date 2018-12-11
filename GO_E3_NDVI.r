rm(list=ls())
sentinel_2_folder="/home/martin-r/Rasters/SENTINEL_HDD_invierno/S2_2A"
res=10
###################################
## Funcion NDVI para sentinel 2 ###
## tal como sale de sen2cor     ###
##                              ###
###################################

GO_E3_NDVI_SEN2COR=function(sentinel_2_folder,res){
######
library(raster)
library(rgdal)
library(gdalUtils)
library(stringr)
#### ETAPA 1: Paso previo a generar el NDVI

#### GENERO los patterns a buscar en las imagenes S2_2A
pattern_r=paste("B04_",res,"m.jp2$",sep="")
pattern_nir=paste("B08_",res,"m.jp2$",sep="")
##### creo dos listas de las bandas R y NIR en el folder  S2_A
R_band_list=list.files(path=sentinel_2_folder,pattern=pattern_r,recursive=TRUE,ignore.case=TRUE,full.names=TRUE)
NIR_band_list=list.files(path=sentinel_2_folder,pattern=pattern_nir,recursive=TRUE,ignore.case=TRUE,full.names=TRUE)
### creo una lista para los nombres del ndvi a generar
ndvi_filename_list=list()
### genero los nombres del ndvi para cada ndvi a generar
for(j in 1:length(R_band_list)){
ndvi_filename_list[[j]]=paste("NDVI_",str_replace(basename(R_band_list[j]),pattern="_B04_10m.jp2",replacement=".tif"),sep="")
}
### ETAPA 2: Generar el NDVI y exportarlo.

######################################
## para trabajar con jpg 2000 tal  ###
## como sale de sen2cor esta fun   ###
######################################
for(i in 1:length(R_band_list)){
###
rband_name=R_band_list[[i]]
nirband_name=NIR_band_list[[i]]
###
ndvi_filename=ndvi_filename_list[[i]]
dir=paste("NDVI/Full/",ndvi_filename,sep="")
###paso no necesario
#r_band=readGDAL(rband_name)
#nir_band=readGDAL(nirband_name)
### Conviero JPG2000 con Rgdal en Geotiff.
r_band=gdal_translate(rband_name,"r_band.tif")
nir_band=gdal_translate(nirband_name,"nir_band.tif")
### Importo el Geotiff como Raster
r_band<- raster("r_band.tif")
nir_band<- raster("nir_band.tif")
### Genero el NDVI
ndvi=(nir_band-r_band)/(nir_band+r_band)
### Exporto el raster NDVI, con el nombre y path dado en dir.
writeRaster(ndvi,filename=dir,format="GTiff",overwrite=TRUE)
### opcion para devolver un objeto en vez de exportar el raster
#return(ndvi)
### Elimino los tif que use para tranformar
### jpg 2000 en tif
file.remove("r_band.tif")
file.remove("nir_band.tif"
###cierro el for loop
}
########
}
######## FIN DE LA FUNCION ######






















