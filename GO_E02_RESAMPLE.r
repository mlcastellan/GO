##########################################
### esta funcion hace un resample de   ###
### los rasters que estan en un dir    ###
### para que todos tengan la misma res ###
### en forma previa al calculo de ndvi ###
### y posterior recorte de los mismos  ###
### input:raster y shapefile           ###
### output: raster recortado           ###
##########################################
### esta funcion se aplica sobre los rasteraereos
##########################################
library(raster)
library(tools)
##--------------------

raster_dir_1=("C:/Users/mm/Documents/test_data/BGRNIR-Pruebas/Monteviejo_vistaflores")#variable para la funcion

stack_list=list.files(path=raster_dir_1,pattern=".IMG$",recursive=FALSE,ignore.case=TRUE,full.names=TRUE)


##---busco la mayor resolucion de los stacks
res=vector()
for(i in 1:length(stack_list)){
r=stack(stack_list[[i]]) 
res[i]=as.numeric(str_split(res(r),pattern=" ",n=1)[[1]])

}
##---selecciono la maxima resolucion
max_res=max(res)
index_max_res=which.max(res)
##---hago un resample de todos los rasters
## de una carpeta a esa resolucion
for(i in 1:length(stack_list)){
r=stack(stack_list[[i]])
r=resample(r,raster(stack_list[[index_max_res]]))
##---sobrescribo el raster con la nueva resolucion
export_name=basename(stack_list[[i]])
export_name=tools::file_path_sans_ext(export_name)
export_name=paste(export_name,".tif",sep="")
writeRaster(r,filename=export_name,format="GTiff",overwrite=TRUE,options="INTERLEAVE=BAND")
}
