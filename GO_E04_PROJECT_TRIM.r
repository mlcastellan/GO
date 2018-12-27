  ##########################################
  ### esta funcion realiza un crop sobre ###
  ### un raster basado en un extent      ###
  ### input:raster y shapefile           ###
  ### output: raster recortado           ###
  ##########################################
  #r <- raster(file.choose())
  #shp=readOGR(file.choose())
S_35_clip_raster_2_extent_shapefile=function(r,SHP_folder="SHP_IN"){
    ##
    library(raster)
    library(sp)
    library(rgdal)
    ##
    shp=shapefile(list.files(path=SHP_folder,pattern=".shp$",full.names=TRUE,ignore.case=TRUE)[[1]])
    ### reprojecto el shapefile al crs del raster
    shp=spTransform(shp,proj4string(r))
    shp_extent=extent(shp)
    shp_extent=as(shp_extent,'SpatialPolygons')
    proj4string(shp_extent)=proj4string(shp)
    ## recorto el raster al extent del raster
    r2 <- crop(r,shp_extent,snap='in') 
    ## enmascaro el raster para solo tener los valores dentro del shapefile
    r3 <- mask(r2,shp_extent)
    ##
    return(r3)
    
  }
  ############### fin de la funcion #########
############### FIN #########
###-------------------------------#
#shp=readOGR(file.choose())
#raster_folder="C:/Users/mm/Documents/test_data/sentinel_mendoza_prueba"
###-------------------------------#
multi_crop=function(raster_folder="NDVI/Full",out_dir="NDVI/Trim/",shape=shp){
#r <- raster(file.choose())
raster_list=list.files(raster_folder,pattern=".tif$",full.names=TRUE,ignore.case=TRUE)
### ETAPA 3: Recorte de los rasters ndvi en la carpeta NDVI/Full
for(i in 1:length(raster_list)){
  raster=raster(raster_list[[i]])
  trim=S_35_clip_raster_2_extent_shapefile(raster,shape)
  out_name=paste(out_dir,"TRIM_",basename(raster_list[[i]]),sep="")
  writeRaster(trim,filename=out_name,format="GTiff",overwrite=TRUE)
}
}
 ############### FIN #########
