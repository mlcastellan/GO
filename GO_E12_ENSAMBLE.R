

###### PASO 0 CREAR LAS CARPETAS PARA EL PROCESAMIENTO
create_Directories_for_processing()
###### PASO 1 GENERAR NDVI--->GO_E3_NDVI
GO_E3_NDVI_SEN2COR(sentinel_2_folder="/home/martin-r/05_Rasters/SENTINEL_HDE_invierno",res=10)
###### PASO 2 RECORTAR NDVI--->GO_E4_PROJECT_TRIM
multi_crop(raster_folder="NDVI/Full/",out_dir="NDVI/Trim/",SHP_folder="SHP_IN")
###### PASO 3 CALCULO DE MEDIA-SD --->GO_E5_NDVI_MEAN_SD
GO_E5_NDVI_MEAN_SD(ndvi_dir="NDVI/Trim/",ndvi_mean_out="NDVI/Mean/NDVI_mean.tif",ndvi_sd_out="NDVI/Mean/NDVI_sd.tif")
  ###### ELEGIR PARA CADA CASO CUAL USAR---------------------------
###### PASO 4A INVARIANTES POR PERCENTIL--->GO_E6_NDVI_INVARIANTS
ndvi_invs=GO_E6_NDVI_INVARIANTS(mean=raster("./NDVI/NDVI_mean.tif"),sd=raster("./NDVI/NDVI_sd.tif"),percentil=0.99)
###### PASO 4B INVARIANTES POR CANTIDAD--->GO_E11_NDVI_INVARIANTS
ndvi_invs=GO_11_NDVI_NQTY(mean=raster("./NDVI/NDVI_mean.tif"),sd=raster("./NDVI/NDVI_sd.tif"),n_elements=2000)
######-----------------------------------------------------------
###### PASO 5 GENERAR SHAPEFILE DE INVARIANTES--->GO_E8_NDVI_SHP_INVARIANTS
###### USAR luego de GO_E06
GO_E8_NDVI_INVARIANTS_SHP=function(ndvi_invs)
 
###### PASO 6 OBTENER COEF DE REGRESION LINEAL--->GO_E09_LINEAR_REGRESSION
###### PASO 7 TRANSFORMACION LINEAL DEL RASTER--->GO_E10_RASTER_LINEAR_TRANSF





########################################################
########################################################
###            SECCION DE FUNCIONES                 ####
########################################################
########################################################
create_Directories_for_processing<-function(){
  
  #### Check if folders exist. 
  # Sentinel Directories
  directoryExists("GO/NDVI/Full")
  directoryExists("GO/NDVI/Trim")
  directoryExists("GO/NDVI/Mean")
  directoryExists("GO/SHP_IN")
  directoryExists("GO/Invariants/Rasters")
  directoryExists("GO/Invariants/Spatial_points")
  
}
####################################################
####################################################
directoryExists<-function(directory){
  # check to see if there is a processing folder in workingDirectory 
  # and check that it is clear.
  if(dir.exists(directory)){
    message(paste("Nothing to do,",directory," exists"))
  }else{
    # if not the case, create it.
    
    dir.create(directory,recursive=TRUE)
  }
}
####################################################
####################################################
removeNDVIextremes<-function(r1){
  # remove values below -0.995, and make them -0.995
  r1[r1<-0.995]=-0.995
  # remove values above 0.995 and make them 0.995
  r1[r1>0.995]=0.995
  return(r1)
}
####################################################
####################################################

####################################
## Funcion NDVI para sentinel 2 ####
## tal como sale de sen2cor     ####
##                              ####
####################################
GO_E3_NDVI_SEN2COR=function(sentinel_2_folder="/home/martin-r/05_Rasters/SENTINEL_HDE_invierno",res=10){
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
    file.remove("nir_band.tif")
    ###cierro el for loop
  }
  ########
}
######## FIN DE LA FUNCION ######

####################################################
####################################################

###########################################
### esta funcion realiza un crop sobre ####
### un raster basado en un extent      ####
### input:raster y shapefile           ####
### output: raster recortado           ####
###########################################
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

####################################################
####################################################

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

####################################################
####################################################

#################################################
### esta funcion lee stack de mean y sd       ###
### y calcula filtra pix por debajo de        ###
### un threshold, o con un percentil          ###
###input: raster_stack con media-sd percent   ###
###output: 2 rasters con invariantes  ndvi    ###
#################################################
#setwd("C:/Users/mm/Documents/test_data/sentinel_mendoza_prueba/sentinel_mendoza_trimados")
#mean=raster(file.choose())
#sd=raster(file.choose())
#percentil=0.99
#ndvi_mean_stack=stack(mean,sd)
###
GO_E6_NDVI_INVARIANTS=function(mean="./NDVI/Mean/NDVI_mean.tif",sd="./NDVI/Mean/NDVI_sd.tif",percentil=0.99){
  library(raster)
  library(stringr)
  #########
  mean=raster(mean)
  sd=raster(sd)
  ndvi_mean_stack=stack(mean,sd)
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
  #ndvi_10m_positivos=calc(ndvi_sd_10m[[1]],fun=function(x){ifelse(x>=0,x,NA)})
  #---
  ndvi_bajo=raster::quantile(ndvi_sd_10m[[1]],1-percentil+0.01)##??? agregue un 0.01 para que no sea nulo
  ##------------------------------------------
  sd_ndvi=raster::quantile(ndvi_sd_10m[[2]],0.7)#Uso un threshold de 70% para el desvio standard
  #sd_ndvi_bajo=raster::quantile(ndvi_sd_10m[[2]],percentil)
  ##------------------------------------------
  ##---extraigo los puntos de ndvi alto
  dir_out_1="Invariants/Rasters/NDVI_alto_INV.tif"
  inv_alto<-overlay(ndvi_sd_10m,fun=function(x,y){ifelse(x>=ndvi_alto&y<=sd_ndvi,x,NA)})
  writeRaster(inv_alto,filename=dir_out_1,format="GTiff",overwrite=TRUE)
  ##---extraigo los puntos de ndvi bajo
  dir_out_2="Invariants/Rasters/NDVI_bajo_INV.tif"
  inv_bajo<-overlay(ndvi_sd_10m,fun=function(x,y){ifelse(x<=ndvi_bajo&y<=sd_ndvi,x,NA)})
  writeRaster(inv_bajo,filename=dir_out_2,format="GTiff",overwrite=TRUE)
  ##---- exporto los invariantes
  ndvi_invs=stack(inv_alto,inv_bajo)
  return(ndvi_invs)
}

####################################################
####################################################

###################################################
###  esta funcion lee stack de mean y sd        ###
###  y filtra una n cantidad de pixeles         ###
###  input: raster_stack con media-sd,n_elemets ###
###  output: 2 rasters con invariantes  ndvi    ###
###################################################
GO_11_NDVI_NQTY=function(mean=raster("./NDVI/NDVI_mean.tif"),sd=raster("./NDVI/NDVI_sd.tif"),n_elements)
{
  ##-------------
  library(raster)
  library(rgdal)
  library(sp)
  library(rgeos)
  library(stringr)
  ##-------------
  ndvi_mean_stack=stack(mean,sd)
  ##---extraigo los valores del raster
  mean_values=getValues(ndvi_mean_stack[[1]])
  sd_values=getValues(ndvi_mean_stack[[2]])
  ##---
  max_pix=tail(order(mean_values),n)
  min_pix=head(order(mean_values),n)
  ##----
  max_sd=tail(order(sd_values),n)
  min_sd=head(order(sd_values),n)
  ##---
  max_ndvi=max_pix[max_pix %in% min_sd]
  min_ndvi=min_pix[min_pix %in% min_sd]
  inv_ndvi=c(max_ndvi,min_ndvi)
  ##---
  interest_values <- rep(NA,length(mean_values))
  interest_values[inv_ndvi]=mean_values[inv_ndvi]
  ##---
  #interest_sd_values=interest_values[ind_ndvi]
  ndvi_invs=setValues(ndvi_mean_stack[[1]],interest_values)
  ndvi_invs=stack(ndvi_invs,sd)
  #setvalues(ndvi_mean_stack[[2]],interest_sd_values)
  return(ndvi_invs)
}

############### fin de la funcion #########

####################################################
####################################################

GO_E8_NDVI_INVARIANTS_SHP=function(ndvi_invs){
  
  library(raster)
  library(rgdal)
  library(sp)
  ##--------------
  inv_alto_spatial=rasterToPoints(ndvi_invs[[1]],fun=NULL,spatial=TRUE)
  inv_alto_spatial$TIPO="ALTO"
  #writeOGR(inv_alto_spatial, dsn=".",layer="inv_points_alto", driver = "ESRI Shapefile",overwrite=T)
  inv_bajo_spatial=rasterToPoints(ndvi_invs[[2]],fun=NULL,spatial=TRUE)
  inv_bajo_spatial$TIPO="BAJO"
  #writeOGR(inv_bajo_spatial, dsn=".",layer="inv_points_bajo", driver = "ESRI Shapefile",overwrite=T)
  ##--- merge both spatial points dataframe
  invs=bind(inv_alto_spatial,inv_bajo_spatial)
  ### projecto los puntos en lon lat
  invs=spTransform(invs,CRS("+init=epsg:4326"))
  ##--- exporto el shapefile
  writeOGR(invs, dsn="Invariants/Spatial_points",layer="ndvi_invariants", driver = "ESRI Shapefile",overwrite=T)
  
}

####################################################
####################################################

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
  
  library(raster)
  library(rgdal)
  library(sp)
  ##--------------
  inv_alto_spatial=rasterToPoints(ndvi_invs[[1]],fun=NULL,spatial=TRUE)
  inv_alto_spatial$TIPO="ALTO"
  #writeOGR(inv_alto_spatial, dsn=".",layer="inv_points_alto", driver = "ESRI Shapefile",overwrite=T)
  inv_bajo_spatial=rasterToPoints(ndvi_invs[[2]],fun=NULL,spatial=TRUE)
  inv_bajo_spatial$TIPO="BAJO"
  #writeOGR(inv_bajo_spatial, dsn=".",layer="inv_points_bajo", driver = "ESRI Shapefile",overwrite=T)
  ##--- merge both spatial points dataframe
  invs=bind(inv_alto_spatial,inv_bajo_spatial)
  ### projecto los puntos en lon lat
  invs=spTransform(invs,CRS("+init=epsg:4326"))
  ##--- exporto el shapefile
  writeOGR(invs, dsn="Invariants/Spatial_points",layer="ndvi_invariants", driver = "ESRI Shapefile",overwrite=T)
  
}

