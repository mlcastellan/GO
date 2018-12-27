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
  ndvi_mean_stack=stack(mean,stack)
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
  #setvalues(ndvi_mean_stack[[2]],interest_sd_values)
  return(ndvi_invs)
}
