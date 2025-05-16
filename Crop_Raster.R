#===========================================================================================
# Crops raster
#===========================================================================================
Crop_Raster <- function(raster_orig,
                        shape,
                        mask)
{
  # raster_orig <- rast(raster_orig)
  raster_orig <- crop(raster_orig,extent(shape))
  if(mask == T){
    raster_orig <- mask(raster_orig,shape)
  }
  return(raster_orig)
  
}
# ------------------------------------------------------------------------------------------------

