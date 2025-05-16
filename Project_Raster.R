#===========================================================================================
# Reprojects raster of any type
#===========================================================================================
Project_Raster <- function(raster_orig,
                           crs = "+proj=longlat +datum=WGS84 +no_defs +type=crs")
{
  
  a <- rast(ncols=40, nrows=40, xmin=-110, xmax=-90, ymin=40, ymax=60,
            crs= crs)
  raster_orig <- rast(raster_orig)
  raster_orig <- project(raster_orig,crs(a))
  return(raster_orig)
  
}
# ------------------------------------------------------------------------------------------------