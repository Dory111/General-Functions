# rast1 <- rast('C:/Users/ChristopherDory/LWA Dropbox/Christopher Dory/Projects/598/598.07/5c Shasta_ISW/Data/DEM/USGS_13_n42w123_20231102.tif')
# rast2 <- rast('C:/Users/ChristopherDory/LWA Dropbox/Christopher Dory/Projects/598/598.07/5c Shasta_ISW/Data/DEM/USGS_13_n42w124_20250812.tif')
# rast <- merge(rast2,rast1)
# ext <- st_read('C:/Users/ChristopherDory/LWA Dropbox/Christopher Dory/Projects/598/598.07/5c Shasta_ISW/Shapefiles/Accessory_Shapefiles/Scott_HUC_12.shp')
# plot(rast, add = F)
# plot(st_geometry(ext), add = T)
# rast <- crop(rast, ext(ext))
# rast <- mask(rast, ext)
# Watershed_Delineator(raster = rast,
#                      out_dir = 'C:/Users/ChristopherDory/LWA Dropbox/Christopher Dory/Projects/598/598.07/5c Shasta_ISW/Shapefiles/Accessory_Shapefiles',
#                      flow_dir_rast_name = 'Scott_Valley_Flow_Dir')
library(raster)
library(terra)
library(sf)
# ==================================================================================================
# Delineates watersheds
# ==================================================================================================
Watershed_Delineator <- function(raster,
                                 out_dir,
                                 flow_dir_rast_name = NULL)
{
  ################################## ERRORS ########################################################
  # ------------------------------------------------------------------------------------------------
  # not a raster error
  if(class(raster)[1] != 'SpatRaster'){
    if(class(raster)[1] != 'RasterLayer'){
      stop(paste0('Watershed_Delineator:\n\n',
                  'Inputs not in the form of a raster\n'))
    } else{
      raster <- rast(raster)
    }
  }
  # ------------------------------------------------------------------------------------------------
  
  
  
  
  ################################## CORRECTIONS ###################################################
  # ------------------------------------------------------------------------------------------------
  # accounting for no entered name
  if(is.null(flow_dir_rast_name) == TRUE){
    flow_dir_rast_name <- 'flow_dir_rast'
  }
  # ------------------------------------------------------------------------------------------------
  
  
  
  
  

  ################################## FLOW DIRECTION RASTER #########################################
  # ------------------------------------------------------------------------------------------------
  # padding values
  theta_rad <- list()
  values <- values(raster)
  nrow <- nrow(raster)
  ncol <- ncol(raster)
  values <- matrix(values,
                   nrow = nrow,
                   ncol = ncol,
                   byrow = TRUE)
  values <- rbind(values,
                  rep(NA,ncol))
  values <- rbind(rep(NA,ncol),
                  values)
  values <- cbind(values,
                  rep(NA,nrow+2))
  values <- cbind(rep(NA,nrow+2),
                  values)
  # ------------------------------------------------------------------------------------------------
  
  # ------------------------------------------------------------------------------------------------
  # getting iteratble quantities and notifying user
  diff_x <- res(raster)[1]
  diff_y <- res(raster)[2]
  counter <- 0
  
  mils <- ncell(raster)/1e6
  seconds <- mils*159
  minutes <- round(seconds/60,2)
  cat(paste0('For details on the machine these functions were tested on\n',
             'please call wdl_machine_specs()\n\n'))
  cat(paste0('Compiling flow direction raster\n',
             'based on historical performance \n',
             'and the size of your raster this will take:\n',
             minutes,' minutes\n\n'))
  # ------------------------------------------------------------------------------------------------
  
  # ------------------------------------------------------------------------------------------------
  # getting flow directions
  for(i in 1:nrow(raster)){
    loading_bar(i,
                nrow(raster),
                width = 50)
    for(j in 1:ncol(raster)){
      counter <- counter +1
      theta_rad[[counter]] <- flow_dir_of_DEM(raster = raster,
                                values = values,
                                row = i,
                                column = j,
                                diff_x = diff_x,
                                diff_y = diff_y)[[2]]


    }
  }
  # ------------------------------------------------------------------------------------------------

  # ------------------------------------------------------------------------------------------------
  # building raster and writing out
  theta_rad <- as.numeric(theta_rad)
  theta_rast <- rast(ncol = ncol(raster),
                     nrow = nrow(raster),
                     crs = crs(raster),
                     xmin = xmin(raster),
                     xmax = xmax(raster),
                     ymin = ymin(raster),
                     ymax = ymax(raster))
  values(theta_rast) <- theta_rad
  writeRaster(theta_rast,
              file.path(out_dir,paste0(flow_dir_rast_name,'.tif')),
              overwrite = TRUE)
  # ------------------------------------------------------------------------------------------------
}
# ------------------------------------------------------------------------------------------------



# ==================================================================================================
# Function to give user information to compare their computer
# to the testing computer
# ==================================================================================================
wdl_machine_specs <- function()
{
  cat(paste0('Watershed delineation functions were tested on a\n',
             'Lenovo thinkpad with:\n',
             '16 GB of RAM\n',
             'and a Intel i7-1365U, 1800Mhz 10 core processor\n\n'))
}
# ------------------------------------------------------------------------------------------------

# ==================================================================================================
# Simple function to concatenate a loading bar
# ==================================================================================================
loading_bar <- function(iter, total, width = 50) 
{
  # ------------------------------------------------------------------------------------------------
  # percent completion
  pct <- iter / total
  # ------------------------------------------------------------------------------------------------
  
  # ------------------------------------------------------------------------------------------------
  # how much to fill the bar
  filled <- round(width * pct)
  bar <- paste0(rep("=", filled), collapse = "")
  # ------------------------------------------------------------------------------------------------
  
  # ------------------------------------------------------------------------------------------------
  # how much is left
  space <- paste0(rep(" ", width - filled), collapse = "")
  # ------------------------------------------------------------------------------------------------
  
  # ------------------------------------------------------------------------------------------------
  cat(sprintf("\r[%s%s] %3d%%",
              bar,
              space,
              round(100*pct)))
  # ------------------------------------------------------------------------------------------------
  
  # ------------------------------------------------------------------------------------------------
  if (iter == total){
    cat("\n")
  }
  # ------------------------------------------------------------------------------------------------
}
# ------------------------------------------------------------------------------------------------
