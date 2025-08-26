# rast1 <- rast('C:/Users/ChrisDory/LWA Dropbox/Christopher Dory/Projects/598/598.07/5c Shasta_ISW/Data/DEM/USGS_13_n42w123_20231102.tif')
# rast2 <- rast('C:/Users/ChrisDory/LWA Dropbox/Christopher Dory/Projects/598/598.07/5c Shasta_ISW/Data/DEM/USGS_13_n42w124_20250812.tif')
# rast <- merge(rast2,rast1)
# ext <- st_read('C:/Users/ChrisDory/LWA Dropbox/Christopher Dory/Projects/598/598.07/5c Shasta_ISW/Shapefiles/Accessory_Shapefiles/Scott_HUC_12.shp')
# 
# rast <- crop(rast, ext(ext[ext$name == 'South Fork Scott River', ]))
# rast <- mask(rast, ext[ext$name == 'South Fork Scott River', ])
# Watershed_Delineator(raster = rast,
#                      out_dir = 'C:/Users/ChrisDory/LWA Dropbox/Christopher Dory/Projects/598/598.07/5c Shasta_ISW/Shapefiles/Accessory_Shapefiles',
#                      flow_dir_rast_name = 'South_Fork_Flow_Dir_Test')
# a <- rast('C:/Users/ChrisDory/LWA Dropbox/Christopher Dory/Projects/598/598.07/5c Shasta_ISW/Shapefiles/Accessory_Shapefiles/South_Fork_Flow_Dir_Test.tif')
# png(filename=file.path('C:/Users/ChristopherDory/LWA Dropbox/Christopher Dory/Projects/598/598.07/5c Shasta_ISW/Shapefiles/Accessory_Shapefiles/South_Fork_Slope.png'),
#     width=8, height=10, units="in", res=1000)
# plot(a,legend = T,
#      main = 'Slope of South Fork')
# b <- a
# c <- a
# values(b)[values(a) != 's'] <- NA
# values(b)[values(a) == 's'] <- 1
# values(c)[values(a) != 'f'] <- NA
# values(c)[values(a) == 'f'] <- 1
# plot(b, add = T, col = 'red', legend = F)
# plot(c, add = T, col = 'darkorange', legend = F)
# dev.off()

library(raster)
library(terra)
library(sf)
# ==================================================================================================
# Delineates watersheds
# ==================================================================================================
Watershed_Delineator <- function(raster,
                                 out_dir,
                                 flow_dir_rast_name = NULL,
                                 min_slope = 1,
                                 diff_x = NULL,
                                 diff_y = NULL,
                                 zunit = 'm')
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
  
  
  
  # ------------------------------------------------------------------------------------------------
  # calculate distance based on latlon
  Haversine_Formula <- function(LatA,LonA,
                                LatB,LonB,
                                Re = 6371)
  {
    LatA <- LatA * (3.14159/180)
    LonA <- LonA * (3.14159/180)
    LatB <- LatB * (3.14159/180)
    LonB <- LonB * (3.14159/180)
    
    dlat <- LatB - LatA
    dlon <- LonB - LonA
    
    a <- sin(dlat / 2)^2 + cos(LatA) * cos(LatB) * sin(dlon / 2)^2
    c <- 2 * atan2(sqrt(a),sqrt(1-a))
    
    d <- Re * c
    return(d)
  }
  # ------------------------------------------------------------------------------------------------
  
  # ------------------------------------------------------------------------------------------------
  # getting km res from arc second raster
  axis <- strsplit(crs(raster),'\n')[[1]]
  axis <- axis[grep('AXIS',axis)] %>%
    trimws() %>% strsplit("\"")
  if(length(grep('Lat',axis)) > 0){ # can latitude be found in the axis def, if so its latlon
    if(is.null(diff_x) == TRUE |
       is.null(diff_y) == TRUE){
      y <- (ymin(raster) + ymax(raster))/2
      x <- (xmin(raster) + xmax(raster))/2
      diff_x <- Haversine_Formula(y, x,
                                  y, x + res(raster)[1]) * 1000
      diff_y <- Haversine_Formula(y, x,
                                  y + res(raster)[2],x) * 1000
      
      if(zunit == 'ft'){
        diff_x <- diff_x * 3.28
        diff_y <- diff_y * 3.28
      }
    }
  }
  # ------------------------------------------------------------------------------------------------
  

  ################################## FLOW DIRECTION RASTER #########################################
  # ------------------------------------------------------------------------------------------------
  # padding values
  flow_dir_deg_output <- list()
  flow_dir_rad_output <- list()
  flow_dir_slope_output <- list()
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
      counter <- counter + 1

      output <- flow_dir_of_DEM(raster = raster,
                                values = values,
                                row = i,
                                column = j,
                                diff_x = diff_x,
                                diff_y = diff_y,
                                min_slope = min_slope)
      flow_dir_deg_output[[counter]] <- output[[2]]
      flow_dir_rad_output[[counter]] <- output[[1]]
      flow_dir_slope_output[[counter]] <- output[[3]]
    }
  }
  # ------------------------------------------------------------------------------------------------

  # ------------------------------------------------------------------------------------------------
  # building raster and writing out
  flow_dir_deg_output <- as.numeric(flow_dir_deg_output)
  flow_dir_rad_output <- as.numeric(flow_dir_rad_output)
  flow_dir_slope_output <- as.numeric(flow_dir_slope_output)
  flow_dir_rast <- rast(ncol = ncol(raster),
                        nrow = nrow(raster),
                        crs = crs(raster),
                        xmin = xmin(raster),
                        xmax = xmax(raster),
                        ymin = ymin(raster),
                        ymax = ymax(raster))
  flow_dir_deg_rast <- flow_dir_rast
  flow_dir_rad_rast <- flow_dir_rast
  flow_dir_slope_rast <- flow_dir_rast
  
  values(flow_dir_deg_rast) <- flow_dir_deg_output
  values(flow_dir_rad_rast) <- flow_dir_rad_output
  values(flow_dir_slope_rast) <- flow_dir_slope_output
  stack <- c(flow_dir_deg_rast,
             flow_dir_rad_rast,
             flow_dir_slope_rast)
  names(stack) <- c('degrees','radians','slope')
  plot(stack[[3]])
  
  writeRaster(stack,
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
             'Lenovo thinkpad with:\n\n',
             '16 GB of RAM\n',
             'and an Intel i7-1365U, 1800Mhz 10 core processor\n\n'))
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
