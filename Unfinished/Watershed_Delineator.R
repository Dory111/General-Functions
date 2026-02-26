# rast1 <- rast('C:/Users/ChrisDory/LWA Dropbox/Christopher Dory/Projects/598/598.07/5c Shasta_ISW/Data/DEM/USGS_13_n42w123_20231102.tif')
# rast2 <- rast('C:/Users/ChrisDory/LWA Dropbox/Christopher Dory/Projects/598/598.07/5c Shasta_ISW/Data/DEM/USGS_13_n42w124_20250812.tif')
# rast <- merge(rast2,rast1)
# ext <- st_read('C:/Users/ChrisDory/LWA Dropbox/Christopher Dory/Projects/598/598.07/5c Shasta_ISW/Shapefiles/Accessory_Shapefiles/Scott_HUC_12.shp')
# 
# rast <- crop(rast, ext(ext[ext$name == 'South Fork Scott River', ]))
# rast <- mask(rast, ext[ext$name == 'South Fork Scott River', ])

# rast <- rast('C:/Users/ChristopherDory/LWA Dropbox/Christopher Dory/Projects/712/712.01/T00-Vina-ISW/Data/Raster/USGS_13_n40w122_20250514.tif')
# ext <- st_read('C:/Users/ChristopherDory/LWA Dropbox/Christopher Dory/Projects/712/712.01/T00-Vina-ISW/Data/Shapefiles/Study_Area_Extent.shp')
# ext <- st_transform(ext,4269)
# rast <- crop(rast, ext(ext))
# rast <- project(rast, crs(st_transform(ext,4326)))
# 
# Watershed_Delineator(raster = rast,
#                      out_dir = 'C:/Users/ChristopherDory/LWA Dropbox/Christopher Dory/Projects/712/712.01/T00-Vina-ISW/Data/Raster',
#                      flow_dir_rast_name = 'Flow_Dir_Test')
# a <- rast('C:/Users/ChristopherDory/LWA Dropbox/Christopher Dory/Projects/712/712.01/T00-Vina-ISW/Data/Raster/Flow_Dir_Test.tif')
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
  ############################################################################################################
  ################################## HELPER FUNCTIONS ########################################################
  ############################################################################################################
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
  
  
  
  
  # ==================================================================================================
  # calculate distance based on latlon
  # ==================================================================================================
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
  
  
  
  
  
  #===========================================================================================
  # install required packages if not present
  #===========================================================================================
  require_package <- function(pkg)
  {
    if(require(pkg, character.only = TRUE) == FALSE){
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    } else {
      library(pkg, character.only = TRUE)
    }
  }
  # ------------------------------------------------------------------------------------------------
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ############################################################################################################
  ################################## MAIN FUNCTIONS ##########################################################
  ############################################################################################################
  
  
  # ==================================================================================================
  # Generates the likely direction of flow from a DEM (0-360 degrees, 90 being north)
  # based on D (inf) method of Tarboton 1997 (https://doi.org/10.1029/96WR03137)
  # ==================================================================================================
  flow_dir_of_DEM <- function(raster = test_rast,
                              values = NULL,
                              row = 2,
                              column = 2,
                              diff_x = NULL,
                              diff_y = NULL,
                              min_slope = 1)
  {
    
    # ==================================================================================================
    # Calculates the cross product of two vectors in matrix form
    # ==================================================================================================
    Cross_Product_Matrix <- function(V,
                                     U)
    {
      # ------------------------------------------------------------------------------------------------
      imat <- cbind(V[c(1:8) ,c(2:3)],
                    U[c(1:8) ,c(2:3)])
      jmat <- cbind(V[c(1:8) ,c(1,3)],
                    U[c(1:8) ,c(1,3)])
      kmat <- cbind(V[c(1:8) ,c(1:2)],
                    U[c(1:8) ,c(1:2)])
      # ------------------------------------------------------------------------------------------------
      
      # ------------------------------------------------------------------------------------------------
      # ad-bc
      ifinal <- (imat[,c(1)]*imat[,c(4)]) - (imat[,c(2)]*imat[,c(3)])
      jfinal <- (jmat[,c(2)]*jmat[,c(3)]) - (jmat[,c(1)]*jmat[,c(4)]) # bc - ad
      kfinal <- (kmat[,c(1)]*kmat[,c(4)]) - (kmat[,c(2)]*kmat[,c(3)])
      # ------------------------------------------------------------------------------------------------
      
      # ------------------------------------------------------------------------------------------------
      a <- -(ifinal/kfinal)
      b <- -(jfinal/kfinal)
      dir_of_travel <- atan2(-b,-a)
      inner <- ifinal**2 + jfinal**2 + kfinal**2
      inclination_of_plane <- acos(abs(kfinal/ sqrt(inner)))
      # ------------------------------------------------------------------------------------------------
      
      # ------------------------------------------------------------------------------------------------
      return(list(c(ifinal,jfinal,kfinal),
                  a,
                  b,
                  dir_of_travel,
                  inclination_of_plane))
      # ------------------------------------------------------------------------------------------------
    }
    # ------------------------------------------------------------------------------------------------
    
    
    # ==================================================================================================
    # Calculates the cross product of two vectors in vector form
    # ==================================================================================================
    Cross_Product <- function(V,
                              U)
    {
      # ------------------------------------------------------------------------------------------------
      imat <- matrix(nrow = 2,
                     ncol = 2,
                     data = c(V[2:3],
                              U[2:3]),
                     byrow = TRUE)
      jmat <- matrix(nrow = 2,
                     ncol = 2,
                     data = c(V[c(1,3)],
                              U[c(1,3)]),
                     byrow = TRUE)
      kmat <- matrix(nrow = 2,
                     ncol = 2,
                     data = c(V[1:2],
                              U[1:2]),
                     byrow = TRUE)
      # ------------------------------------------------------------------------------------------------
      
      # ------------------------------------------------------------------------------------------------
      # ad-bc
      ifinal <- (imat[1,1]*imat[2,2]) - (imat[1,2]*imat[2,1])
      jfinal <- (jmat[1,2]*jmat[2,1]) - (jmat[1,1]*jmat[2,2])
      kfinal <- (kmat[1,1]*kmat[2,2]) - (kmat[1,2]*kmat[2,1])
      # ------------------------------------------------------------------------------------------------
      
      # ------------------------------------------------------------------------------------------------
      a <- -(ifinal/kfinal)
      b <- -(jfinal/kfinal)
      dir_of_travel <- atan2(-b,-a)
      inner <- ifinal**2 + jfinal**2 + kfinal**2
      inclination_of_plane <- acos(abs(kfinal/ sqrt(inner)))
      # ------------------------------------------------------------------------------------------------
      
      # ------------------------------------------------------------------------------------------------
      return(list(c(ifinal,jfinal,kfinal),
                  a,
                  b,
                  dir_of_travel,
                  inclination_of_plane))
      # ------------------------------------------------------------------------------------------------
    }
    # ------------------------------------------------------------------------------------------------
    
    
    
    
    
    ################################## ERRORS ########################################################
    # ------------------------------------------------------------------------------------------------
    # dimensions and values of raster
    if(is.matrix(values) == FALSE){
      if(all(is.na(values[1,1:ncol(values)])) == FALSE |
         all(is.na(values[1:nrow(values),1])) == FALSE |
         all(is.na(values[1:ncol(values),nrow(values)])) == FALSE |
         all(is.na(values[1:nrow(values),ncol(values)])) == FALSE){
        stop(paste0('flow_dir_of_DEM:\n\n',
                    'Matrix must have border of NA values\n'))
      }
      stop(paste0('flow_dir_of_DEM:\n\n',
                  'values must be in form of matrix\n'))
    } else {
      ncol <- ncol(values)
      nrow <- nrow(values)
    }
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # calculate position of cell
    position <- (ncol * (row-1)) +
      column
    
    if(is.null(diff_x) == TRUE |
       is.null(diff_y) == TRUE){
      diff_x <- res(raster)[1]
      diff_y <- res(raster)[2]
    }
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # adjust position accoutning for NA pad
    row <- row + 1
    column <- column + 1
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # matrix of neighbor values
    current_cell_value <- values[row,column]
    neighbors <- c(values[row-1,column-1], # NW
                   values[row-1,column], # N
                   values[row-1,column+1], # NE
                   values[row,column-1], # W
                   values[row,column], # C
                   values[row,column+1], # E
                   values[row+1,column-1], # SW
                   values[row+1,column], # S
                   values[row+1,column+1]) # SE
    neighbors <- matrix(nrow = 3,
                        ncol = 3,
                        byrow = TRUE,
                        data = neighbors)
    # ------------------------------------------------------------------------------------------------
    
    
    # ------------------------------------------------------------------------------------------------
    # if an eight direction method wont work
    # will only 4 cardinal directions?
    if(any(is.na(neighbors) == TRUE)){
      
      # ------------------------------------------------------------------------------------------------
      # get cardinal values
      ctr_x <- 2
      ctr_y <- 2
      east_value <- neighbors[ctr_x + 0,
                              ctr_y + 1]
      south_value <- neighbors[ctr_x + 1,
                               ctr_y + 0]
      west_value <- neighbors[ctr_x + 0,
                              ctr_y + -1]
      north_value <- neighbors[ctr_x + -1,
                               ctr_y + 0]
      # ------------------------------------------------------------------------------------------------
      
      
      # ------------------------------------------------------------------------------------------------
      # are any of the four neighbors NA
      if(any(is.na(c(east_value,
                     south_value,
                     west_value,
                     north_value)) == TRUE)){
        
        final_dir <- NA
        final_dir_deg <- NA
        max_slope <- NA
        
      } else {
        
        dzdx <- (east_value - west_value)/ (2*diff_x)
        dzdy <- (north_value - south_value)/(2*diff_y)
        final_dir <- atan2(-dzdy,-dzdx)
        final_dir_deg <- final_dir * (180/3.14159)
        final_dir_deg <- (final_dir_deg + 360) %% 360
        max_slope <- -(dzdx*cos(final_dir) + dzdy*sin(final_dir))
        max_slope <- atan2(max_slope,1) * (180/3.14195)
        
      }
      # ------------------------------------------------------------------------------------------------
    }
    # ------------------------------------------------------------------------------------------------
    
    
    # ------------------------------------------------------------------------------------------------
    # data is fully present
    dir_wedges <- c()
    slopes_wedges <- c()
    if(all(is.na(neighbors) == FALSE)){
      # ------------------------------------------------------------------------------------------------
      # neighbors from east, clockwise repeating east
      nbr_ids <- 1:9
      diff_dx <- c( 1,  1,  0, -1, -1, -1,  0,  1)
      diff_dy <- c( 0, -1, -1, -1,  0,  1,  1,  1)
      
      diff_dx_shifted <- c(1,  0, -1, -1, -1,  0,  1,  1)
      diff_dy_shifted <-c(-1, -1, -1,  0,  1,  1,  1,  0)
      
      nbr_dx  <- c( 1,  1,  0, -1, -1,  -1,  0,  1)
      nbr_dy  <- c( 0,  1,  1,  1,  0,  -1, -1, -1)
      
      nbr_dx_shifted  <- c(1,  0, -1, -1,  -1,  0,  1,  1)
      nbr_dy_shifted  <- c(1,  1,  1,  0,  -1, -1, -1,  0)
      
      nbr_angles <- c(  0, 45, 90, 135, 180, 225, 270, 315, 0)
      ctr_x <- 2
      ctr_y <- 2
      # ------------------------------------------------------------------------------------------------
      
      
      # ------------------------------------------------------------------------------------------------
      # ijk coordinates
      p0z <- neighbors[ctr_x,
                       ctr_y]
      p1z <- neighbors[cbind(ctr_y + nbr_dy,
                             ctr_x + nbr_dx)]
      p2z <- neighbors[cbind(ctr_y + nbr_dx_shifted,
                             ctr_x + nbr_dy_shifted)]
      
      p0x <- 0
      p1x <- diff_x*diff_dx
      p2x <- diff_x*diff_dx_shifted
      
      p0y <- 0
      p1y <- diff_y*diff_dy
      p2y <- diff_y*diff_dy_shifted
      # ------------------------------------------------------------------------------------------------
      
      # ------------------------------------------------------------------------------------------------
      # edge vectors of the plane
      V <- c(p1x - p0x,
             p1y - p0y,
             p1z - p0z)
      U <- c(p2x - p0x,
             p2y - p0y,
             p2z - p0z)
      # ------------------------------------------------------------------------------------------------
      
      # ------------------------------------------------------------------------------------------------
      # get bounds of wedges
      bounds <- cbind(atan2(diff_y*diff_dy,
                            diff_x*diff_dx),
                      atan2(diff_y*diff_dy_shifted,
                            diff_x*diff_dx_shifted))
      # ------------------------------------------------------------------------------------------------
      
      # ------------------------------------------------------------------------------------------------
      # bind ijk
      V <- cbind(V[1:8],
                 V[9:16],
                 V[17:24])
      U <- cbind(U[1:8],
                 U[9:16],
                 U[17:24])
      # ------------------------------------------------------------------------------------------------
      
      # ------------------------------------------------------------------------------------------------
      # output cross product
      output <- Cross_Product_Matrix(V,U) 
      # ------------------------------------------------------------------------------------------------
      
      # ------------------------------------------------------------------------------------------------
      # is there a valid angle
      if(any(is.na(output[[4]])) == FALSE){
        # ------------------------------------------------------------------------------------------------
        # convert from signed to unsigned (0-2*pi) angle
        bounds_comp <- cbind(bounds,output[[4]])
        bounds_comp <- (bounds_comp + (2*pi)) %% (2*pi)
        # ------------------------------------------------------------------------------------------------
        
        # ------------------------------------------------------------------------------------------------
        # do any directions exceed the wedge bounds
        inds <- which(bounds_comp[,3] < apply(bounds[,1:2],1,min)|
                        bounds_comp[,3] > apply(bounds[,1:2],1,max))
        if(length(inds) > 0){
          bounds_comp_min_inds <- apply(bounds_comp[inds,3] - bounds_comp[inds,1:2],1,which.min)
          bounds_comp[inds,3] <- bounds_comp[cbind(inds,bounds_comp_min_inds)]
        }
        # ------------------------------------------------------------------------------------------------
        
        # ------------------------------------------------------------------------------------------------
        # convert back to signed angle
        final_out <- ifelse(bounds_comp[,3] > pi,
                            bounds_comp[,3] - 2*pi,
                            bounds_comp[,3])
        # ------------------------------------------------------------------------------------------------
        
        # ------------------------------------------------------------------------------------------------
        # slopes of vectors
        a <- output[[2]]
        b <- output[[3]]
        
        slopes_wedges <- -((a*cos(final_out)) + (b*sin(final_out)))
        slopes_wedges <- atan2(slopes_wedges,1) * (180/3.14159)
        dir_wedges <- final_out
        # ------------------------------------------------------------------------------------------------
        
      } else {
        slopes_wedges <- NA
        dir_wedges <- NA
      }
      # ------------------------------------------------------------------------------------------------
      
      # ------------------------------------------------------------------------------------------------
      slopes_wedges <- slopes_wedges[is.na(slopes_wedges) == FALSE]
      dir_wedges <- dir_wedges[is.na(dir_wedges) == FALSE]
      # ------------------------------------------------------------------------------------------------
      
      # ------------------------------------------------------------------------------------------------
      # if there are no slopes then all surrounding cells must be NA
      # if there are slopes which is the maximum increase
      if(length(slopes_wedges) > 0){
        # ------------------------------------------------------------------------------------------------
        # is cell a sink ('s') or flat ('f')
        if(all(slopes_wedges <= 0)){
          final_dir <- 's'
          final_dir_deg <- 's'
          max_slope <- 0
        } else if (all(slopes_wedges < min_slope)){
          final_dir <- 'f'
          final_dir_deg <- 'f'
          max_slope <- 0
        } else {
          
          ind <- which(slopes_wedges == max(slopes_wedges))
          
          if(length(ind) > 1){
            final_dir <- mean(dir_wedges[ind], na.rm = T)
            final_dir_deg <- final_dir * (180/3.14159)
            final_dir_deg <- (final_dir_deg + 360) %% 360
            max_slope <- mean(slopes_wedges[ind], na.rm = T)
          } else {
            final_dir <- dir_wedges[ind]
            final_dir_deg <- final_dir * (180/3.14159)
            final_dir_deg <- (final_dir_deg + 360) %% 360
            max_slope <- slopes_wedges[ind]
          }
          
        }
        # ------------------------------------------------------------------------------------------------
      } else{
        final_dir <- NA
        final_dir_deg <- NA
        max_slope <- NA
      }
      # ------------------------------------------------------------------------------------------------
    }
    # ------------------------------------------------------------------------------------------------
    
    return(list(final_dir,
                final_dir_deg,
                max_slope))
  }
  # ------------------------------------------------------------------------------------------------
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ############################################################################################################
  ################################## RUN FUNCTIONS ###########################################################
  ############################################################################################################
  
  
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
  # ensure correct packages are loaded
  required_packages <- c('sf','sp','raster','terra')
  for(i in 1:length(required_packages)){
    require_package(required_packages[i])
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