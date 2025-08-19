library(raster)
library(terra)
library(sf)
# rast1 <- rast('C:/Users/ChristopherDory/LWA Dropbox/Christopher Dory/Projects/598/598.07/5c Shasta_ISW/Data/DEM/USGS_13_n42w123_20231102.tif')
# ext <- st_read('C:/Users/ChristopherDory/LWA Dropbox/Christopher Dory/Projects/598/598.07/5c Shasta_ISW/Shapefiles/Accessory_Shapefiles/Scott_HUC_12.shp')
# plot(rast1, add = T)
# rast1 <- crop(rast1, ext(ext[ext$name == "South Fork Scott River", ]))
# rast1 <- mask(rast1, ext(ext[ext$name == "South Fork Scott River", ]))
# plot(st_geometry(ext))
# ext$name
# 
# 
# 
# # ------------------------------------------------------------------------------------------------
# # example
# inds <- c(1:40)
# ncol <- ncol(rast1)
# ncol <- rep(ncol,40)
# mod <- c(1:40)
# ncol <- mod*ncol
# inds <- c()
# for(i in 1:40){
#   for(j in 1:40){
#     inds <- append(inds,
#                    ncol[i]+j)
#   }
# }
# 
# 
# 
# rast2 <- rast1[inds]
# test_rast <- raster(ncol = 40,
#                     nrow = 40,
#                     xmn = -120,
#                     xmx = -119,
#                     ymn = 40,
#                     ymx = 41)
# values(test_rast) <- as.vector(unlist(rast2))
# 
# plot(test_rast)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# s1 <- Sys.time()
# 
# theta_rad <- list()
# values <- values(rast1)
# values <- matrix(values,
#                  nrow = nrow,
#                  ncol = ncol,
#                  byrow = TRUE)
# values <- rbind(values,
#                 rep(NA,ncol))
# values <- rbind(rep(NA,ncol),
#                 values)
# values <- cbind(values,
#                 rep(NA,nrow+2))
# values <- cbind(rep(NA,nrow+2),
#                 values)
# nrow <- nrow(rast1)
# ncol <- ncol(rast1)
# diff_x <- res(rast1)[1]
# diff_y <- res(rast1)[2]
# counter <- 0
# for(i in 1:nrow(rast1)){
#   print(i/nrow(rast1))
#   for(j in 1:ncol(rast1)){
#     counter <- counter +1
#     theta_rad[[counter]] <- flow_dir_of_DEM(raster = rast1,
#                               values = values,
#                               row = i,
#                               column = j,
#                               diff_x = diff_x,
#                               diff_y = diff_y)[[2]]
# 
# 
#   }
# }
# print(Sys.time()- s1)
# 
# theta_rad <- as.numeric(theta_rad)
# theta_rast <- rast(ncol = ncol(rast1),
#                    nrow = nrow(rast1),
#                    crs = crs(rast1),
#                    xmin = xmin(rast1),
#                    xmax = xmax(rast1),
#                    ymin = ymin(rast1),
#                    ymax = ymax(rast1))
# values(theta_rast) <- theta_rad
# # writeRaster(theta_rast,
# #             'C:/Users/ChristopherDory/LWA Dropbox/Christopher Dory/Projects/598/598.07/5c Shasta_ISW/Shapefiles/Accessory_Shapefiles/South_Fork_Flow_Dir.tif')
# 
# print((Sys.time() - s1)/ncell(rast1))
# plot(rast1)
# plot(theta_rast)
# 
# 
# 
# 
# values[2,2]
# 
# 
# (7*60) + 30
# 450/1000
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# plot(rast(test_rast))
# x <- terra::xyFromCell(test_rast,1:ncell(test_rast))[,1]
# y <- terra::xyFromCell(test_rast,1:ncell(test_rast))[,2]
# 
# 
# 
# for(i in 1:ncell(test_rast)){
#   ymod <- sin(theta_rad[i])*0.025
#   xmod <- cos(theta_rad[i])*0.025
#   lines(c(x[i], x[i] + xmod),
#         c(y[i], y[i] + ymod))
#   points(c(x[i] + xmod),
#          c(y[i] + ymod),
#          cex = 0.6,
#          col = 'red',
#          pch = 19)
# }
# # ------------------------------------------------------------------------------------------------





flow_dir_of_DEM <- function(raster = test_rast,
                            values = NULL,
                            row = 2,
                            column = 2,
                            diff_x = NULL,
                            diff_y = NULL,
                            min_slope = 1e-9)
{
  
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
      max_slope <- -(dzdx*cos(dir) + dzdy*sin(dir))


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
    nbr_ids    <- 1:9
    diff_dx     <- c( 1,  1,  0, -1, -1, -1,  0,  1,  1)
    diff_dy     <- c( 0, -1, -1, -1,  0,  1,  1,  1,  0)
    nbr_dx     <- c( 1,  1,  0, -1, -1,  -1,  0,  1,  1)
    nbr_dy     <- c( 0,  1,  1,  1,  0,  -1, -1, -1,  0)
    nbr_angles <- c(  0, 45, 90, 135, 180, 225, 270, 315, 0)
    ctr_x <- 2
    ctr_y <- 2
    # ------------------------------------------------------------------------------------------------

    # ------------------------------------------------------------------------------------------------
    # get direction and gradient of each facet
    for(i in 1:8){

      # ------------------------------------------------------------------------------------------------
      # ijk coordinates
      p0z <- neighbors[ctr_x,
                       ctr_y]
      p1z <- neighbors[ctr_y + nbr_dy[i],
                       ctr_x + nbr_dx[i]]
      p2z <- neighbors[ctr_y + nbr_dy[i+1],
                       ctr_x + nbr_dx[i+1]]

      p0x <- 0
      p1x <- diff_x*diff_dx[i]
      p2x <- diff_x*diff_dx[i+1]

      p0y <- 0
      p1y <- diff_y*diff_dy[i]
      p2y <- diff_y*diff_dy[i+1]
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
      # get bounds of wedge
      bounds <- c(atan2(diff_y*diff_dy[i],
                        diff_x*diff_dx[i]),
                  atan2(diff_y*diff_dy[i+1],
                        diff_x*diff_dx[i+1]))

      output <- Cross_Product(V,U)
      dir <- output[[4]]
      # ------------------------------------------------------------------------------------------------

      # ------------------------------------------------------------------------------------------------
      if(is.na(dir) == FALSE){
        # ------------------------------------------------------------------------------------------------
        # clamp to within triangular plane if direction of maximum inclination
        # resides outside triangular wedge
        if(dir < min(bounds) |
           dir > max(bounds)){
          ind <- which((bounds-dir) == min(bounds-dir))
          dir <- bounds[ind]
        } else {}#pass
        # ------------------------------------------------------------------------------------------------

        # ------------------------------------------------------------------------------------------------
        # append information
        a <- output[[2]]
        b <- output[[3]]
        slope_wedge <- -((a*cos(dir)) + (b*sin(dir)))
        slopes_wedges <- append(slopes_wedges,
                                slope_wedge)
        dir_wedges <- append(dir_wedges,
                             dir)
        s2 <- append(s2,
                     output[[5]])
        # ------------------------------------------------------------------------------------------------
      } else {
        dir_wedges <- append(dir_wedges,
                             NA)
        slopes_wedges <- append(slopes_wedges,
                                NA)
      }
      # ------------------------------------------------------------------------------------------------
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
        max_slope <- 0
      } else if (all(slopes_wedges < min_slope)){
        final_dir <- 'f'
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
    }
    # ------------------------------------------------------------------------------------------------
  }
  # ------------------------------------------------------------------------------------------------

  return(list(final_dir,
              final_dir_deg,
              max_slope))
}
# ------------------------------------------------------------------------------------------------






# ------------------------------------------------------------------------------------------------
# calculates the cross product of two vectors
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