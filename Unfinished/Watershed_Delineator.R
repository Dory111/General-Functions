# ------------------------------------------------------------------------------------------------
# testing values
rast <- raster(ncol = 10, nrow = 10,
               xmn = 1000, xmx = 2000,
               ymn = 1000, ymx = 2000,
               crs = 3310)
start_value <- 100
subtract <- 20
start_row <- rep(start_value, ncol(rast))
half_rows <- list(start_row)
for(i in 2:(nrow(rast)/2)){
  row <- half_rows[[i-1]]
  inds <- c(i,ncol(rast)-i+1)
  row[inds[1]:inds[2]] <- row[inds[1]:inds[2]] - subtract
  half_rows[[i]] <- row
}
half_rows2 <- do.call(rbind, half_rows[5:1])
half_rows <- do.call(rbind, half_rows)


all_rows <- rbind(half_rows,
                  half_rows2)
values(rast) <- all_rows



raster <- rast
out_dir <- 'C:/Users/ChristopherDory/LWA Dropbox/Christopher Dory/Projects/598/598.06/00 ISW/Output/Raster'
flow_dir_rast_name <- 'Flow_Dir_Test'
flow_to_outlet_rast_name <- 'Outlet_Test'
min_slope <- 1
diff_x <- NULL
diff_y <- NULL
zunit <- 'm'
suppress_loading_bar <- FALSE
suppress_console_messages <- FALSE
spinning_bar_update_cycle <- 1
sink_code <- -4444
flat_code <- -9999
outlet_location_CRS <- NULL
outlet_location_is_sf <- TRUE
outlet_location <- st_sf(st_sfc(st_point(x = c(1350,1350)),
                          crs = 3310))
st_geometry(outlet_location) <- 'geometry'
# ------------------------------------------------------------------------------------------------


# ==================================================================================================
# Delineates watersheds by first determining the flow direction of a DEM, and then recursively finding
# all the cells that flow towards a chosen outlet location
# ==================================================================================================
Watershed_Delineator <- function(raster,
                                 out_dir,
                                 outlet_location = NULL,
                                 outlet_location_CRS = NULL,
                                 outlet_location_is_sf = TRUE,
                                 outlet_location_is_line = FALSE,
                                 outlet_location_line_density = 100,
                                 flow_to_outlet_rast_name = NULL,
                                 flow_dir_rast_name = NULL,
                                 min_slope = 1,
                                 flat_code = -9999,
                                 sink_code = -4444,
                                 diff_x = NULL,
                                 diff_y = NULL,
                                 zunit = 'm',
                                 suppress_loading_bar = FALSE,
                                 suppress_console_messages = FALSE,
                                 spinning_bar_update_cycle = 1)
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
  # Simple function to concatenate a loading bar
  spinning_bar <- function(optional_text, character) 
  {
    # ------------------------------------------------------------------------------------------------
    cat(sprintf("\r[ %s ] %s",
                character,
                optional_text))
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
  
  
  
  
  
  #===========================================================================================
  # if outlet location is passed as an XY point then move it to an sf object
  #===========================================================================================
  coerce_outlet_location <- function(outlet_location)
  {
    out_list <- list()
    
    # ------------------------------------------------------------------------------------------------
    # if user modifies to indicate that outlet location is not as an sf dataframe but an XY point
    if(outlet_location_is_sf == FALSE){
      if(is.null(outlet_location_CRS) == TRUE){
        for(i in 1:nrow(outlet_location)){
          out_list[[i]] <- st_sf(st_sfc(st_point(x = c(outlet_location[i,1],outlet_location[i,2])),
                                 crs = st_crs(raster)))
        }
      } else {
        for(i in 1:nrow(outlet_location)){
          out <- st_sf(st_sfc(st_point(x = c(outlet_location[i,1],outlet_location[i,2])),
                       crs = outlet_location_CRS))
          out <- st_transform(out,
                              st_crs(raster))
          out_list[[i]] <- out
        }
      }
    } 
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # if user indicates that default case is true (location is an sf dataframe) then check if that
    # is actually the case
    # If it isnt the case and default left on by accident then coerce to sf if possible
    if(outlet_location_is_sf == TRUE){
      # ------------------------------------------------------------------------------------------------
      # move vector to sf dataframe
      if(class(outlet_location) != 'sf'){
        if(is.null(outlet_location_CRS) == TRUE){
          for(i in 1:nrow(outlet_location)){
            out_list[[i]] <- st_sf(st_sfc(st_point(x = c(outlet_location[i,1],outlet_location[i,2])),
                                          crs = st_crs(raster)))
          }
        } else {
          for(i in 1:nrow(outlet_location)){
            out <- st_sf(st_sfc(st_point(x = c(outlet_location[i,1],outlet_location[i,2])),
                         crs = outlet_location_CRS))
            out <- st_transform(outlet_location,
                                st_crs(raster))
            out_list[[i]] <- out
          }
        }
      } else {}
      # ------------------------------------------------------------------------------------------------
    }
    # ------------------------------------------------------------------------------------------------
    names(out_list) <- NULL
    out_list <- do.call(rbind, out_list)
    st_geometry(out_list) <- 'geometry'
    outlet_location <- out_list
    return(outlet_location)
  }
  # ------------------------------------------------------------------------------------------------
  
  
  
  # ==================================================================================================
  # Uses passed bounds and the flow angles of cell neighbors to determine what cells flow
  # to current cell
  # ==================================================================================================
  check_outlet_neighbors <- function(bounds,
                                     outlet_neighbors)
  {
    # ------------------------------------------------------------------------------------------------
    # checking whether outlet flows to current cell
    TF_points <- c()
    checked <- rep(TRUE, length(outlet_neighbors))
    for(i in 1:length(outlet_neighbors)){
      # ------------------------------------------------------------------------------------------------
      # east neighbor has to be treated differently because the angles wrap passed 360
      if(i == 4){
        # ------------------------------------------------------------------------------------------------
        # if its an edge piece it cant point anywhere so disregard it
        if(is.na(outlet_neighbors[i]) == TRUE){
          TF_points <- append(TF_points, FALSE)
        } else {
          # ------------------------------------------------------------------------------------------------
          # if the neighbor points within the bound angles then it flows to the outlet cell
          # otherwise FALSE
          if(outlet_neighbors[i] >= 0 &
             outlet_neighbors[i] <= min(bounds[i, ])){
            TF_points <- append(TF_points, TRUE)
          } else if (outlet_neighbors[i] >= max(bounds[i, ]) &
                     outlet_neighbors[i] <= 360){
            TF_points <- append(TF_points, TRUE)
          } else {
            TF_points <- append(TF_points, FALSE)
          }
          # ------------------------------------------------------------------------------------------------
        }
        # ------------------------------------------------------------------------------------------------
        
        
      } else {
        
        # ------------------------------------------------------------------------------------------------
        # if its an edge piece it cant point anywhere so disregard it
        if(is.na(outlet_neighbors[i]) == TRUE){
          TF_points <- append(TF_points, FALSE)
        } else {
          # ------------------------------------------------------------------------------------------------
          # if the neighbor points within the bound angles then it flows to the outlet cell
          # otherwise FALSE
          if(outlet_neighbors[i] >= min(bounds[i, ]) &
             outlet_neighbors[i] <= max(bounds[i, ])){
            TF_points <- append(TF_points, TRUE)
          } else {
            TF_points <- append(TF_points, FALSE)
          }
          # ------------------------------------------------------------------------------------------------
        }
        # ------------------------------------------------------------------------------------------------
      }
      # ------------------------------------------------------------------------------------------------
    }
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    return(list(TF_points,
                checked))
    # ------------------------------------------------------------------------------------------------
  }
  # ------------------------------------------------------------------------------------------------
  
  
  
  # ==================================================================================================
  # Interpolates between points on a line
  # ==================================================================================================
  custom_st_line_sample <- function(sfobj, n)
  {
    # ------------------------------------------------------------------------------------------------
    # getting coordinates
    coords <- st_coordinates(sfobj)
    new_line_points <- list()
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    for(i in 1:(nrow(coords)-1)){
      # ------------------------------------------------------------------------------------------------
      # interpolating between points
      interp_x <- seq(from = coords[i,1],
                      to = coords[i+1,1], length.out = outlet_location_line_density)
      interp_y <- seq(from = coords[i,2],
                      to = coords[i+1,2], length.out = outlet_location_line_density)
      # ------------------------------------------------------------------------------------------------
      
      # ------------------------------------------------------------------------------------------------
      bound <- cbind(interp_x,
                     interp_y)
      bound <- unique(bound)
      
      intermediate <- list()
      for(j in 1:nrow(bound)){
        if(is.na(outlet_location_CRS) == FALSE){
          intermediate[[j]] <- st_sf(st_sfc(st_point(bound[j, ])),
                                     crs = outlet_location_CRS)
        } else {
          intermediate[[j]] <- st_sf(st_sfc(st_point(bound[j, ])),
                                     crs = st_crs(raster))
        }
      }
      names(intermediate) <- NULL
      intermediate <- do.call(rbind, intermediate)
      st_geometry(intermediate) <- 'geometry'
      # ------------------------------------------------------------------------------------------------
      
      new_line_points[[i]] <- intermediate
    }
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    names(new_line_points) <- NULL
    new_line_points <- do.call(rbind, new_line_points)
    return(new_line_points)
    # ------------------------------------------------------------------------------------------------
  }
  # ------------------------------------------------------------------------------------------------
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ############################################################################################################
  ################################## MAIN FUNCTIONS ##########################################################
  ############################################################################################################
  
  
  # ==================================================================================================
  # Generates the likely direction of flow from a DEM (0-360 degrees, 90 being north)
  # based on D (inf) method of Tarboton 1997 (https://doi.org/10.1029/96WR03137)
  # ==================================================================================================
  flow_dir_of_DEM <- function(raster = test_rast){
    # ==================================================================================================
    # Takes current cell and uses 8 neighbors to find the flow across that cell
    # ==================================================================================================
    flow_dir_of_neighbors <- function(raster = test_rast,
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
      } else {}
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
        p2z <- neighbors[cbind(ctr_y + nbr_dy_shifted,
                               ctr_x + nbr_dx_shifted)]
        
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
        # bounds <- matrix(nrow = 8, ncol = 2,
        #                  data = c(0,       -pi/4,
        #                           -pi/4,   -pi/2,
        #                           -pi/2,   -3*pi/4,
        #                           -3*pi/4, -pi,
        #                           pi,      3*pi/4,
        #                           3*pi/4,  pi/2,
        #                           pi/2,    pi/4,
        #                           pi/4,    0),
        #                  byrow= T)
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
          bounds_comp <- cbind(bounds, output[[4]])
          bounds_comp <- (bounds_comp + (2*pi)) %% (2*pi)
          bounds_comp <- bounds_comp * (180/pi)
          bounds_comp[1,1] <- 360
          # ------------------------------------------------------------------------------------------------
          
          # ------------------------------------------------------------------------------------------------
          # do any directions exceed the wedge bounds
          inds <- which(bounds_comp[,3] < apply(bounds_comp[,1:2],1,min)|
                        bounds_comp[,3] > apply(bounds_comp[,1:2],1,max))
          if(length(inds) > 0){
            bounds_comp_min_inds <- apply(bounds_comp[inds,3] - bounds_comp[inds,1:2],1,which.min)
            bounds_comp[inds,3] <- bounds_comp[cbind(inds,bounds_comp_min_inds)]
          }
          # ------------------------------------------------------------------------------------------------
          
          # ------------------------------------------------------------------------------------------------
          # convert to unsigned angle
          bounds_comp <- bounds_comp * (pi/180)
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
          # if all less than 0 (all pointing to cell) its a sink
          # if not zero but all less than min slope its a flat
          # if neither of these criteria is fulfilled
          if(all(slopes_wedges < 0)){
            final_dir <- sink_code
            final_dir_deg <- sink_code
            max_slope <- 0
          } else if (all(slopes_wedges < min_slope)){
            final_dir <- flat_code
            final_dir_deg <- flat_code
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
    
    
    
    
    
    
    
    
    
    
    
    
    ######################################### FIND FLOW DIR ACROSS ALL NEIGHBORS ####################
    
    # ------------------------------------------------------------------------------------------------
    # getting flow directions
    flow_dir_deg_output <- list()
    flow_dir_rad_output <- list()
    flow_dir_slope_output <- list()
    counter <- 0
    for(i in 1:nrow(raster)){
      # ------------------------------------------------------------------------------------------------
      # update loading bar
      if(suppress_loading_bar == FALSE){
        loading_bar(i,
                    nrow(raster),
                    width = 50)
      }
      # ------------------------------------------------------------------------------------------------
      
      # ------------------------------------------------------------------------------------------------
      # run function
      for(j in 1:ncol(raster)){
        counter <- counter + 1
        
        output <- flow_dir_of_neighbors(raster = raster,
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
      # ------------------------------------------------------------------------------------------------
    }
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    return(list(flow_dir_deg_output,
                flow_dir_rad_output,
                flow_dir_slope_output))
    # ------------------------------------------------------------------------------------------------
  }
  # ------------------------------------------------------------------------------------------------
  
  
  
  
  
  
  
  
  # ==================================================================================================
  # Recursively checks all cells that flow to the outlet
  # ==================================================================================================
  check_all_cells_flowing_to_outlet <- function(raster,
                                                diff_x,
                                                diff_y,
                                                flow_dir_deg_output,
                                                outlet_cell)
  {
    # ------------------------------------------------------------------------------------------------
    # which cells have been checked, and which flow to the outlet cell
    all_check <- matrix(FALSE,
                        ncol = ncol(raster),
                        nrow = nrow(raster))
    cells_flowing_to_outlet <- matrix(FALSE,
                                      ncol = ncol(raster),
                                      nrow = nrow(raster))
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # get degree values
    flow_dir_deg_mat <- matrix(flow_dir_deg_output,
                               ncol = ncol(raster),
                               nrow = nrow(raster),
                               byrow = TRUE)
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # which column and row is the outlet in
    # if its a multiple of the columns it must be the last column and then the row stays the same
    # else a value of something like 64 in a 10 by 10 matrix will actually be located in the 7th row at position 4
    # so add one to the outlet row
    outlet_column <- outlet_cell %% ncol(raster)
    if(outlet_column == 0){
      outlet_column <- ncol(raster)
      outlet_row <- floor(outlet_cell/ncol(raster))
    } else {
      outlet_row <- floor(outlet_cell/ncol(raster))
      outlet_row <- outlet_row + 1
    }
    all_check[outlet_row, outlet_column] <- TRUE
    cells_flowing_to_outlet[outlet_row, outlet_column] <- TRUE
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # positions of cell neighbors to the north south east and west
    outlet_neighbors_dx <- c(0,0,1,-1)
    outlet_neighbors_dy <- c(-1,1,0,0)
    outlet_neighbors <- flow_dir_deg_mat[cbind(outlet_row + outlet_neighbors_dy,
                                               outlet_column + outlet_neighbors_dx)]
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # angles that neighbors can point to
    if(is.null(diff_x) == TRUE |
       is.null(diff_y) == TRUE){
      diff_x <- res(raster)[1]
      diff_y <- res(raster)[2]
    }
    
    diff_dx <- c( -1, -1,  -1, 1)
    diff_dy <- c( -1,  1,   1, 1)
    
    diff_dx_shifted <- c( 1,  1, -1,  1) 
    diff_dy_shifted <- c(-1,  1, -1, -1)
    
    bounds <- cbind(atan2(diff_y*diff_dy,
                          diff_x*diff_dx),
                    atan2(diff_y*diff_dy_shifted,
                          diff_x*diff_dx_shifted))
    bounds <- (bounds + (2*pi)) %% (2*pi)
    bounds <- bounds * (180/pi)
    # ------------------------------------------------------------------------------------------------
    
    
    # ------------------------------------------------------------------------------------------------
    finished <- FALSE
    counter <- 0
    ncheck <- 0
    nstep <- 0
    niter <- 0
    go_back_and_check <- matrix(c(1,1), ncol = 2)
    characters <- c('|', '/', '-','\\')
    while(finished == FALSE){
      niter <- niter + 1

      # ------------------------------------------------------------------------------------------------
      # check what cells flow to current cell
      output <- check_outlet_neighbors(bounds,
                                       outlet_neighbors)
      # ------------------------------------------------------------------------------------------------
      
      # ------------------------------------------------------------------------------------------------
      # update matrices
      row_columns <- cbind(outlet_row + outlet_neighbors_dy,
                           outlet_column + outlet_neighbors_dx)
      update_needed <- which(cells_flowing_to_outlet[row_columns] == FALSE)
      ncheck <- ncheck + length(which(all_check[row_columns] == FALSE))
      if(length(update_needed) != 0){
        cells_flowing_to_outlet[matrix(row_columns[update_needed, ], ncol = 2)] <- output[[1]][update_needed]
        all_check[matrix(row_columns[update_needed, ], ncol = 2)] <- output[[2]][update_needed]
      } else {}
      # ------------------------------------------------------------------------------------------------
      
      # ------------------------------------------------------------------------------------------------
      # find which rows and columns to check next
      next_row_column <- matrix(row_columns[which(output[[1]] == TRUE), ],
                                ncol = 2)
      # ------------------------------------------------------------------------------------------------
      
      # ------------------------------------------------------------------------------------------------
      # update loading bar
      if(suppress_loading_bar == FALSE){
        if(niter %% spinning_bar_update_cycle == 0){
          nstep <- nstep + 1
          pos <- nstep%%length(characters)
          if(pos == 0){
            pos <- 1
          }
          spinning_bar(optional_text = paste0('Ncell Checked: ', ncheck),
                       character = characters[pos])
        } else {
          if(niter == 1){
            pos <- 1
          } else {}
          spinning_bar(optional_text = paste0('Ncell Checked: ', ncheck),
                       character = characters[pos])
        }
      }
      # ------------------------------------------------------------------------------------------------
      
      # ------------------------------------------------------------------------------------------------
      # if the current cell has neighbors still to check then check them
      # if more than one cell to check choose a random one first and set other aside
      # if current cell has none to check look back to the ones that have been set aside
      # and choose a random one from that set
      # if no neighbors need to be checked on current cell, and none are left set aside set break loop
      if(nrow(next_row_column) > 1){
        # ------------------------------------------------------------------------------------------------
        # get the next row and column
        to_check_currently <- round(runif(n = 1, min = 1, max = nrow(next_row_column)))
        set_aside <- c(1:nrow(next_row_column))[-c(to_check_currently)]
        # ------------------------------------------------------------------------------------------------
        
        # ------------------------------------------------------------------------------------------------
        # if there are items to set aside put it in the matrix
        if(length(set_aside) > 0){
          # ------------------------------------------------------------------------------------------------
          # if its the first time remove dummy values, if not just bind it
          if(counter == 0){
            counter <- counter + 1
            go_back_and_check <- rbind(go_back_and_check,
                                       next_row_column[set_aside, ])
            go_back_and_check <- matrix(go_back_and_check[-c(1), ],
                                        ncol = 2)
          } else {
            go_back_and_check <- rbind(go_back_and_check,
                                       next_row_column[set_aside, ])
          }
          # ------------------------------------------------------------------------------------------------
        } else{}
        next_row_column <- next_row_column[to_check_currently, ]
        # ------------------------------------------------------------------------------------------------
      } else {
        
        # ------------------------------------------------------------------------------------------------
        # if theres nothing left set aside, then the loop must be over
        if(nrow(go_back_and_check) != 0){
          # ------------------------------------------------------------------------------------------------
          # get the next row and column
          to_check_currently <- round(runif(n = 1, min = 1, max = nrow(go_back_and_check)))
          set_aside <- c(1:nrow(go_back_and_check))[-c(to_check_currently)]
          
          
          next_row_column <- go_back_and_check[to_check_currently, ]
          go_back_and_check <- matrix(go_back_and_check[set_aside, ],
                                      ncol = 2)
          # ------------------------------------------------------------------------------------------------
        } else {
          finished <- TRUE
          next_row_column <- c(2,2)
        }
        # ------------------------------------------------------------------------------------------------
      }
      # ------------------------------------------------------------------------------------------------
      
      # ------------------------------------------------------------------------------------------------
      # increment loop to the next step
      outlet_row <- next_row_column[1]
      outlet_column <- next_row_column[2]
      outlet_neighbors <- flow_dir_deg_mat[cbind(outlet_row + outlet_neighbors_dy,
                                                 outlet_column + outlet_neighbors_dx)]
      # ------------------------------------------------------------------------------------------------
    }
    cat('\n')
    # ------------------------------------------------------------------------------------------------
    return(list(all_check,
                cells_flowing_to_outlet))
  }
  # ------------------------------------------------------------------------------------------------
  
  
  
  
  
  
  
  
  
  
  
  
  
 
  
  
  
  ############################################################################################################
  ################################## ERRORS ##################################################################
  ############################################################################################################
  
  # ------------------------------------------------------------------------------------------------
  # not a raster error
  if(class(raster)[1] != 'SpatRaster'){
    if(class(raster)[1] != 'RasterLayer'){
      stop(paste0('\nWatershed_Delineator:\n\n',
                  'Inputs not in the form of a raster\n'))
    } else {
      raster <- rast(raster)
    }
  }
  # ------------------------------------------------------------------------------------------------
  
  # ------------------------------------------------------------------------------------------------
  # raster too small error
  if(ncell(raster) < 16){
    stop(paste0('\nWatershed_Delineator:\n\n',
                'Raster too small (<16 cells) to be meaningful\n')) 
  }
  # ------------------------------------------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------
  # forgot to pass outlet location error
  if(is.null(outlet_location) == TRUE){
    stop(paste0('\nWatershed_Delineator:\n\n',
                'Outlet location passed is NULL\n'))
  } else {
    outlet_location <- coerce_outlet_location(outlet_location)
    outlet_cell <- terra::cellFromXY(raster, st_coordinates(outlet_location))
    
    # ------------------------------------------------------------------------------------------------
    # is outlet location outside raster extent
    if(any(is.na(outlet_cell)) == TRUE){
      stop(paste0('\nWatershed_Delineator:\n\n',
                  'Outlet location passed is not within raster extent\n',
                  'Is passed CRS of outlet location correct?\n'))
    }
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # testing to see if outlet location is border cell (forbidden)
    # border is replaced with NA in first step of calculating flow direction, as direction cant be determined
    # when neighbors of cell are not defined
    edge1 <- 1:ncol(raster) # top edge
    edge2 <- seq(from = 1, to = ncell(raster), by = ncol(raster)) # left edge
    edge3 <- seq(from = ncol(raster), to = ncell(raster), by = ncol(raster)) # right edge
    edge4 <- seq(from = tail(edge2,1), to = tail(edge3,1), by = 1) # bottom edge
    if(any(outlet_cell %in% c(edge1, edge2, edge3, edge4))){
      stop(paste0('\nWatershed_Delineator:\n\n',
                  'Outlet location passed is on border cell of raster (forbidden)\n',
                  'Either pass more interior location or extend DEM past current location'))
    }
    # ------------------------------------------------------------------------------------------------
  }
  # ------------------------------------------------------------------------------------------------

  
  
  
  # ------------------------------------------------------------------------------------------------
  # make outlet location into a line if necessary
  if(is.null(outlet_location) == FALSE){
    if(outlet_location_is_line == TRUE){
      if(nrow(outlet_location) < 2){
        stop(paste0('\nWatershed_Delineator:\n\n',
                    'Outlet location is marked as line, yet only single point passed.\n',
                    'Please either designate outlet as a point, or pass multiple points\n',
                    'between which to draw a line.\n'))
      } else {
        # ------------------------------------------------------------------------------------------------
        # making linestring
        out_line <- st_coordinates(outlet_location)
        out_line <- st_linestring(out_line)
        # ------------------------------------------------------------------------------------------------
        
        # ------------------------------------------------------------------------------------------------
        # moving to sf object
        if(is.na(outlet_location_CRS) == FALSE){
          out_line <- st_sf(st_sfc(out_line),
                            crs = outlet_location_CRS)
        } else {
          out_line <- st_sf(st_sfc(out_line),
                            crs = st_crs(raster))
        }
        st_geometry(out_line) <- 'geometry'
        # ------------------------------------------------------------------------------------------------
        
        # ------------------------------------------------------------------------------------------------
        new_line_points <- custom_st_line_sample(sfobj = out_line,
                                                 n = outlet_location_line_density)
        outlet_location <- new_line_points
        outlet_cell <- terra::cellFromXY(raster, st_coordinates(new_line_points))
        outlet_cell <- unique(outlet_cell)
        # ------------------------------------------------------------------------------------------------
        
        
        # ------------------------------------------------------------------------------------------------
        # testing to see if outlet location is border cell (forbidden)
        # border is replaced with NA in first step of calculating flow direction, as direction cant be determined
        # when neighbors of cell are not defined
        edge1 <- 1:ncol(raster) # top edge
        edge2 <- seq(from = 1, to = ncell(raster), by = ncol(raster)) # left edge
        edge3 <- seq(from = ncol(raster), to = ncell(raster), by = ncol(raster)) # right edge
        edge4 <- seq(from = tail(edge2,1), to = tail(edge3,1), by = 1) # bottom edge
        if(any(outlet_cell %in% c(edge1, edge2, edge3, edge4))){
          stop(paste0('\nWatershed_Delineator:\n\n',
                      'Outlet location passed is on border cell of raster (forbidden)\n',
                      'Either pass more interior location or extend DEM past current location'))
        }
        # ------------------------------------------------------------------------------------------------
      }
    } else {}
  } else {}
  # ------------------------------------------------------------------------------------------------
  
  
  
  
  
  
  
  ############################################################################################################
  ################################## CORRECTIONS #############################################################
  ############################################################################################################
  
  
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
  } else {
    diff_x <- res(raster)[1]
    diff_y <- res(raster)[2]
    
    if(zunit == 'ft'){
      diff_x <- diff_x * 3.28
      diff_y <- diff_y * 3.28
    }
  }
  # ------------------------------------------------------------------------------------------------
  
  # ------------------------------------------------------------------------------------------------
  # padding values
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
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ############################################################################################################
  ################################## RUN FUNCTIONS ###########################################################
  ############################################################################################################

  ################################################## FLOW DIR FROM DEM #######################################
  
  # ------------------------------------------------------------------------------------------------
  # notify user
  if(suppress_console_messages == FALSE){
    mils <- ncell(raster)/1e6
    seconds <- mils*159
    minutes <- round(seconds/60,2)
    
    cat(paste0('\n####################################################################################\n'))
    cat(paste0('WATERSHED DELINEATOR\n'))
    cat(paste0('####################################################################################\n\n'))
    cat(paste0('\nFor details on the machine these functions were tested on please call wdl_machine_specs()\n\n'))
    cat(paste0('Calculating flow direction raster.\n',
               'Based on historical performance and the size of your raster this will take:\n',
               minutes,' minutes\n'))
    cat('Step (1/2)\n\n')
  }
  # ------------------------------------------------------------------------------------------------
  

  # ------------------------------------------------------------------------------------------------
  # building raster and writing out
  output <- flow_dir_of_DEM(raster = raster)
  
  flow_dir_deg_output <- as.numeric(as.vector(unlist(output[[1]])))
  flow_dir_rad_output <- as.numeric(as.vector(unlist(output[[2]])))
  flow_dir_slope_output <- as.numeric(as.vector(unlist(output[[3]])))
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
  
  
  writeRaster(stack,
              file.path(out_dir,paste0(flow_dir_rast_name,'.tif')),
              overwrite = TRUE)
  # ------------------------------------------------------------------------------------------------
  
  
  
  
  
  
  
  
  ################################################## UPSTREAM RECURSION #######################################
  
  # ------------------------------------------------------------------------------------------------
  # notify user
  if(suppress_console_messages == FALSE){

    cat(paste0('Calculating watershed of specified outlet point.\n'))
    cat('Step (2/2)\n')
  }
  # ------------------------------------------------------------------------------------------------
  
  # ------------------------------------------------------------------------------------------------
  output <- check_all_cells_flowing_to_outlet(raster = raster,
                                              diff_x = diff_x,
                                              diff_y = diff_y,
                                              flow_dir_deg_output = flow_dir_deg_output,
                                              outlet_cell = outlet_cell)
  # ------------------------------------------------------------------------------------------------
  
  # ------------------------------------------------------------------------------------------------
  flow_dir_rast <- rast(ncol = ncol(raster),
                        nrow = nrow(raster),
                        crs = crs(raster),
                        xmin = xmin(raster),
                        xmax = xmax(raster),
                        ymin = ymin(raster),
                        ymax = ymax(raster))
  flow_rast <- flow_dir_rast
  check_rast <- flow_dir_rast
  
  values(flow_rast) <- output[[2]]
  values(check_rast) <- output[[1]]
  
  stack <- c(flow_rast,
             check_rast)
  names(stack) <- c('Flow to Outlet','Checked Cells')
  # ------------------------------------------------------------------------------------------------
  
  # ------------------------------------------------------------------------------------------------
  # writeout
  writeRaster(stack,
              file.path(out_dir,paste0(flow_to_outlet_rast_name,'.tif')),
              overwrite = TRUE)
  # ------------------------------------------------------------------------------------------------
}
# ------------------------------------------------------------------------------------------------