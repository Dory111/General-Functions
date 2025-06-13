Find_Exterior_Points <- function(polygon_points,
                                 mean_len_mult = 0.2,
                                 len_out_divisor = 10)
{
  #-------------------------------------------------------------------------------
  # extracting the points
  points_df <- data.frame(latitude = NA,
                          longitude = NA)
  final_points_list <- list()
  #-------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------
  # Extracting the layers
  for(i in 1:nrow(polygon_points)){
    n_lay <- Extract_SF_MultiPolygon_Layers(polygon_points$geometry[i])[[1]]
    #-------------------------------------------------------------------------------
    # extracting the points from all layers of object[i]
    for(j in 1:n_lay){
      output <- Extract_SF_MultiPolygon_Vertices(polygon_points$geometry[i],
                                                 j)
      points_df <- rbind(points_df, data.frame(latitude = output[[1]],
                                               longitude = output[[2]]))
    }
    points_df <- points_df[-c(1),]
    #-------------------------------------------------------------------------------
    
    #-------------------------------------------------------------------------------
    # calculating the mean distance to all other points
    dist_tot_list <- list()
    dist_lat_list <- list()
    dist_lon_list <- list()
    for(k in round(seq(from = 1, to = nrow(points_df), length.out = nrow(points_df)/len_out_divisor),0)){
      longitude_diff <- points_df$longitude - points_df$longitude[k]
      latitude_diff <- points_df$latitude - points_df$latitude[k]
      diff_points_df <- data.frame(lon_diff = longitude_diff,
                                   lat_diff = latitude_diff,
                                   dist_tot = sqrt((longitude_diff^2)+(latitude_diff^2)))
      
      diff_points_df <- diff_points_df[-c(which(diff_points_df$dist_tot == 0)), ]
      dist_tot_list[[k]] <- diff_points_df$dist_tot 
      dist_lat_list[[k]] <- latitude_diff
      dist_lon_list[[k]] <- longitude_diff
    }
    dist_tot_list <- do.call(rbind, dist_tot_list)
    dist_lat_list <- do.call(rbind, dist_lat_list) 
    dist_lon_list <- do.call(rbind, dist_lon_list) 
    dist_lon_list <- dist_lon_list[dist_lon_list != 0] %>% abs()
    dist_lat_list <- dist_lat_list[dist_lat_list != 0] %>% abs()
    dist_tot_list <- dist_tot_list[dist_tot_list != 0]
    
    
    mean_dist_tot <- mean(dist_tot_list,na.rm = T)
    mean_dist_lat <- mean(dist_lat_list,na.rm = T)
    mean_dist_lon <- mean(dist_lon_list,na.rm = T)
    #-------------------------------------------------------------------------------
    
    
    #-------------------------------------------------------------------------------
    inds_to_remove <- c()
    for(x in 1:nrow(points_df)){
      lat_mat <- c(points_df$latitude[x] + mean_dist_lat*mult,
                   points_df$latitude[x] - mean_dist_lat*mult)
      lon_mat <- c(points_df$longitude[x] + mean_dist_lon*mult,
                   points_df$longitude[x] - mean_dist_lon*mult)
      flag1 <- (which(points_df$latitude > points_df$latitude[x] &
                        points_df$latitude < lat_mat[1] &
                        points_df$longitude > points_df$longitude[x] &
                        points_df$longitude < lon_mat[1])) # quadrant direction 1
      flag2 <- (which(points_df$latitude < points_df$latitude[x] &
                        points_df$latitude > lat_mat[2] &
                        points_df$longitude > points_df$longitude[x] &
                        points_df$longitude < lon_mat[1])) # quadrant direction 2
      flag3 <- (which(points_df$latitude < points_df$latitude[x] &
                        points_df$latitude > lat_mat[2] &
                        points_df$longitude < points_df$longitude[x] &
                        points_df$longitude > lon_mat[2])) # quadrant direction 3
      flag4 <- (which(points_df$latitude > points_df$latitude[x] &
                        points_df$latitude < lat_mat[1] &
                        points_df$longitude < points_df$longitude[x] &
                        points_df$longitude > lon_mat[2])) # quadrant direction 4
      
      
      flags <- c(length(flag1),
                 length(flag2),
                 length(flag3),
                 length(flag4)) # quadrant direction 4
      
      
      if(!(0 %in% flags)){
        inds_to_remove <- append(inds_to_remove, x)
      }
    }
    final_points_list[[i]] <- points_df[-c(inds_to_remove), ]
    #-------------------------------------------------------------------------------
  }
  #-------------------------------------------------------------------------------
  out_polys <- polygon_points
  
  #-------------------------------------------------------------------------------
  for(r in 1:length(final_points_list)){
    latitude_diff <- final_points_list[[r]]$latitude[1] - final_points_list[[r]]$latitude
    longitude_diff <- final_points_list[[r]]$longitude[1] - final_points_list[[r]]$longitude
    
    
    final_points_list[[r]]$dist <- sqrt((longitude_diff^2)+(latitude_diff^2))
    final_points_list[[r]] <- final_points_list[[r]][order(final_points_list[[r]]$dist),]
    
    final_points_list[[r]] <- rbind(final_points_list[[r]],
                                    data.frame(latitude = final_points_list[[r]]$latitude[1],
                                               longitude = final_points_list[[r]]$longitude[1],
                                               dist = 0))
    out_polys$geometry[r] <- st_polygon(list(cbind(final_points_list[[r]]$longitude,
                                                   final_points_list[[r]]$latitude)))
  }
  #-------------------------------------------------------------------------------
  return(out_polys)
}
#-------------------------------------------------------------------------------





#===========================================================================================
# must be the geometry itself and not the entire sf object
#===========================================================================================
Extract_SF_MultiPolygon_Vertices <- function(geometry,lay)
{
  
  geometry <- geometry[[1]]
  
  longitude <- geometry[[lay]][[1]][,1]
  
  latitude <- geometry[[lay]][[1]][,2]
  
  return(list(latitude,
              longitude))
}
#-------------------------------------------------------------------------------





#===========================================================================================
# must be the geometry itself and not the entire sf object
#===========================================================================================
Extract_SF_MultiPolygon_Layers <- function(geometry)
{
  
  nlay <- length(geometry[[1]])
  
  
  return(list(nlay))
  
  
}
#-------------------------------------------------------------------------------