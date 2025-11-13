# Written By:
#     ___         ___         ___                   ___                  _____        ___         ___               
#    /  /\       /__/\       /  /\      ___        /  /\                /  /::\      /  /\       /  /\        ___   
#   /  /:/       \  \:\     /  /::\    /  /\      /  /:/_              /  /:/\:\    /  /::\     /  /::\      /__/|  
#  /  /:/         \__\:\   /  /:/\:\  /  /:/     /  /:/ /\            /  /:/  \:\  /  /:/\:\   /  /:/\:\    |  |:|  
# /  /:/  ___ ___ /  /::\ /  /:/~/:/ /__/::\    /  /:/ /::\          /__/:/ \__\:|/  /:/  \:\ /  /:/~/:/    |  |:|  
#/__/:/  /  //__/\  /:/\:/__/:/ /:/__\__\/\:\__/__/:/ /:/\:\         \  \:\ /  /:/__/:/ \__\:/__/:/ /:/_____|__|:|  
#\  \:\ /  /:\  \:\/:/__\\  \:\/:::::/  \  \:\/\  \:\/:/~/:/          \  \:\  /:/\  \:\ /  /:\  \:\/:::::/__/::::\  
# \  \:\  /:/ \  \::/     \  \::/~~~~    \__\::/\  \::/ /:/            \  \:\/:/  \  \:\  /:/ \  \::/~~~~   ~\~~\:\ 
#  \  \:\/:/   \  \:\      \  \:\        /__/:/  \__\/ /:/              \  \::/    \  \:\/:/   \  \:\         \  \:\
#   \  \::/     \  \:\      \  \:\       \__\/     /__/:/                \__\/      \  \::/     \  \:\         \__\/
#    \__\/       \__\/       \__\/                 \__\/                             \__\/       \__\/              
#   
# 11/5/2025 - 11/7/2025
calculate_stream_depletions <- function(streams,
                                        streams_are_points = FALSE,
                                        stream_id_key = NULL,
                                        wells,
                                        wells_id_key = NULL,
                                        pumping,
                                        subwatersheds = NULL,
                                        influence_radius = NULL,
                                        proximity_criteria = 'adjacent',
                                        apportionment_criteria = 'inverse distance',
                                        analytical_model = 'glover',
                                        stream_depletion_output = 'volumetric',
                                        lagged_depletions_end_time = 0.99,
                                        lagged_depletions_start_time = 0.01,
                                        data_out_dir = getwd(),
                                        diag_out_dir = getwd(),
                                        suppress_loading_bar = TRUE,
                                        suppress_console_messages = TRUE,
                                        stor_coef_key = 'Stor',
                                        transmissivity_key = 'Tr')
{

  ############################################################################################
  ######################################### HELPER FUNCTIONS #################################
  ############################################################################################
  # ==================================================================================================
  # Simple function to concatenate a loading bar
  # ==================================================================================================
  loading_bar <- function(optional_text = '', iter, total, width = 50) 
  {
    if(iter == 1){
      cat('\n')
    }
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
    cat(sprintf("\r[%s%s] %3d%% %s",
                bar,
                space,
                round(100*pct),
                optional_text))
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    if (iter == total){
      cat("\n")
    }
    # ------------------------------------------------------------------------------------------------
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
  # must be the geometry itself and not the entire sf object
  #===========================================================================================
  Extract_SF_Linestring_Vertices <- function(geometry)
  {
    
    latitude <- geometry[1][[1]]
    latitude <- as.matrix(latitude)[,2]
    
    
    longitude <- geometry[1][[1]]
    longitude <- as.matrix(longitude)[,1]
    
    
    return(list(latitude,
                longitude))
  }
  #-------------------------------------------------------------------------------
  
  
  #===========================================================================================
  # complimentary error function
  # as x approaches 0 erfc returns 1. 
  # for example in glover model where Qs(t) = Qw*erfc(z)
  # where z = sqrt((d^2*S)/(4Tt))
  # as t approaches infinity the expression z approaches 0
  # and erfc returns 1
  # therefore at t = infinity the cumulative depletion will
  # be equal to the pumping rate
  # therefore when 99% of depletions have happened erfc (z) will return 0.99
  #===========================================================================================
  erfc <- function(x)
  {
    p <- 2 * pnorm(x * sqrt(2), lower.tail = FALSE)
    return(p)
  }
  #-------------------------------------------------------------------------------
  
  
  #===========================================================================================
  # inverse complimentary error function
  #===========================================================================================
  erfcinv <- function(y) {
    return(qnorm(1 - y / 2) / sqrt(2))
  }
  #-------------------------------------------------------------------------------
  
  #===========================================================================================
  # called to find stream points impacted by wells within the same watershed
  #===========================================================================================
  find_adjacent_stream_points <- function(wells,
                                          subwatersheds,
                                          stream_points_geometry)
  {
    #-------------------------------------------------------------------------------
    # for each well first check which subwatershed its within
    # then remove all other subwatersheds
    # in paired list check which stream points are also within that subwatershed
    impacted_points <- list()
    impacted_length <- list()
    for(i in 1:nrow(wells)){
      #-------------------------------------------------------------------------------
      if(suppress_loading_bar == FALSE){
        #-------------------------------------------------------------------------------
        # user message
        loading_bar(iter = i,
                    total = nrow(wells),
                    width = 50,
                    optional_text = '')
        #-------------------------------------------------------------------------------
      }
      #-------------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------------
      # check which subwatershed its within
      well_intersect_indices <- st_intersects(subwatersheds,
                                              wells[i, ])
      rm_empty_intersections <- which(lengths(well_intersect_indices) == 0)
      if(length(rm_empty_intersections) > 0){
        well_intersect_indices <- c(1:length(well_intersect_indices))[-c(rm_empty_intersections)]
      } else {
        well_intersect_indices <- c(1:length(well_intersect_indices))
      }
      #-------------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------------
      # if for some reason outside domain then place NA
      # else find all stream points within that subwatershed
      if(length(well_intersect_indices) == 0){
        impacted_points[[i]] <- as.character(NA)
        impacted_length[[i]] <- as.character(NA)
      } else {
        #-------------------------------------------------------------------------------
        # check which subwatershed touches original subwatershed
        subwatershed_touches_indices <- st_touches(subwatersheds,
                                                   subwatersheds[well_intersect_indices, ])
        subwatershed_touches_indices <- which(lengths(subwatershed_touches_indices) == 0)
        subwatershed_touches_indices <- c(1:length(subwatershed_touches_indices))[-c(rm_empty_intersections)]
        
        if(length(subwatershed_touches_indices) > 0){
          well_intersect_indices <- append(well_intersect_indices,
                                           subwatershed_touches_indices)
        } else {}
        #-------------------------------------------------------------------------------
        
        #-------------------------------------------------------------------------------
        # find which streams are within adjacent watersheds
        strm_intersect_indices <- st_intersects(stream_points_geometry,
                                                st_geometry(subwatersheds[well_intersect_indices, ]))
        rm_empty_intersections <- which(lengths(strm_intersect_indices) == 0)
        if(length(rm_empty_intersections) > 0){
          strm_intersect_indices <- c(1:length(strm_intersect_indices))[-c(rm_empty_intersections)]
        } else {
          strm_intersect_indices <- c(1:length(strm_intersect_indices))
        }
        #-------------------------------------------------------------------------------
        
        #-------------------------------------------------------------------------------
        # if there are no streams within the subwatershed
        if(length(strm_intersect_indices) == 0){
          impacted_points[[i]] <- as.character(NA)
          impacted_length[[i]] <- as.character(NA)
        } else {
          impacted_points[[i]] <- as.character(strm_intersect_indices)

          #-------------------------------------------------------------------------------
          # finding impacted length by reach
          all_keys <- as.vector(unlist(st_drop_geometry(stream_points_geometry[strm_intersect_indices,stream_id_key])))
          
          coords <- st_coordinates(stream_points_geometry[strm_intersect_indices, ])
          coords <- cbind(coords, all_keys)
          coords <- as.data.frame(coords)
          colnames(coords) <- c('x','y',stream_id_key)
          
          split_coords <- split(coords, coords[ ,stream_id_key])
          
          lines_list <- lapply(split_coords, function(x) {
            st_linestring(as.matrix(x[, c("x", "y")]))
          })
          lines_geometry <- st_sf(id = names(split_coords),
                                  geometry = st_sfc(lines_list),
                                  crs = st_crs(wells))
          #-------------------------------------------------------------------------------
          
          impacted_length[[i]] <- as.character(round(sum(st_length(lines_geometry)),0))
        }
        #-------------------------------------------------------------------------------
      }
      #-------------------------------------------------------------------------------
    }
    #-------------------------------------------------------------------------------
    
    #-------------------------------------------------------------------------------
    # making sure theyre all the same dimension
    max_impacted_n <- max(lengths(impacted_points))
    impacted_points <- lapply(impacted_points,
                              function(x) append(x,
                                                 rep(NA,max_impacted_n - length(x))))
    impacted_points <- do.call(rbind, impacted_points)
    impacted_length <- do.call(rbind, impacted_length)
    #-------------------------------------------------------------------------------
    
    #-------------------------------------------------------------------------------
    # formatting output
    w_index <- as.vector(unlist(st_drop_geometry(wells[ ,wells_id_key])))
    wells$ImpLMet <- as.vector(impacted_length)
    out <- cbind(w_index, impacted_points)
    colnames(out) <- c('wellN',
                       paste0('PN',c(1:max_impacted_n)))
    out <- as.data.frame(out)
    return(list(out,
                wells))
    #-------------------------------------------------------------------------------
  }
  #-------------------------------------------------------------------------------

  
  
  
  
  #===========================================================================================
  # called to find stream points impacted by wells within the same watershed
  #===========================================================================================
  find_adjacent_and_expanding_stream_points <- function(wells,
                                                        subwatersheds,
                                                        influence_radius,
                                                        stream_points_geometry)
  {
    #-------------------------------------------------------------------------------
    # for each well first check which subwatershed its within
    # then remove all other subwatersheds
    # in paired list check which stream points are also within that subwatershed
    impacted_points <- list()
    impacted_length <- list()
    for(i in 1:nrow(wells)){
      #-------------------------------------------------------------------------------
      if(suppress_loading_bar == FALSE){
        #-------------------------------------------------------------------------------
        # user message
        loading_bar(iter = i,
                    total = nrow(wells),
                    width = 50,
                    optional_text = '')
        #-------------------------------------------------------------------------------
      }
      #-------------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------------
      # check which subwatershed its within
      well_intersect_indices <- st_intersects(subwatersheds,
                                              wells[i, ])
      rm_empty_intersections <- which(lengths(well_intersect_indices) == 0)
      if(length(rm_empty_intersections) > 0){
        well_intersect_indices <- c(1:length(well_intersect_indices))[-c(rm_empty_intersections)]
      } else {
        well_intersect_indices <- c(1:length(well_intersect_indices))
      }
      #-------------------------------------------------------------------------------
      
      
      #-------------------------------------------------------------------------------
      # if for some reason outside domain then place NA
      # else find all stream points within that subwatershed
      if(length(well_intersect_indices) == 0){
        impacted_points[[i]] <- as.character(NA)
        impacted_length[[i]] <- as.character(NA)
      } else {
        #-------------------------------------------------------------------------------
        # check which subwatershed touches original subwatershed
        subwatershed_touches_indices <- st_touches(subwatersheds,
                                                   subwatersheds[well_intersect_indices, ])
        subwatershed_touches_indices <- which(lengths(subwatershed_touches_indices) == 0)
        subwatershed_touches_indices <- c(1:length(subwatershed_touches_indices))[-c(rm_empty_intersections)]
        
        if(length(subwatershed_touches_indices) > 0){
          well_intersect_indices <- append(well_intersect_indices,
                                           subwatershed_touches_indices)
        } else {}
        #-------------------------------------------------------------------------------
        
        #-------------------------------------------------------------------------------
        # find which streams are within adjacent watersheds
        strm_intersect_indices <- st_intersects(stream_points_geometry,
                                                st_geometry(subwatersheds[well_intersect_indices, ]))
        rm_empty_intersections <- which(lengths(strm_intersect_indices) == 0)
        if(length(rm_empty_intersections) > 0){
          strm_intersect_indices <- c(1:length(strm_intersect_indices))[-c(rm_empty_intersections)]
        } else {
          strm_intersect_indices <- c(1:length(strm_intersect_indices))
        }
        #-------------------------------------------------------------------------------
        
        #-------------------------------------------------------------------------------
        # find which streams are within influence radius
        expanding_indices <- st_intersects(stream_points_geometry,
                                           st_buffer(wells[i, ], influence_radius))
        rm_empty_intersections <- which(lengths(expanding_indices) == 0)
        if(length(rm_empty_intersections) > 0){
          expanding_indices <- c(1:length(expanding_indices))[-c(rm_empty_intersections)]
        } else {
          expanding_indices <- c(1:length(expanding_indices))
        }
        #-------------------------------------------------------------------------------
        
        #-------------------------------------------------------------------------------
        # append
        strm_intersect_indices <- append(strm_intersect_indices,
                                         expanding_indices)
        strm_intersect_indices <- unique(strm_intersect_indices)
        #-------------------------------------------------------------------------------
        
        #-------------------------------------------------------------------------------
        # if there are no streams within the subwatershed
        if(length(strm_intersect_indices) == 0){
          impacted_points[[i]] <- as.character(NA)
          impacted_length[[i]] <- as.character(NA)
        } else {
          impacted_points[[i]] <- as.character(strm_intersect_indices)
          
          #-------------------------------------------------------------------------------
          # finding impacted length by reach
          all_keys <- as.vector(unlist(st_drop_geometry(stream_points_geometry[strm_intersect_indices,stream_id_key])))
          
          coords <- st_coordinates(stream_points_geometry[strm_intersect_indices, ])
          coords <- cbind(coords, all_keys)
          coords <- as.data.frame(coords)
          colnames(coords) <- c('x','y',stream_id_key)
          
          split_coords <- split(coords, coords[ ,stream_id_key])
          
          lines_list <- lapply(split_coords, function(x) {
            st_linestring(as.matrix(x[, c("x", "y")]))
          })
          lines_geometry <- st_sf(id = names(split_coords),
                                  geometry = st_sfc(lines_list),
                                  crs = st_crs(wells))
          #-------------------------------------------------------------------------------
          
          impacted_length[[i]] <- as.character(round(sum(st_length(lines_geometry)),0))
        }
        #-------------------------------------------------------------------------------
      }
      #-------------------------------------------------------------------------------
    }
    #-------------------------------------------------------------------------------
    
    #-------------------------------------------------------------------------------
    # making sure theyre all the same dimension
    max_impacted_n <- max(lengths(impacted_points))
    impacted_points <- lapply(impacted_points,
                              function(x) append(x,
                                                 rep(NA,max_impacted_n - length(x))))
    impacted_points <- do.call(rbind, impacted_points)
    impacted_length <- do.call(rbind, impacted_length)
    #-------------------------------------------------------------------------------
    
    #-------------------------------------------------------------------------------
    # formatting output
    w_index <- as.vector(unlist(st_drop_geometry(wells[ ,wells_id_key])))
    wells$ImpLMet <- as.vector(impacted_length)
    out <- cbind(w_index, impacted_points)
    colnames(out) <- c('wellN',
                       paste0('PN',c(1:max_impacted_n)))
    out <- as.data.frame(out)
    return(list(out,
                wells))
    #-------------------------------------------------------------------------------
  }
  #-------------------------------------------------------------------------------
  
  
  
  
  
  
  
  
  #===========================================================================================
  # called to find stream points impacted by wells within an influence radius
  # calculated by Zipper et al (2019) https://doi.org/10.1029/2018WR024403
  # to be two times the maximum distance from any landpoint within the domain to its closest
  # stream segment
  # this base level assumption ensures 1 >= segments for each well
  #===========================================================================================
  find_local_stream_points <- function(wells,
                                       influence_radius,
                                       stream_points_geometry)
  {
    #-------------------------------------------------------------------------------
    # for each well first check which subwatershed its within
    # then remove all other subwatersheds
    # in paired list check which stream points are also within that subwatershed
    impacted_points <- list()
    impacted_length <- list()
    for(i in 1:nrow(wells)){
      
      #-------------------------------------------------------------------------------
      if(suppress_loading_bar == FALSE){
        #-------------------------------------------------------------------------------
        # user message
        loading_bar(iter = i,
                    total = nrow(wells),
                    width = 50,
                    optional_text = '')
        #-------------------------------------------------------------------------------
      }
      #-------------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------------
      # find all stream points within influence radius
      strm_intersect_indices <- st_intersects(stream_points_geometry,
                                              st_buffer(wells[i, ], influence_radius))
      rm_empty_intersections <- which(lengths(strm_intersect_indices) == 0)
      if(length(rm_empty_intersections) > 0){
        strm_intersect_indices <- c(1:length(strm_intersect_indices))[-c(rm_empty_intersections)]
      } else {
        strm_intersect_indices <- c(1:length(strm_intersect_indices))
      }
      #-------------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------------
      # if there are no streams within the radius
      # (should not be possible with propper Zipper 2019 implementation)
      if(length(strm_intersect_indices) == 0){
        impacted_points[[i]] <- as.character(NA)
        impacted_length[[i]] <- as.character(NA)
      } else {
        impacted_points[[i]] <- as.character(strm_intersect_indices)
        
        #-------------------------------------------------------------------------------
        # finding impacted length by reach
        all_keys <- as.vector(unlist(st_drop_geometry(stream_points_geometry[strm_intersect_indices,stream_id_key])))
        
        coords <- st_coordinates(stream_points_geometry[strm_intersect_indices, ])
        coords <- cbind(coords, all_keys)
        coords <- as.data.frame(coords)
        colnames(coords) <- c('x','y',stream_id_key)
        
        split_coords <- split(coords, coords[ ,stream_id_key])
        
        lines_list <- lapply(split_coords, function(x) {
          st_linestring(as.matrix(x[, c("x", "y")]))
        })
        lines_geometry <- st_sf(id = names(split_coords),
                                geometry = st_sfc(lines_list),
                                crs = st_crs(wells))
        #-------------------------------------------------------------------------------
        
        impacted_length[[i]] <- as.character(round(sum(st_length(lines_geometry)),0))
      }
      #-------------------------------------------------------------------------------
    }
    #-------------------------------------------------------------------------------
    
    #-------------------------------------------------------------------------------
    # making sure theyre all the same dimension
    max_impacted_n <- max(lengths(impacted_points))
    impacted_points <- lapply(impacted_points,
                              function(x) append(x,
                                                 rep(NA,max_impacted_n - length(x))))
    impacted_points <- do.call(rbind, impacted_points)
    impacted_length <- do.call(rbind, impacted_length)
    #-------------------------------------------------------------------------------
    
    #-------------------------------------------------------------------------------
    # formatting output
    w_index <- as.vector(unlist(st_drop_geometry(wells[ ,wells_id_key])))
    wells$ImpLMet <- as.vector(impacted_length)
    out <- cbind(w_index, impacted_points)
    colnames(out) <- c('wellN',
                       paste0('PN',c(1:max_impacted_n)))
    out <- as.data.frame(out)
    return(list(out,
                wells))
    #-------------------------------------------------------------------------------
  }
  #-------------------------------------------------------------------------------
  
  
  
  #===========================================================================================
  # all wells impacted by all points
  #===========================================================================================
  find_whole_domain_points <- function(wells,
                                       stream_points_geometry)
  {
    #-------------------------------------------------------------------------------
    # for each well first check which subwatershed its within
    # then remove all other subwatersheds
    # in paired list check which stream points are also within that subwatershed
    impacted_points <- list()
    impacted_length <- list()
    for(i in 1:nrow(wells)){
      
      #-------------------------------------------------------------------------------
      if(suppress_loading_bar == FALSE){
        #-------------------------------------------------------------------------------
        # user message
        loading_bar(iter = i,
                    total = nrow(wells),
                    width = 50,
                    optional_text = '')
        #-------------------------------------------------------------------------------
      }
      #-------------------------------------------------------------------------------
      
      
      #-------------------------------------------------------------------------------
      # finding impacted length by reach
      all_keys <- as.vector(unlist(st_drop_geometry(stream_points_geometry[ ,stream_id_key])))
      
      coords <- st_coordinates(stream_points_geometry)
      coords <- cbind(coords, all_keys)
      coords <- as.data.frame(coords)
      colnames(coords) <- c('x','y',stream_id_key)
      
      split_coords <- split(coords, coords[ ,stream_id_key])
      
      lines_list <- lapply(split_coords, function(x) {
        st_linestring(as.matrix(x[, c("x", "y")]))
      })
      lines_geometry <- st_sf(id = names(split_coords),
                              geometry = st_sfc(lines_list),
                              crs = st_crs(wells))
      #-------------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------------
      impacted_points[[i]] <- as.character(c(1:nrow(stream_points_geometry)))
      impacted_length[[i]] <- as.character(round(sum(st_length(lines_geometry)),0))
      #-------------------------------------------------------------------------------
    }
    #-------------------------------------------------------------------------------
    
    #-------------------------------------------------------------------------------
    # making sure theyre all the same dimension
    max_impacted_n <- max(lengths(impacted_points))
    impacted_points <- lapply(impacted_points,
                              function(x) append(x,
                                                 rep(NA,max_impacted_n - length(x))))
    impacted_points <- do.call(rbind, impacted_points)
    impacted_length <- do.call(rbind, impacted_length)
    #-------------------------------------------------------------------------------
    
    #-------------------------------------------------------------------------------
    # formatting output
    w_index <- as.vector(unlist(st_drop_geometry(wells[ ,wells_id_key])))
    wells$ImpLMet <- as.vector(impacted_length)
    out <- cbind(w_index, impacted_points)
    colnames(out) <- c('wellN',
                       paste0('PN',c(1:max_impacted_n)))
    out <- as.data.frame(out)
    return(list(out,
                wells))
    #-------------------------------------------------------------------------------
  }
  #-------------------------------------------------------------------------------
  
  
  
  #===========================================================================================
  # convert everything to the projection of the wells
  #===========================================================================================
  ensure_projections <- function(wells,
                                 geometry_list)
  {
    new_list <- list()
    for(i in 1:length(geometry_list)){
      new_list[[i]] <- st_transform(geometry_list[[i]],
                                    st_crs(wells))
    }
    return(new_list)
  }
  #-------------------------------------------------------------------------------
  
  
  
  
  #===========================================================================================
  # For each well, find the closest point on the reaches that it impacts
  #===========================================================================================
  find_closest_points_per_segment <- function(wells,
                                              stream_points_geometry,
                                              stream_id_key){
    #-------------------------------------------------------------------------------
    # write status to log
    writeLines(text = sprintf('%s',
                              'For each well, finding the closest point on the reaches that it impacts'),
               con = log_file)
    #-------------------------------------------------------------------------------
    
    #-------------------------------------------------------------------------------
    # blank data to be filled
    blank_matrix <- base::matrix(nrow = nrow(wells),
                                 ncol = nrow(streams))
    blank_matrix <- as.data.frame(blank_matrix)
    colnames(blank_matrix) <- c(1:ncol(blank_matrix))
    #-------------------------------------------------------------------------------
    
    #-------------------------------------------------------------------------------
    # get the closest point on each stream segment associated with each well
    stats <- list()
    for(i in 1:nrow(wells)){
      #-------------------------------------------------------------------------------
      if(suppress_loading_bar == FALSE){
        #-------------------------------------------------------------------------------
        # user message
        loading_bar(iter = i,
                    total = nrow(wells),
                    width = 50,
                    optional_text = 'Closest point by reach')
        #-------------------------------------------------------------------------------
      }
      #-------------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------------
      imp_pts <- as.vector(unlist(impacted_points[i, ])) # get all impacted points for this well
      imp_pts <- as.numeric(imp_pts)[-c(1)] # get rid of well number
      #-------------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------------
      # if there are no impacted points there can not be a closest one
      # additionally, trying to index the stream points data frame by a NA, or numeric(0)
      # would cause an error
      if(all(is.na(imp_pts)) == TRUE){
        # pass
        stats[[i]] <- NA
      } else {
        rm_NA_index <- which(is.na(imp_pts) == TRUE)
        if(length(rm_NA_index) > 0){
          imp_pts <- imp_pts[-c(rm_NA_index)] # removing NA indexes
        }
        
        #-------------------------------------------------------------------------------
        # for each stream reach impacted by the well, what is the closest point on that segment
        stream_points_subset <- stream_points_geometry[imp_pts, ]
        stream_points_subset_inner <- split(stream_points_subset,
                                            st_drop_geometry(stream_points_subset[ ,stream_id_key]))
        #-------------------------------------------------------------------------------
        
        #-------------------------------------------------------------------------------
        closest_points <- lapply(stream_points_subset_inner,
                                 function(x){
                                   rownames <- row.names(x)
                                   closest_index <- rownames[which.min(st_distance(wells[i, ], x))]
                                   as.numeric(closest_index)
                                 })
        stats[[i]] <- as.vector(unlist(closest_points))
        #-------------------------------------------------------------------------------
        
        #-------------------------------------------------------------------------------
        blank_matrix[i, as.numeric(colnames(blank_matrix)) %in% as.numeric(names(closest_points))] <- 
          closest_points
        #-------------------------------------------------------------------------------
      }
      #-------------------------------------------------------------------------------
    }
    #-------------------------------------------------------------------------------
    
    #-------------------------------------------------------------------------------
    max_closest_n <- max(lengths(stats))
    mean_closest_n <- round(mean(lengths(stats)),0)
    median_closest_n <- round(median(lengths(stats)),0)
    #-------------------------------------------------------------------------------

    #-------------------------------------------------------------------------------
    # write status to log
    writeLines(text = sprintf('%s %s',
                              'Max | Mean | Median number of reaches effected per well: ',
                              paste(max_closest_n,
                                    '|',
                                    mean_closest_n,
                                    '|',
                                    median_closest_n)),
               con = log_file)
    #-------------------------------------------------------------------------------
    
    return(blank_matrix)
  }
  #-------------------------------------------------------------------------------
  
  
  
  #===========================================================================================
  # Apportion depletions by inverse distance method
  #===========================================================================================
  inverse_distance_apportionment <- function(power,
                                             wells,
                                             closest_points_per_segment,
                                             stream_points_geometry)
  {
    #-------------------------------------------------------------------------------
    # inverse distance equation in zipper
    # https://doi.org/10.1029/2018WR024403
    equation <- function(well,
                         power = power,
                         closest_points_per_well,
                         stream_points_geometry = stream_points_geometry)
    {
      closest_points <- stream_points_geometry[closest_points_per_well, ]
      reaches <- st_drop_geometry(stream_points_geometry[closest_points_per_well,stream_id_key])
      reaches <- as.vector(unlist(reaches))
      
      dists <- as.vector(st_distance(well,
                                     st_geometry(closest_points)))
      
      numerator <- 1/(dists**power)
      denominator <- sum(1/(dists**power))
      fractions_of_depletions <- numerator/denominator
      
      return(list(fractions_of_depletions,
                  reaches))
    }
    #-------------------------------------------------------------------------------
    
    
    #-------------------------------------------------------------------------------
    # use inverse distance weighting to assign fractions of depletions
    fractions_of_depletions <- as.data.frame(base::matrix(nrow = nrow(wells),
                                                          ncol = nrow(streams)))
    colnames(fractions_of_depletions) <- c(1:ncol(fractions_of_depletions))
    reaches <- as.data.frame(base::matrix(nrow = nrow(wells),
                                          ncol = nrow(streams)))
    colnames(reaches) <- c(1:ncol(reaches))
    for(i in 1:nrow(wells)){
      #-------------------------------------------------------------------------------
      if(suppress_loading_bar == FALSE){
        #-------------------------------------------------------------------------------
        # user message
        loading_bar(iter = i,
                    total = nrow(wells),
                    width = 50,
                    optional_text = 'Apportioning depletions')
        #-------------------------------------------------------------------------------
      }
      #-------------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------------
      closest_points_per_well <- closest_points_per_segment[i,-c(1)]
      reference <- as.vector(unlist(closest_points_per_well))
      closest_points_per_well <- as.vector(unlist(closest_points_per_well))
      #-------------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------------
      # are there any depletions to apportion? if yes continue, if not append NA
      if(all(is.na(closest_points_per_well)) == FALSE){
        
        
        #-------------------------------------------------------------------------------
        # are there any NAs to remove to avoid indexing the stream points by NA
        rm_indices <- which(is.na(closest_points_per_well) == TRUE)
        if(length(rm_indices) > 0){
          #-------------------------------------------------------------------------------
          closest_points_per_well <- closest_points_per_well[-c(rm_indices)] %>%
            as.vector() %>% unlist()
          #-------------------------------------------------------------------------------
        } else {}
        #-------------------------------------------------------------------------------
        
        #-------------------------------------------------------------------------------
        # assign fractions
        output <- equation(well = wells[i, ],
                           power = power,
                           closest_points_per_well = closest_points_per_well,
                           stream_points_geometry = stream_points_geometry)
        fractions_of_depletions[i,
                                is.na(reference) == FALSE] <- output[[1]]
        reaches[i,
                is.na(reference) == FALSE] <- output[[2]]
        #-------------------------------------------------------------------------------
        
      } else {
        # pass
      }
      #-------------------------------------------------------------------------------
    }
    #-------------------------------------------------------------------------------
  
    
    #-------------------------------------------------------------------------------
    # writeout statistics
    max_dep <- max(fractions_of_depletions, na.rm = T)
    wm <- which(fractions_of_depletions == max_dep, arr.ind = TRUE)
    reach_max_dep <- reaches[wm]
    well_max_dep <- unlist(as.vector(st_drop_geometry(wells[wm[1,1],wells_id_key])))
    #-------------------------------------------------------------------------------
    

    #-------------------------------------------------------------------------------
    # format writeout pt 2
    w_index <- as.vector(unlist(st_drop_geometry(wells[ ,wells_id_key])))
    fractions_of_depletions <- cbind(w_index, fractions_of_depletions)
    fractions_of_depletions <- as.data.frame(fractions_of_depletions)
    colnames(fractions_of_depletions) <- c('wellN',
                                           paste0('RN',
                                                  1:(ncol(fractions_of_depletions)-1)))
    reaches <- cbind(w_index, reaches)
    reaches <- as.data.frame(reaches)
    colnames(reaches) <- c('wellN',
                            paste0('RN',
                                   1:(ncol(reaches)-1)))
    #-------------------------------------------------------------------------------
    

    #-------------------------------------------------------------------------------
    # write status to log
    writeLines(text = sprintf('%s %s',
                              'Max apportioned depletion fraction: ',
                              paste(round(max_dep,4),
                                    'for reach',
                                    reach_max_dep,
                                    'for well',
                                    well_max_dep)),
               con = log_file)
    #-------------------------------------------------------------------------------
    
    
    #-------------------------------------------------------------------------------
    return(list(fractions_of_depletions,
                reaches))
    #-------------------------------------------------------------------------------
  }
  #-------------------------------------------------------------------------------
  
  
  
  #===========================================================================================
  # Apportion depletions by thiessen polygon method
  # method taken from Zipper (2018) https://doi.org/10.1029/2018WR022707
  #===========================================================================================
  thiessen_polygon_apportionment <- function(wells,
                                             closest_points_per_segment,
                                             stream_points_geometry)
  {
    #-------------------------------------------------------------------------------
    fractions_of_depletions <- as.data.frame(base::matrix(nrow = nrow(wells),
                                                          ncol = nrow(streams)))
    colnames(fractions_of_depletions) <- c(1:ncol(fractions_of_depletions))
    reaches <- as.data.frame(base::matrix(nrow = nrow(wells),
                                          ncol = nrow(streams)))
    colnames(reaches) <- c(1:ncol(reaches))
    for(i in 1:nrow(wells)){
      #-------------------------------------------------------------------------------
      if(suppress_loading_bar == FALSE){
        #-------------------------------------------------------------------------------
        # user message
        loading_bar(iter = i,
                    total = nrow(wells),
                    width = 50,
                    optional_text = 'Apportioning Depletions')
        #-------------------------------------------------------------------------------
      }
      #-------------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------------
      # find closest points on stream reaches for that well
      closest_points_subset <- closest_points_per_segment[i,-c(1)]
      rm <- which(is.na(closest_points_subset))
      if(length(rm) > 0){
        reference <- closest_points_subset
        closest_points_subset <- as.vector(unlist(closest_points_subset[-c(which(is.na(closest_points_subset)))]))
      } else {
        reference <- closest_points_subset
        closest_points_subset <- as.vector(unlist(closest_points_subset))
      }
      #-------------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------------
      # if there are closest points continue
      if(length(closest_points_subset) > 0){
        
        #-------------------------------------------------------------------------------
        # generate thiessen polygons out of the closest points
        closest_stream_points <- stream_points_geometry[closest_points_subset, ]
        closest_stream_points$key <- paste(st_coordinates(closest_stream_points)[ ,1],
                                           st_coordinates(closest_stream_points)[ ,2],
                                           sep = '_')
        closest_stream_points <- closest_stream_points[ ,'key']
        closest_stream_points <- unique(closest_stream_points)
        closest_stream_points$BUFF <- rep(NA, nrow(closest_stream_points))
        
        closest_voronoi <- st_voronoi(st_union(st_geometry(closest_stream_points)))
        closest_voronoi <- st_collection_extract(closest_voronoi,'POLYGON')
        closest_voronoi <- st_sf(geometry = closest_voronoi, crs = st_crs(closest_voronoi))
        closest_voronoi$key <- closest_stream_points$key
        
        closest_stream_points <- closest_stream_points[ ,'BUFF']
        #-------------------------------------------------------------------------------
        
        
        #-------------------------------------------------------------------------------
        # generate thiessen polygons out of the closest points plus the well
        wells_tmp <- wells[i, ]
        wells_tmp$BUFF <- rep(NA,nrow(wells_tmp))
        wells_tmp <- wells_tmp[ ,'BUFF']
        closest_stream_points_plus_well <- rbind(wells_tmp, closest_stream_points)
        
        closest_voronoi_plus_well <- st_voronoi(st_union(st_geometry(closest_stream_points_plus_well)))
        closest_voronoi_plus_well <- st_collection_extract(closest_voronoi_plus_well,'POLYGON')
        closest_voronoi_plus_well <- st_sf(geometry = closest_voronoi_plus_well, crs = st_crs(closest_voronoi_plus_well))
        #-------------------------------------------------------------------------------
        
        
        #-------------------------------------------------------------------------------
        # which thiessen polygon does the well intersect
        wells_intersection <- st_intersects(closest_voronoi_plus_well, wells[i, ])
        rm <- which(lengths(wells_intersection) == 0)
        if(length(rm) > 0){
          wells_intersection <- c(1:length(wells_intersection))[-c(rm)]
        } else {}
        wells_intersection <- closest_voronoi_plus_well[wells_intersection, ]
        #-------------------------------------------------------------------------------
        
        #-------------------------------------------------------------------------------
        # get areas
        well_voronoi_area <- as.numeric(st_area(wells_intersection))
        closest_voronoi_intersection <- st_intersection(closest_voronoi, st_geometry(wells_intersection))
        voronoi_intersected_areas <- as.numeric(st_area(closest_voronoi_intersection))
        #-------------------------------------------------------------------------------
        
        #-------------------------------------------------------------------------------
        # find which intersections get what depletion fraction
        apportioned_depletions <- voronoi_intersected_areas/well_voronoi_area
        apportioned_depletions <- data.frame(dep = apportioned_depletions,
                                             key = closest_voronoi_intersection$key)
        #-------------------------------------------------------------------------------
        
        #-------------------------------------------------------------------------------
        # get which voronoi polygons these depletions should be assigned to
        surrounding_voronoi <- st_intersects(closest_voronoi, closest_voronoi_intersection)
        rm <- which(lengths(surrounding_voronoi) == 0)
        if(length(rm) > 0){
          surrounding_voronoi <- c(1:length(surrounding_voronoi))[-c(rm)]
        } else {}
        surrounding_voronoi <- closest_voronoi[surrounding_voronoi, ]
        #-------------------------------------------------------------------------------
        
        
        #-------------------------------------------------------------------------------
        # re-creating original points
        closest_stream_points <- stream_points_geometry[closest_points_subset, ]
        closest_stream_points$key <- paste(st_coordinates(closest_stream_points)[ ,1],
                                           st_coordinates(closest_stream_points)[ ,2],
                                           sep = '_')
        closest_stream_points <- closest_stream_points[ ,'key']
        #-------------------------------------------------------------------------------
        
        #-------------------------------------------------------------------------------
        # in case more than one point got collapsed to a single voronoi polygon
        depletions_accounting_duplicates <- lapply(apportioned_depletions$key, function(x){
          length(which(closest_stream_points$key == x))
        })
        depletions_accounting_duplicates <- unlist(depletions_accounting_duplicates)
        #-------------------------------------------------------------------------------
        
        #-------------------------------------------------------------------------------
        # dividing depletions among duplicates (if exists)
        apportioned_depletions$dep <- 
          apportioned_depletions$dep/depletions_accounting_duplicates
        apportioned_depletions$divisor <- depletions_accounting_duplicates
        #-------------------------------------------------------------------------------
        
        #-------------------------------------------------------------------------------
        # assigning depletions to points in the correct positions
        frac <- lapply(closest_stream_points$key, function(x){
          apportioned_depletions$dep[apportioned_depletions$key == x]
        })
        frac[lengths(frac) == 0] <- NA
        frac <- unlist(frac)
        closest_stream_points$frac <- frac
        #-------------------------------------------------------------------------------
        
        #-------------------------------------------------------------------------------
        # finding what reaches these depletions apply to
        reach_names <- as.numeric(row.names(closest_stream_points)[(is.na(closest_stream_points$frac) == FALSE)])
        
        reach_names <- st_drop_geometry(stream_points_geometry[reach_names, stream_id_key])
        reach_names <- as.vector(unlist(reach_names))
        closest_stream_points$reach_name <- rep(NA, nrow(closest_stream_points))
        closest_stream_points$reach_name[is.na(closest_stream_points$frac) == FALSE] <- reach_names
        #-------------------------------------------------------------------------------
        
        #-------------------------------------------------------------------------------
        # output
        fractions_of_depletions[i,
                                as.numeric(colnames(fractions_of_depletions)) %in% as.numeric(reach_names)] <-
          frac[is.na(frac) == FALSE]
        reaches[i, as.numeric(colnames(reaches)) %in% as.numeric(reach_names)] <-
          as.vector(reach_names)
        #-------------------------------------------------------------------------------
      } else {
        # pass
      }
      #-------------------------------------------------------------------------------
    }
    #-------------------------------------------------------------------------------
    
    
    
    #-------------------------------------------------------------------------------
    # writeout statistics
    max_dep <- max(fractions_of_depletions, na.rm = T)
    wm <- which(fractions_of_depletions == max_dep, arr.ind = TRUE)
    reach_max_dep <- reaches[wm]
    well_max_dep <- unlist(as.vector(st_drop_geometry(wells[wm[1,1],wells_id_key])))
    #-------------------------------------------------------------------------------
    
    
    #-------------------------------------------------------------------------------
    # format writeout pt 2
    w_index <- as.vector(unlist(st_drop_geometry(wells[ ,wells_id_key])))
    fractions_of_depletions <- cbind(w_index, fractions_of_depletions)
    fractions_of_depletions <- as.data.frame(fractions_of_depletions)
    colnames(fractions_of_depletions) <- c('wellN',
                                           paste0('RN',
                                                  1:(ncol(fractions_of_depletions)-1)))
    reaches <- cbind(w_index, reaches)
    reaches <- as.data.frame(reaches)
    colnames(reaches) <- c('wellN',
                           paste0('RN',
                                  1:(ncol(reaches)-1)))
    #-------------------------------------------------------------------------------
    
    
    #-------------------------------------------------------------------------------
    # write status to log
    writeLines(text = sprintf('%s %s',
                              'Max apportioned depletion fraction: ',
                              paste(round(max_dep,4),
                                    'for reach',
                                    reach_max_dep,
                                    'for well',
                                    well_max_dep)),
               con = log_file)
    #-------------------------------------------------------------------------------
    
    
    #-------------------------------------------------------------------------------
    return(list(fractions_of_depletions,
                reaches))
    #-------------------------------------------------------------------------------
  }
  #-------------------------------------------------------------------------------
  
  
  #===========================================================================================
  # Apportion depletions by web or web squared method
  #===========================================================================================
  web_apportionment <- function(power,
                                wells,
                                closest_points_per_segment,
                                stream_points_geometry)
  {
    #-------------------------------------------------------------------------------
    # web equation in zipper (2018)
    # https://doi.org/10.1029/2018WR022707
    equation <- function(well,
                         power = power,
                         closest_points_per_well,
                         stream_points_geometry = stream_points_geometry)
    {
      #-------------------------------------------------------------------------------
      # finding the closest points and the reaches that those points represent
      closest_points <- stream_points_geometry[closest_points_per_well, ]
      reaches <- st_drop_geometry(stream_points_geometry[closest_points_per_well,stream_id_key])
      reaches <- as.vector(unlist(reaches))
      #-------------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------------
      # get all the points per reach
      n_points_per_reach_proximity <- split(stream_points_geometry,
                                            st_drop_geometry(stream_points_geometry[ ,stream_id_key]))
      n_points_per_reach_proximity <- n_points_per_reach_proximity[reaches]
      #-------------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------------
      # get sum per reach of the point distances to the well
      dists <- lapply(n_points_per_reach_proximity, function(x){
        s1 <- st_distance(well,
                          x)
        sum(as.vector(unlist(s1)))
      })
      dists <- as.vector(unlist(dists))
      #-------------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------------
      # repeat above but with all points
      n_points_per_reach_all <- split(stream_points_geometry,
                                      st_drop_geometry(stream_points_geometry[ ,stream_id_key]))
      n_points_per_reach_all <- n_points_per_reach_all[reaches]
      #-------------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------------
      # repeat above but with all points
      dists_all <- lapply(n_points_per_reach_all, function(x){
        s1 <- st_distance(well,
                          x)
        sum(as.vector(unlist(s1)))
      })
      dists_all <- as.vector(unlist(dists_all))
      #-------------------------------------------------------------------------------
      
      
      #-------------------------------------------------------------------------------
      numerator <- 1/(dists**power)
      denominator <- sum(1/(dists_all**power))
      fractions_of_depletions <- numerator/denominator
      #-------------------------------------------------------------------------------
      
      
      return(list(fractions_of_depletions,
                  reaches))
    }
    #-------------------------------------------------------------------------------
    
    
    #-------------------------------------------------------------------------------
    # use inverse distance weighting to assign fractions of depletions
    fractions_of_depletions <- as.data.frame(base::matrix(nrow = nrow(wells),
                                                          ncol = nrow(streams)))
    colnames(fractions_of_depletions) <- c(1:ncol(fractions_of_depletions))
    reaches <- as.data.frame(base::matrix(nrow = nrow(wells),
                                          ncol = nrow(streams)))
    colnames(reaches) <- c(1:ncol(reaches))
    for(i in 1:nrow(wells)){
      #-------------------------------------------------------------------------------
      if(suppress_loading_bar == FALSE){
        #-------------------------------------------------------------------------------
        # user message
        loading_bar(iter = i,
                    total = nrow(wells),
                    width = 50,
                    optional_text = 'Apportioning depletions')
        #-------------------------------------------------------------------------------
      }
      #-------------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------------
      closest_points_per_well <- closest_points_per_segment[i,-c(1)]
      reference <- as.vector(unlist(closest_points_per_well))
      closest_points_per_well <- as.vector(unlist(closest_points_per_well))
      #-------------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------------
      # are there any depletions to apportion? if yes continue, if not append NA
      if(all(is.na(closest_points_per_well)) == FALSE){
        
        
        #-------------------------------------------------------------------------------
        # are there any NAs to remove to avoid indexing the stream points by NA
        rm_indices <- which(is.na(closest_points_per_well) == TRUE)
        if(length(rm_indices) > 0){
          #-------------------------------------------------------------------------------
          closest_points_per_well <- closest_points_per_well[-c(rm_indices)] %>%
            as.vector() %>% unlist()
          #-------------------------------------------------------------------------------
        } else {}
        #-------------------------------------------------------------------------------
        
        #-------------------------------------------------------------------------------
        # assign fractions
        output <- equation(well = wells[i, ],
                           power = power,
                           closest_points_per_well = closest_points_per_well,
                           stream_points_geometry = stream_points_geometry)
        fractions_of_depletions[i,
                                is.na(reference) == FALSE] <- output[[1]]
        reaches[i,
                is.na(reference) == FALSE] <- output[[2]]
        #-------------------------------------------------------------------------------
        
      } else {
        # pass
      }
      #-------------------------------------------------------------------------------
    }
    #-------------------------------------------------------------------------------
    
    
    #-------------------------------------------------------------------------------
    # writeout statistics
    max_dep <- max(fractions_of_depletions, na.rm = T)
    wm <- which(fractions_of_depletions == max_dep, arr.ind = TRUE)
    reach_max_dep <- reaches[wm]
    well_max_dep <- unlist(as.vector(st_drop_geometry(wells[wm[1,1],wells_id_key])))
    #-------------------------------------------------------------------------------
    
    
    #-------------------------------------------------------------------------------
    # format writeout pt 2
    w_index <- as.vector(unlist(st_drop_geometry(wells[ ,wells_id_key])))
    fractions_of_depletions <- cbind(w_index, fractions_of_depletions)
    fractions_of_depletions <- as.data.frame(fractions_of_depletions)
    colnames(fractions_of_depletions) <- c('wellN',
                                           paste0('RN',
                                                  1:(ncol(fractions_of_depletions)-1)))
    reaches <- cbind(w_index, reaches)
    reaches <- as.data.frame(reaches)
    colnames(reaches) <- c('wellN',
                           paste0('RN',
                                  1:(ncol(reaches)-1)))
    #-------------------------------------------------------------------------------
    
    
    #-------------------------------------------------------------------------------
    # write status to log
    writeLines(text = sprintf('%s %s',
                              'Max apportioned depletion fraction: ',
                              paste(round(max_dep,4),
                                    'for reach',
                                    reach_max_dep,
                                    'for well',
                                    well_max_dep)),
               con = log_file)
    #-------------------------------------------------------------------------------
    
    
    #-------------------------------------------------------------------------------
    return(list(fractions_of_depletions,
                reaches))
    #-------------------------------------------------------------------------------
  }
  #-------------------------------------------------------------------------------
  
  
  
  
  
  
  #===========================================================================================
  # Uses glover model to calculate the depletions per reach
  #===========================================================================================
  glover_stream_depletion_calculations <- function(closest_points_per_segment = closest_points_per_segment,
                                                   stream_points_geometry = stream_points_geometry,
                                                   reach_impact_frac = reach_impact_frac,
                                                   wells = wells,
                                                   transmissivity_key = transmissivity_key,
                                                   stor_coef_key = stor_coef_key,
                                                   stream_depletion_output = stream_depletion_output)
  {
    #===========================================================================================
    # Calculates stream depletions assuming a fully penetrating stream with no
    # clogging layer according to Glover and Balmer (1954) https://doi.org/10.1029/TR035i003p00468
    #===========================================================================================
    glover_stream_depletion_model <- function(stor_coef,
                                              transmissivity,
                                              distance,
                                              QW)
    {
      #-------------------------------------------------------------------------------
      # glover model equation in zipper (2019)
      # https://doi.org/10.1029/2018WR024403
      equation <- function(stor_coef,
                           transmissivity,
                           elapsed_time,
                           distance)
      {
        QA <- erfc(sqrt((stor_coef * distance**2)/
                          (4*transmissivity*elapsed_time)))
        
        
        return(QA)
      }
      #-------------------------------------------------------------------------------
      
      
      # EXPLANATION
      # The following matrices are an abstraction of the principle of linear superposition.
      # In these matrices, each column is a different pumping rate.
      # This method is necessary as analytical stream depletion functions do not return
      # the depletion at timestep t, but rather the cumulative depletion between 0 and t.
      # Therefore the depletion at timestep t is actually f(t) - f(t-1).
      
      # The matrix [timestep_mat] shows, as stated, each column as a pumping rate and
      # each row as the timesteps. This is the platonic ideal of if all pumping rates
      # started at timestep 1. In this case to get the cumulative depletion at step 1
      # we would just need to for each pumping rate evaluate f(1)*pump and sum them.
      
      # The matrices [starts_mat and stops_mat] represent for each pumping rate (column)
      # when they start and stop. For example column 1 starts has each row set to 0 (starts)
      # at time 0 in [start_mat]. These are less physical representations and more structures
      # that allow us to assemble a physical representation.
      
      # The same is true of [pumping_mat], each column is filled with its representative pumping rate
      # even if it is not active for that timestep
      
      # The matrix [starts_actual] assembles when each pumping rate actually starts,
      # and for how long it has been active. For example columns 1 and 2 may look like
      # 0 0
      # 1 0
      # 2 1
      # 3 2
      # ...
      # showing that at row 4 pumping rate 1 has been active for 3 timesteps, and pumping
      # rate 2 has been active for 2 timestep.
      
      # The matrix [stops_actual] assembles how much time we need to subtract from [starts_actual]
      # to get the impulse at that timestep only. For example columns 1 and 2 may look like
      # 0 0
      # 0 0
      # 1 0
      # 2 1
      # ...
      # so to get the depletion in timestep 4 for column 1 we can use [starts_actual and stops_actual] to evaluate
      # f(3) - f(2). Then for column 2 at timestep 4 we can evaluate f(2) - f(1). The sum of depletions at timestep
      # 4 will then be the sum of these evaluations.
      
      
      # FOR AN EXAMPLE RUN:
      # THIS WILL BE THE SAME AS A CONTINUOUS PUMPING RATE
      # timesteps = c(0,1,2,3,4)
      # start_pumping = c(0,1,2,3)
      # stop_pumping = c(1,2,3,4)
      # QW = c(10,10,10,10)
      #-------------------------------------------------------------------------------
      start_pumping <- c(0:(length(QW)-1))
      stop_pumping <- c(1:length(QW))
      timesteps <- c(0:length(QW)) 
      
      
      timestep_mat <- base::matrix(timesteps,
                                   nrow = length(timesteps),
                                   ncol = length(start_pumping))
      starts_mat <- base::matrix(start_pumping,
                                 nrow = length(timesteps),
                                 ncol = length(start_pumping),
                                 byrow = T)
      stops_mat <- base::matrix(stop_pumping,
                                nrow = length(timesteps),
                                ncol = length(stop_pumping),
                                byrow = T)
      pumping_mat <- base::matrix(QW,
                                  nrow = length(timesteps),
                                  ncol = length(QW),
                                  byrow = T)
      #-------------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------------
      # calculate time since each pumping interval starts/stops, bounded at 0
      starts_actual <- timestep_mat - starts_mat
      starts_actual[starts_actual < 0] <- 0
      
      stops_actual <- timestep_mat - stops_mat
      stops_actual[stops_actual < 0] <- 0
      #-------------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------------
      # vectorize for calculations
      starts_actual_vec <- c(starts_actual)
      stops_actual_vec <- c(stops_actual)
      pumping_vec <- c(pumping_mat)
      #-------------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------------
      # vector of zeroes that will be filled with the function evaluations
      depletions_vec <- rep(0, length(starts_actual_vec))
      #-------------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------------
      # evaulate f(t) - f(t-1)
      depletions_vec[starts_actual_vec > 0] <-
        pumping_vec[starts_actual_vec > 0] *
        (equation(elapsed_time = starts_actual_vec[starts_actual_vec > 0],
                  distance = distance,
                  stor_coef = stor_coef,
                  transmissivity = transmissivity) -
           equation(elapsed_time = stops_actual_vec[starts_actual_vec > 0],
                    distance = distance,
                    stor_coef = stor_coef,
                    transmissivity = transmissivity))
      #-------------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------------
      # return to matrix form for summation
      depletions_mat <- matrix(depletions_vec,
                               nrow = length(timesteps),
                               ncol = length(start_pumping))
      depletions_mat <- depletions_mat[-c(1), ]
      #-------------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------------
      # sum and return
      return(base::rowSums(depletions_mat))
      #-------------------------------------------------------------------------------
    }
    #-------------------------------------------------------------------------------
    
    
    
    
    
    
    
    
    
    
    
    #-------------------------------------------------------------------------------
    # for each reach calculate sum of all depletions
    depletions_per_reach <- list()
    for(i in 1:ncol(closest_points_per_segment)){
      #-------------------------------------------------------------------------------
      if(suppress_loading_bar == FALSE){
        #-------------------------------------------------------------------------------
        # user message
        loading_bar(iter = i,
                    total = ncol(closest_points_per_segment),
                    width = 50,
                    optional_text = '')
        #-------------------------------------------------------------------------------
      }
      #-------------------------------------------------------------------------------
      
      
      #-------------------------------------------------------------------------------
      # what are the closest points to each well
      points <- as.vector(unlist(closest_points_per_segment[, i]))
      fracs <- as.vector(unlist(reach_impact_frac[ , i]))
      well_indices <- c(1:length(points))
      rm <- which(is.na(points))
      #-------------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------------
      # if no wells are associated with the reach then pass
      if(all(is.na(points) == TRUE) == FALSE){
        #-------------------------------------------------------------------------------
        # remove any non-relevant wells from the for loop
        if(length(rm) > 0){
          well_indices <- well_indices[-c(rm)]
        } else {}
        #-------------------------------------------------------------------------------
        
        #-------------------------------------------------------------------------------
        depletions_per_well <- list()
        pump_frac_per_well <- list()
        sdf_end <- list()
        sdf_start <-
        depletions_end_time <- erfcinv(lagged_depletions_end_time)
        depletions_start_time <- erfcinv(lagged_depletions_start_time)
        counter <- 0
        for(j in well_indices){
          #-------------------------------------------------------------------------------
          # increment list counter
          counter <- counter + 1
          #-------------------------------------------------------------------------------
          
          #-------------------------------------------------------------------------------
          # get necessary information
          distance <- st_distance(wells[j, ],
                                  stream_points_geometry[points[j], ])
          distance <- as.numeric(distance)
          transmissivity <- as.numeric(st_drop_geometry(wells[j,transmissivity_key]))
          stor_coef <- as.numeric(st_drop_geometry(wells[j, stor_coef_key]))
          #-------------------------------------------------------------------------------
          
          #-------------------------------------------------------------------------------
          # calculate maximum stream depletion potential and
          # multiply by fraction of depletions of this well apportioned to this reach
          Q_out <- glover_stream_depletion_model(stor_coef = stor_coef,
                                                 transmissivity = transmissivity,
                                                 distance = distance,
                                                 QW = pumping[j, ])
          Q_final <- Q_out*fracs[j]
          #-------------------------------------------------------------------------------
          
          #-------------------------------------------------------------------------------
          depletions_per_well[[counter]] <- Q_final
          pump_frac_per_well[[counter]] <- pumping[j, ] * fracs[j]
          # sdf taken from barlow and leake 2012 citing Jenkins 1968
          # https://pubs.usgs.gov/circ/1376/
          # gives time of maximum impact
          # sdf[[counter]] <- ((distance*distance)*stor_coef)/transmissivity
          
          # specific to glover
          sdf_end[[counter]] <- ((distance*distance)*stor_coef)/(4*transmissivity*depletions_end_time)
          sdf_start[[counter]] <- ((distance*distance)*stor_coef)/(4*transmissivity*depletions_start_time)
          #-------------------------------------------------------------------------------
        }
        #-------------------------------------------------------------------------------
        
        #-------------------------------------------------------------------------------
        # fractional depletion method taken from Zipper 2019
        # https://doi.org/10.1029/2018WR024403 eq 1
        # sdf taken from barlow and leake 2012 citing Jenkins 1968
        # https://pubs.usgs.gov/circ/1376/
        # qs(t)  = Qw * erfc(z)
        # Qs (90 % depletions) = wanted quantity (time to 90% depletions)
        # Qs (90%) = Qw * erfc(z99)
        # erfc(z99) = 0.99
        # z99 = erfcinv(0.99) = 0.0086
        # z99 = sqrt(Sd^2/4Tt99) [Zipper 2019; Glover and Balmer 1954]
        # z99 = d/2(sqrt(Tt99/S))
        # t99 = (d/2*z99)^2 * S/T
        # t99 = d^2*S/4Tz99^2
        
        # evaluate erfcinv at 0.99 (when 99% has occured) to get function value
        # evaluate at what time this must have occurred by rearranging equation
        depletions_total <- do.call(cbind, depletions_per_well)
        pump_frac_total <- do.call(cbind, pump_frac_per_well)
        # take median so if we have a far away well that is close to no other stream
        # it doesnt bias sdf_avg
        sdf_end_avg <- round(median(unlist(sdf_end), na.rm = T), 0)
        sdf_start <- unlist(sdf_start)
        sdf_start <- sort(sdf_start[is.na(sdf_start) == FALSE])
        start_weights <- rev(c(1:length(sdf_start)))
        
        # take weighted average of sdf starts for conservative net of which pumping
        # is currently contributing to depletions
        sdf_start_avg <- round(weighted.mean(sdf_start,start_weights),0)
        #-------------------------------------------------------------------------------
        
        #-------------------------------------------------------------------------------
        if(str_to_title(stream_depletion_output) == 'Fractional'){
          
          #-------------------------------------------------------------------------------
          # get the average pumping occuring over the time period that pumping is having the 
          # maximum impact on streams mean(pump[t-sdf_avg : t])
          sum_pump_frac <- base::rowSums(pump_frac_total)
          
          sum_pump_frac_lagged <- sapply(seq_along(sum_pump_frac), function(i) {
            start <- max(1, i - sdf_end_avg)
            
            if(i <= sdf_start_avg){
              end <- i
            } else if (i > sdf_start_avg &
                       i < sdf_start_avg*2){
              end <- sdf_start_avg
            } else {
              end <- i - sdf_start_avg
            }
            
            mean(sum_pump_frac[start:end]) # sum on rolling window based on sdf
            # lets say i = 30, we might sum between pump_frac[12:27] or something like that
            # accounts for the fact that pumping at time i will not impact stream for a number of time
            # steps and that it will continue to effect it for a number of time steps
          })
          depletions_per_reach[[i]] <- base::rowSums(depletions_total)/sum_pump_frac_lagged
          #-------------------------------------------------------------------------------

        } else {
          depletions_per_reach[[i]] <- base::rowSums(depletions_total)
        }
        #-------------------------------------------------------------------------------
      } else{
        depletions_per_reach[[i]] <- rep(0, ncol(pumping)) # reach has no depletions
      }
      #-------------------------------------------------------------------------------
    }
    #-------------------------------------------------------------------------------
    
    
    
    
    #-------------------------------------------------------------------------------
    # stats
    start_of_depletions <- lapply(depletions_per_reach, function(x){
      rle(x)$lengths[1]
    })
    start_of_depletions <- unlist(start_of_depletions)
    rm <- which(start_of_depletions == ncol(pumping))
    if(length(rm) > 0){
      start_of_depletions[-c(rm)] # if never started remove
    } else {}
    
    
    
    mean_start_of_depletions <- mean(start_of_depletions, na.rm = TRUE)
    median_start_of_depletions <- median(start_of_depletions, na.rm = TRUE)
    
    final_depletions <- lapply(depletions_per_reach, function(x){
      tail(x, 1)
    })
    n_timesteps <- ncol(pumping)
    mean_final_depletions <- mean(unlist(final_depletions), na.rm = TRUE)
    median_final_depletions <- median(unlist(final_depletions), na.rm = TRUE)
    which_max_final_depletions <- which.max(unlist(final_depletions))
    max_final_depletions <- max(unlist(final_depletions), na.rm = TRUE)
    #-------------------------------------------------------------------------------

    
    
    #-------------------------------------------------------------------------------
    # write status to log
    writeLines(text = sprintf('%s %s',
                              'Mean | Median start of stream depletions (timestep): ',
                              paste(round(mean_start_of_depletions,2),
                                    ' | ',
                                    median_start_of_depletions)),
               con = log_file)
    
    
    writeLines(text = sprintf('%s %s',
                              paste('Mean | Median final depletions','(',units,'^3','):'),
                              paste(round(mean_final_depletions,4),
                                    '|',
                                    round(median_final_depletions,4),
                                    'at timestep (t_final)',
                                    n_timesteps)),
               con = log_file)
    
    writeLines(text = sprintf('%s %s',
                              paste('Max final depletions','(',units,'^3','):'),
                              paste(round(max_final_depletions,4),
                                    'at timestep (t_final)',
                                    n_timesteps,
                                    'for reach',
                                    which_max_final_depletions)),
               con = log_file)
    #-------------------------------------------------------------------------------
    
    
    
    #-------------------------------------------------------------------------------
    # output
    depletions_per_reach <- do.call(rbind, depletions_per_reach)
    return(depletions_per_reach)
    #-------------------------------------------------------------------------------
  }
  #-------------------------------------------------------------------------------
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ############################################################################################
  ######################################### MAIN FUNCTIONS ###################################
  ############################################################################################
  
  #===========================================================================================
  # Takes desired proximity criteria and decides which stream segments are
  # affected by each well
  #===========================================================================================
  find_impacted_stream_segments <- function(streams,
                                            wells,
                                            subwatersheds,
                                            proximity_criteria)
  {
    #-------------------------------------------------------------------------------
    # write status to log
    writeLines(text = sprintf('%s %s',
                              '####',
                              'Calculating Impacted Segments For Each Well'),
               con = log_file)
    
    writeLines(text = sprintf('%s %s',
                              'Well segment proximity criteria: ',
                               str_to_title(proximity_criteria)),
               con = log_file)
    #-------------------------------------------------------------------------------
    
    
    
    
    #-------------------------------------------------------------------------------
    # if streams do not already come pre-processed as points of interest,
    # process them
    if(streams_are_points == FALSE){
      #-------------------------------------------------------------------------------
      # write status to log
      writeLines(text = sprintf('%s',
                                'Streams not preprocessed to points ... converting to points'),
                 con = log_file)
      #-------------------------------------------------------------------------------
      

      #-------------------------------------------------------------------------------
      # getting points from stream linestrings
      stream_points_list <- list()
      average_length <- list()
      id_list <- list()
      for(i in 1:nrow(streams)){
        coords <- Extract_SF_Linestring_Vertices(streams$geometry[i])
        stream_points_list[[i]] <- cbind(coords[[2]],
                                         coords[[1]])

        id_list[[i]] <- rep(st_drop_geometry(streams[i,stream_id_key]),
                            length(coords[[1]]))
        
        average_length[[i]] <- length(coords[[1]])
      }
      #-------------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------------
      # turning into data frame for later export and diag
      stream_points_df <- do.call(rbind, stream_points_list)
      stream_points_df <- as.data.frame(stream_points_df)
      colnames(stream_points_df) <- c('x','y')
      #-------------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------------
      # turning into geometry object for calculation of distances and intersections
      stream_points_geometry <- st_as_sf(stream_points_df,
                                         coords = c('x','y'),
                                         na.fail = FALSE,
                                         crs = crs(streams))
      cnams <- colnames(stream_points_geometry)
      stream_points_geometry <- cbind(unlist(id_list), stream_points_geometry)
      colnames(stream_points_geometry) <- c(stream_id_key, cnams)
      #-------------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------------
      # save memory
      rm(stream_points_list)
      rm(id_list)
      #-------------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------------
      # write status to log
      average_length <- do.call(rbind, average_length)
      writeLines(text = sprintf('%s %s',
                                'Number of unique stream segments: ',
                                nrow(streams)),
                 con = log_file)
      writeLines(text = sprintf('%s %s',
                                'Mean | Median number of points per segment: ',
                                paste(round(mean(average_length, na.rm = TRUE),0),
                                      '|',
                                      round(median(average_length, na.rm = TRUE),0))),
                 con = log_file)
      rm(average_length) # save memory
      #-------------------------------------------------------------------------------
    } else {
      stream_points_geometry <- streams
      #-------------------------------------------------------------------------------
      # write status to log file
      writeLines(text = sprintf('%s',
                                'Streams came already preprocessed to points ... passing'),
                 con = log_file)
      #-------------------------------------------------------------------------------
    }
    #-------------------------------------------------------------------------------
    
    
    
    #-------------------------------------------------------------------------------
    # calculate which wells impact which stream points
    if(str_to_title(proximity_criteria) == 'Adjacent'){
      
      #-------------------------------------------------------------------------------
      # ensure everything is in the same projection
      proj_output <- ensure_projections(wells = wells,
                                        geometry_list = list(subwatersheds,
                                                             stream_points_geometry))
      subwatersheds <- proj_output[[1]]
      stream_points_geometry <- proj_output[[2]]
      rm(proj_output)
      #-------------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------------
      # find adjacent stream points
      writeout <- find_adjacent_stream_points(wells = wells,
                                              subwatersheds = subwatersheds,
                                              stream_points_geometry = stream_points_geometry)
      #-------------------------------------------------------------------------------
    }
    #-------------------------------------------------------------------------------
    
    
    #-------------------------------------------------------------------------------
    # calculate which wells impact which stream points
    if(str_to_title(proximity_criteria) == 'Adjacent+Expanding'){
      
      #-------------------------------------------------------------------------------
      # ensure everything is in the same projection
      proj_output <- ensure_projections(wells = wells,
                                        geometry_list = list(subwatersheds,
                                                             stream_points_geometry))
      subwatersheds <- proj_output[[1]]
      stream_points_geometry <- proj_output[[2]]
      rm(proj_output)
      #-------------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------------
      # find adjacent stream points
      writeout <- find_adjacent_and_expanding_stream_points(wells = wells,
                                                            subwatersheds = subwatersheds,
                                                            influence_radius = influence_radius,
                                                            stream_points_geometry = stream_points_geometry)
      #-------------------------------------------------------------------------------
    }
    #-------------------------------------------------------------------------------
    
    
    
    #-------------------------------------------------------------------------------
    # calculate which wells impact which stream points
    if(str_to_title(proximity_criteria) == 'Local Area' |
       str_to_title(proximity_criteria) == 'Expanding'){
      
      #-------------------------------------------------------------------------------
      # ensure everything is in the same projection
      proj_output <- ensure_projections(wells = wells,
                                        geometry_list = list(stream_points_geometry))
      stream_points_geometry <- proj_output[[1]]
      rm(proj_output)
      #-------------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------------
      # find adjacent stream points
      writeout <- find_local_stream_points(wells = wells,
                                           influence_radius = influence_radius,
                                           stream_points_geometry = stream_points_geometry)
      #-------------------------------------------------------------------------------
    }
    #-------------------------------------------------------------------------------
    
    
    #-------------------------------------------------------------------------------
    # calculate which wells impact which stream points
    if(str_to_title(proximity_criteria) == 'Whole Domain'){
      #-------------------------------------------------------------------------------
      # ensure everything is in the same projection
      proj_output <- ensure_projections(wells = wells,
                                        geometry_list = list(stream_points_geometry))
      stream_points_geometry <- proj_output[[1]]
      rm(proj_output)
      #-------------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------------
      # associate all stream points with all wells
      writeout <- find_whole_domain_points(wells = wells,
                                           stream_points_geometry = stream_points_geometry)
      #-------------------------------------------------------------------------------
    }
    #-------------------------------------------------------------------------------
    
    
    #-------------------------------------------------------------------------------
    # log message
    m1 <- mean(as.numeric(writeout[[2]]$ImpLMet), na.rm = T)
    m2 <- median(as.numeric(writeout[[2]]$ImpLMet), na.rm = T)
    writeLines(text = sprintf('%s %s',
                              paste0('Mean | Median impacted segment length in ',units,': '),
                              paste0(as.character(round(m1,0)),
                                     '|',
                                     as.character(round(m2,0)))),
               con = log_file)
    writeLines(text = sprintf('%s',
                              'Found impacted segments without error'),
               con = log_file)
    writeLines(text = sprintf('%s',
                              'Moving to next step ...'),
               con = log_file)
    #-------------------------------------------------------------------------------
    
    #-------------------------------------------------------------------------------
    # return to higher level
    stream_points_geometry$Index <- c(1:nrow(stream_points_geometry))
    return(list(writeout[[1]],
                writeout[[2]],
                stream_points_geometry))
    #-------------------------------------------------------------------------------
  }
  #-------------------------------------------------------------------------------
  
  
  
  
  #===========================================================================================
  # Apportions depletions based on given criteria
  #===========================================================================================
  calculate_depletion_apportionments <- function(wells,
                                                 apportionment_criteria,
                                                 stream_points_geometry,
                                                 stream_id_key){
    #-------------------------------------------------------------------------------
    # write status to log
    writeLines(text = sprintf('%s %s',
                              '####',
                              'Apportioning Depletions For Each Well'),
               con = log_file)
    
    writeLines(text = sprintf('%s %s',
                              'Well apportionment criteria: ',
                              str_to_title(apportionment_criteria)),
               con = log_file)
    #-------------------------------------------------------------------------------
    
    
    
    #-------------------------------------------------------------------------------
    # find what points are important for each well
    if(str_to_title(apportionment_criteria) %in% c('Inverse Distance',
                                                   'Inverse Distance Squared',
                                                   'Thiessen Polygon',
                                                   'Web',
                                                   'Web Squared')){
      
      w_index <- as.vector(unlist(st_drop_geometry(wells[ ,wells_id_key])))
      closest_points_per_segment <- find_closest_points_per_segment(wells = wells,
                                                                    stream_points_geometry = stream_points_geometry,
                                                                    stream_id_key = stream_id_key)
      closest_points_per_segment <- cbind(w_index,
                                          closest_points_per_segment)
      closest_points_per_segment <- as.data.frame(closest_points_per_segment)
      colnames(closest_points_per_segment) <- c('wellN',
                                                paste0('PN',
                                                       1:(ncol(closest_points_per_segment)-1)))
      
    } else {
      closest_points_per_segment <- NULL # otherwise if web method used all points are important
    }
    #-------------------------------------------------------------------------------
    
    
    #-------------------------------------------------------------------------------
    # Inverse distance apportionment
    if(str_to_title(apportionment_criteria) %in% c('Inverse Distance',
                                                   'Inverse Distance Squared')){
      if(str_to_title(apportionment_criteria) == 'Inverse Distance Squared'){
        power <- 2
      } else {
        power <- 1
      }

      output <- inverse_distance_apportionment(power = power,
                                               wells = wells,
                                               closest_points_per_segment = closest_points_per_segment,
                                               stream_points_geometry = stream_points_geometry)

    }
    #-------------------------------------------------------------------------------


    #-------------------------------------------------------------------------------
    # Inverse distance apportionment
    if(str_to_title(apportionment_criteria) %in% c('Thiessen Polygon')){

      output <- thiessen_polygon_apportionment(wells = wells,
                                               closest_points_per_segment = closest_points_per_segment,
                                               stream_points_geometry = stream_points_geometry)

    }
    #-------------------------------------------------------------------------------



    #-------------------------------------------------------------------------------
    # Inverse distance apportionment
    if(str_to_title(apportionment_criteria) %in% c('Web',
                                                   'Web Squared')){
      if(str_to_title(apportionment_criteria) == 'Web Squared'){
        power <- 2
      } else {
        power <- 1
      }

      output <- web_apportionment(power = power,
                                  wells = wells,
                                  closest_points_per_segment = closest_points_per_segment,
                                  stream_points_geometry = stream_points_geometry)

    }
    #-------------------------------------------------------------------------------
    
    
    #-------------------------------------------------------------------------------
    # log message
    writeLines(text = sprintf('%s',
                              'Apportioned depletions without error'),
               con = log_file)
    writeLines(text = sprintf('%s',
                              'Moving to next step ...'),
               con = log_file)
    #-------------------------------------------------------------------------------
    
    return(list(output[[1]],
                output[[2]],
                closest_points_per_segment))
  }
  #-------------------------------------------------------------------------------
  
  
  
  
  
  
  #===========================================================================================
  # Apportions depletions based on given criteria
  #===========================================================================================
  calculate_stream_depletion_per_reach <- function(closest_points_per_segment = closest_points_per_segment,
                                                   stream_points_geometry = stream_points_geometry,
                                                   wells = wells,
                                                   transmissivity_key = transmissivity_key,
                                                   stor_coef_key = stor_coef_key,
                                                   analytical_model = analytical_model,
                                                   stream_depletion_output = stream_depletion_output){
    #-------------------------------------------------------------------------------
    # write status to log
    writeLines(text = sprintf('%s %s',
                              '####',
                              'Calculating Depletions For Each Well'),
               con = log_file)
    
    writeLines(text = sprintf('%s %s',
                              'Using analytical model: ',
                              str_to_title(analytical_model)),
               con = log_file)
    
    writeLines(text = sprintf('%s %s',
                              'And output format: ',
                              str_to_title(stream_depletion_output)),
               con = log_file)
    #-------------------------------------------------------------------------------
    
    
    
    #-------------------------------------------------------------------------------
    # find what points are important for each well
    if(str_to_title(analytical_model) %in% c('Glover')){
      output <- glover_stream_depletion_calculations(closest_points_per_segment = closest_points_per_segment,
                                                     reach_impact_frac = reach_impact_frac,
                                                     stream_points_geometry = stream_points_geometry,
                                                     wells = wells,
                                                     transmissivity_key = transmissivity_key,
                                                     stor_coef_key = stor_coef_key,
                                                     stream_depletion_output = stream_depletion_output)
      output <- cbind(as.vector(unlist(st_drop_geometry(streams[,stream_id_key]))),
                      output)
      output <- as.data.frame(output)
      colnames(output) <- c('RN', paste0('T',1:(ncol(output)-1)))

    } else {}
    #-------------------------------------------------------------------------------
    
    
    
    #-------------------------------------------------------------------------------
    # log message
    writeLines(text = sprintf('%s',
                              'Calculated depletions without error'),
               con = log_file)
    writeLines(text = sprintf('%s',
                              'Finished ...'),
               con = log_file)
    #-------------------------------------------------------------------------------
    
    return(output)
  }
  #-------------------------------------------------------------------------------
  
  
  


  
  
  
  
  
  
  ############################################################################################
  ######################################### RUN FUNCTIONS ####################################
  ############################################################################################
  required_packages <- c('sf','sp','raster','terra','lubridate','stringr','stats')
  for(i in 1:length(required_packages)){
    require_package(required_packages[i])
  }

  units <- units(st_distance(wells[1, ], wells[1, ]))$numerator
  if(units == 'm'){
    units <- 'meters'
  } else if (units == '°'){
    units <- 'degrees'
  } else if (units == 'US_survey_foot'){
    units <- 'feet'
  }
  
  #-------------------------------------------------------------------------------
  # open log file to write program execution
  log_file <- file(file.path(diag_out_dir,'log.txt'), 'w')
  writeLines(text = sprintf('%s %s',
                            '####',
                            'Opening program to calculate stream depletion'),
             con = log_file)
  writeLines(text = '',
             con = log_file)
  #-------------------------------------------------------------------------------
  
  
  
  ############################################################################################
  # errors
  #-------------------------------------------------------------------------------
  # before proceeding to well calculations do I have the information I need?
  if(str_to_title(proximity_criteria) %in% c('Adjacent','Adjacent+Expanding') &
     is.null(subwatersheds) == TRUE){
    #-------------------------------------------------------------------------------
    writeLines(text = sprintf('%s',
                              'Proximity criteria required subwatersheds but none supplied'),
               con = log_file)
    writeLines(text = sprintf('%s',
                              'Exiting program ...'),
               con = log_file)
    close(log_file)
    #-------------------------------------------------------------------------------
    
    
    #-------------------------------------------------------------------------------
    stop(paste0('\ncalculate_stream_depletions.R encountered Error:    \n',
                'proximity criteria required subwatersheds (Adjacent | Adjacent+Expanding)\n',
                'but none supplied\n',
                'exiting program ...'))
    #-------------------------------------------------------------------------------
  } else if (str_to_title(proximity_criteria) %in% c('Local Area',
                                                     'Expanding',
                                                     'Adjacent+Expanding') &
             is.null(influence_radius) == TRUE){
    #-------------------------------------------------------------------------------
    writeLines(text = sprintf('%s',
                              'Proximity criteria required influence radius but none supplied'),
               con = log_file)
    writeLines(text = sprintf('%s',
                              'Exiting program ...'),
               con = log_file)
    close(log_file)
    #-------------------------------------------------------------------------------
    
    
    #-------------------------------------------------------------------------------
    stop(paste0('\ncalculate_stream_depletions.R encountered Error:    \n',
                'proximity criteria required influence radius (Local Area | Expanding | Adjacent+Expanding)\n',
                'but none supplied\n',
                'exiting program ...'))
    #-------------------------------------------------------------------------------
  } else {}
  
  if(streams_are_points == TRUE &
     is.null(stream_id_key) == TRUE){
    #-------------------------------------------------------------------------------
    writeLines(text = sprintf('%s',
                              'Identifying column for streams required to calculate impacted length but none supplied'),
               con = log_file)
    writeLines(text = sprintf('%s',
                              'Exiting program ...'),
               con = log_file)
    close(log_file)
    #-------------------------------------------------------------------------------
    
    
    #-------------------------------------------------------------------------------
    stop(paste0('\ncalculate_stream_depletions.R encountered Error:    \n',
                'Identifying column for streams required to calculate impacted length',
                'but none supplied\n',
                'exiting program ...'))
    #-------------------------------------------------------------------------------
  }
  
  if(str_to_title(analytical_model) == 'Glover' &
     (is.null(stor_coef_key) == TRUE |
     !stor_coef_key %in% colnames(wells))){
    #-------------------------------------------------------------------------------
    writeLines(text = sprintf('%s',
                              paste0('Identifying column for storage coefficient in ',
                                     analycial_model,
                                     ' model required but not present in well set')),
               con = log_file)
    writeLines(text = sprintf('%s',
                              'Exiting program ...'),
               con = log_file)
    close(log_file)
    #-------------------------------------------------------------------------------
    
    
    #-------------------------------------------------------------------------------
    stop(paste0('\ncalculate_stream_depletions.R encountered Error:    \n',
                'Identifying column for storage coefficient in ',
                analytical_model,
                ' model required but not present in well set\n',
                'exiting program ...'))
    #-------------------------------------------------------------------------------
  }
  
  
  
  if(str_to_title(analytical_model) == 'Glover' &
     (is.null(transmissivity_key) == TRUE |
     !transmissivity_key %in% colnames(wells))){
    #-------------------------------------------------------------------------------
    writeLines(text = sprintf('%s',
                              paste0('Identifying column for transmissivity in ',
                                     analycial_model,
                                     ' model required but not present in well set')),
               con = log_file)
    writeLines(text = sprintf('%s',
                              'Exiting program ...'),
               con = log_file)
    close(log_file)
    #-------------------------------------------------------------------------------
    
    
    #-------------------------------------------------------------------------------
    stop(paste0('\ncalculate_stream_depletions.R encountered Error:    \n',
                'Identifying column for transmissivity in ',
                analytical_model,
                ' model required but not present in well set\n',
                'exiting program ...'))
    #-------------------------------------------------------------------------------
  }
  
  
  
  
  if(str_to_title(apportionment_criteria) == 'Thiessen Polygon' &
     nrow(streams) == 1){
    #-------------------------------------------------------------------------------
    writeLines(text = sprintf('%s',
                              paste0('Apportionment criteria of ',
                                     apportionment_criteria,
                                     ' selected but only one stream present')),
               con = log_file)
    writeLines(text = sprintf('%s',
                              paste0(apportionment_criteria,'s',
                                     ' cannot be made out of only one entity')),
               con = log_file)
    writeLines(text = sprintf('%s',
                              'Exiting program ...'),
               con = log_file)
    close(log_file)
    #-------------------------------------------------------------------------------
    
    
    #-------------------------------------------------------------------------------
    stop(paste0('\ncalculate_stream_depletions.R encountered Error:    \n',
                'Apportionment criteria of ',
                apportionment_criteria,
                ' selected but only one stream present\n',
                apportionment_criteria,'s',
                ' cannot be made out of only one entity\n',
                'please select another apportionment method\n',
                'exiting program ...'))
    #-------------------------------------------------------------------------------
  }
  #-------------------------------------------------------------------------------
  
  
  
  
  
  
  
  
  
  
  ############################################################################################
  # run stream impacted segments
  
  #-------------------------------------------------------------------------------
  # capture any error output and write to log file
  tryCatch(expr = {
    #-------------------------------------------------------------------------------
    # user message
    if(suppress_console_messages == FALSE){
      cat('Finding which stream reaches are impacted by wells: Step (1/3)')
    }
    #-------------------------------------------------------------------------------
    
    #-------------------------------------------------------------------------------
    # find impacted points by proximity criteria
    if(is.null(wells_id_key) == TRUE){
      wells_id_key <- 'ID'
      wells$ID <- c(1:nrow(wells))
    } else {}
    
    if(is.null(stream_id_key) == TRUE){
      stream_id_key <- 'ID'
      streams$ID <- c(1:nrow(streams))
    } else {}
    
    output <- find_impacted_stream_segments(streams,
                                            wells,
                                            subwatersheds,
                                            proximity_criteria)
    impacted_points <- output[[1]]
    wells <- output[[2]]
    stream_points_geometry <- output[[3]]
    #-------------------------------------------------------------------------------

    #-------------------------------------------------------------------------------
    # writeout
    write.csv(impacted_points,
              file.path(data_out_dir,
                        'impacted_stream_points_by_well.csv'),
              row.names = FALSE)

    st_write(wells,
              file.path(data_out_dir,
                        'wells_with_impacted_length.shp'),
              append = FALSE,
              quiet = TRUE)

    st_write(stream_points_geometry,
             file.path(data_out_dir,
                       'stream_points.shp'),
             append = FALSE,
             quiet = TRUE)
    #-------------------------------------------------------------------------------
    
    #-------------------------------------------------------------------------------
    # save space
    rm(output)
    writeLines(text = sprintf('%s',
                              ''),
               con = log_file)
    #-------------------------------------------------------------------------------
  }, error = function(e){
    #-------------------------------------------------------------------------------
    # write error to log file
    status <- 'find_impacted_stream_segments'
    writeLines(text = sprintf('%s %s',
                              'ENCOUNTERED ERROR: ',
                              class(e)[1]),
               con = log_file)
    writeLines(text = sprintf('%s %s',
                              'ON COMMAND: ',
                              paste0(capture.output(e$call),collapse = ' ')),
               con = log_file)
    writeLines(text = sprintf('%s %s',
                              'FOR REASON: ',
                              paste0(e$message, collapse = ' ')),
               con = log_file)
    writeLines(text = sprintf('%s',
                              'exiting program...'),
               con = log_file)
    close(log_file)
    #-------------------------------------------------------------------------------
    
    
    #-------------------------------------------------------------------------------
    # write error to console
    stop(paste0('\ncalculate_stream_depletions.R encountered Error:    ', class(e)[1],'\n',
                'during:    ', status,'\n',
                'on command:    ', paste0(capture.output(e$call),collapse = ' '),'\n',
                'for reason:    ', paste0(e$message, collapse = ' '),'\n',
                'for more information see the log.txt file output\n',
                'exiting program ...'))
    #-------------------------------------------------------------------------------
  })
  #-------------------------------------------------------------------------------
  
  
  
  
  
  
  ############################################################################################
  # run depletion apportionments
  
  #-------------------------------------------------------------------------------
  # capture any error output and write to log file
  tryCatch(expr = {
    #-------------------------------------------------------------------------------
    # user message
    if(suppress_console_messages == FALSE){
      cat('\nApportioning depletions per well: Step (2/3)')
    }
    #-------------------------------------------------------------------------------
    
    #-------------------------------------------------------------------------------
    # find impacted points by proximity criteria
    output <- calculate_depletion_apportionments(stream_points_geometry = stream_points_geometry,
                                                 stream_id_key = stream_id_key,
                                                 wells = wells,
                                                 apportionment_criteria = apportionment_criteria)
    reach_impact_frac <- output[[1]]
    impacted_reaches <- output[[2]]
    closest_points_per_segment <- output[[3]]
    #-------------------------------------------------------------------------------
    
    #-------------------------------------------------------------------------------
    # writeout
    write.csv(reach_impact_frac,
              file.path(data_out_dir,
                        'reach_depletion_fraction_by_well.csv'),
              row.names = FALSE)

    write.csv(impacted_reaches,
              file.path(data_out_dir,
                        'impacted_reaches_by_well.csv'),
              row.names = FALSE)
    
    if(str_to_title(apportionment_criteria) %in% c('Inverse Distance',
                                                   'Inverse Distance Squared',
                                                   'Thiessen Polygon',
                                                   'Web',
                                                   'Web Squared')){
      write.csv(closest_points_per_segment,
                file.path(data_out_dir,
                          'closest_points_per_reach_per_well.csv'),
                row.names = FALSE)
    }
    #-------------------------------------------------------------------------------

    #-------------------------------------------------------------------------------
    # subtracting a column from a two column data frame turns it into a vector
    # when this behavior is not wanted, this is a workaround for this special case
    # of only one well being present
    if(ncol(reach_impact_frac) < 3){
      cnams <- colnames(reach_impact_frac)
      m <- matrix(data = as.vector(unlist(reach_impact_frac[,-c(1)])),
                  nrow = nrow(reach_impact_frac),
                  ncol = ncol(reach_impact_frac) - 1,
                  byrow = TRUE)
      reach_impact_frac <- as.data.frame(m)
      colnames(reach_impact_frac) <- cnams[-c(1)]
      
      
      cnams <- colnames(impacted_reaches)
      m <- matrix(data = as.vector(unlist(impacted_reaches[,-c(1)])),
                  nrow = nrow(impacted_reaches),
                  ncol = ncol(impacted_reaches) - 1,
                  byrow = TRUE)
      impacted_reaches <- as.data.frame(m)
      colnames(impacted_reaches) <- cnams[-c(1)]
      
      
      cnams <- colnames(closest_points_per_segment)
      m <- matrix(data = as.vector(unlist(closest_points_per_segment[,-c(1)])),
                  nrow = nrow(closest_points_per_segment),
                  ncol = ncol(closest_points_per_segment) - 1,
                  byrow = TRUE)
      closest_points_per_segment <- as.data.frame(m)
      colnames(closest_points_per_segment) <- cnams[-c(1)]
    } else {
      reach_impact_frac <- reach_impact_frac[,-c(1)]
      impacted_reaches <- impacted_reaches[,-c(1)]
      closest_points_per_segment <- closest_points_per_segment[,-c(1)]
    }
    #-------------------------------------------------------------------------------
    

    #-------------------------------------------------------------------------------
    # save space
    rm(output)
    #-------------------------------------------------------------------------------
  }, error = function(e){
    #-------------------------------------------------------------------------------
    # write error to log file
    status <- 'calculate_depletion_apportionments'
    writeLines(text = sprintf('%s %s',
                              'ENCOUNTERED ERROR: ',
                              class(e)[1]),
               con = log_file)
    writeLines(text = sprintf('%s %s',
                              'ON COMMAND: ',
                              paste0(capture.output(e$call),collapse = ' ')),
               con = log_file)
    writeLines(text = sprintf('%s %s',
                              'FOR REASON: ',
                              paste0(e$message, collapse = ' ')),
               con = log_file)
    writeLines(text = sprintf('%s',
                              'exiting program...'),
               con = log_file)
    close(log_file)
    #-------------------------------------------------------------------------------
    
    
    #-------------------------------------------------------------------------------
    # write error to console
    stop(paste0('\ncalculate_stream_depletions.R encountered Error:    ', class(e)[1],'\n',
                'during:    ', status,'\n',
                'on command:    ', paste0(capture.output(e$call),collapse = ' '),'\n',
                'for reason:    ', paste0(e$message, collapse = ' '),'\n',
                'for more information see the log.txt file output\n',
                'exiting program ...'))
    #-------------------------------------------------------------------------------
  })
  #-------------------------------------------------------------------------------
  
  
  

  
  
  
  
  
  ############################################################################################
  # run calculate depletions
  
  #-------------------------------------------------------------------------------
  # capture any error output and write to log file
  tryCatch(expr = {
    #-------------------------------------------------------------------------------
    # user message
    if(suppress_console_messages == FALSE){
      cat('\nCalculating depletions per well: Step (3/3)')
    }
    #-------------------------------------------------------------------------------
    
    #-------------------------------------------------------------------------------
    # calculate streamflow depletions based on either
    # a volumetric or fractional approach
    # fractional gives number between 0 and 1, where 1 is depletion is equal to
    # the pumping rate
    output <- calculate_stream_depletion_per_reach(closest_points_per_segment = closest_points_per_segment,
                                                   stream_points_geometry = stream_points_geometry,
                                                   wells = wells,
                                                   transmissivity_key = transmissivity_key,
                                                   stor_coef_key = stor_coef_key,
                                                   analytical_model = analytical_model,
                                                   stream_depletion_output = stream_depletion_output)
    depletions_by_reach <- output
    #-------------------------------------------------------------------------------

    
    #-------------------------------------------------------------------------------
    write.csv(depletions_by_reach,
              file.path(data_out_dir,
                        paste0(stream_depletion_output,'_depletions_by_reach.csv')),
              row.names = FALSE)
    #-------------------------------------------------------------------------------

    
    #-------------------------------------------------------------------------------
    # save space
    rm(output)
    #-------------------------------------------------------------------------------
  }, error = function(e){
    #-------------------------------------------------------------------------------
    # write error to log file
    status <- 'calculate_depletions'
    writeLines(text = sprintf('%s %s',
                              'ENCOUNTERED ERROR: ',
                              class(e)[1]),
               con = log_file)
    writeLines(text = sprintf('%s %s',
                              'ON COMMAND: ',
                              paste0(capture.output(e$call),collapse = ' ')),
               con = log_file)
    writeLines(text = sprintf('%s %s',
                              'FOR REASON: ',
                              paste0(e$message, collapse = ' ')),
               con = log_file)
    writeLines(text = sprintf('%s',
                              'exiting program...'),
               con = log_file)
    close(log_file)
    #-------------------------------------------------------------------------------
    
    
    #-------------------------------------------------------------------------------
    # write error to console
    stop(paste0('\ncalculate_stream_depletions.R encountered Error:    ', class(e)[1],'\n',
                'during:    ', status,'\n',
                'on command:    ', paste0(capture.output(e$call),collapse = ' '),'\n',
                'for reason:    ', paste0(e$message, collapse = ' '),'\n',
                'for more information see the log.txt file output\n',
                'exiting program ...'))
    #-------------------------------------------------------------------------------
  })
  #-------------------------------------------------------------------------------
  
  
  
  #-------------------------------------------------------------------------------
  # close log file
  close(log_file)
  #-------------------------------------------------------------------------------
}
#-------------------------------------------------------------------------------