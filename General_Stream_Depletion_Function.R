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
# 11/5/2025
calculate_stream_depletions <- function(streams,
                                        streams_are_points = FALSE,
                                        stream_id_key = NULL,
                                        wells,
                                        subwatersheds = NULL,
                                        influence_radius = NULL,
                                        proximity_criteria = 'adjacent',
                                        data_out_dir = getwd(),
                                        diag_out_dir = getwd(),
                                        suppress_loading_bar = TRUE,
                                        suppress_console_messages = TRUE)
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
      well_intersect_indices <- c(1:length(well_intersect_indices))[-c(rm_empty_intersections)]
      #-------------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------------
      # if for some reason outside domain then place NA
      # else find all stream points within that subwatershed
      if(length(well_intersect_indices) == 0){
        impacted_points[[i]] <- as.character(NA)
        impacted_length[[i]] <- as.character(NA)
      } else {
        strm_intersect_indices <- st_intersects(stream_points_geometry,
                                                st_geometry(subwatersheds[well_intersect_indices, ]))
        rm_empty_intersections <- which(lengths(strm_intersect_indices) == 0)
        strm_intersect_indices <- c(1:length(strm_intersect_indices))[-c(rm_empty_intersections)]
        
        
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
    w_index <- c(1:nrow(wells))
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
      well_intersect_indices <- c(1:length(well_intersect_indices))[-c(rm_empty_intersections)]
      #-------------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------------
      # if for some reason outside domain then place NA
      # else find all stream points within that subwatershed
      if(length(well_intersect_indices) == 0){
        impacted_points[[i]] <- as.character(NA)
        impacted_length[[i]] <- as.character(NA)
      } else {
        strm_intersect_indices <- st_intersects(stream_points_geometry,
                                                st_geometry(subwatersheds[well_intersect_indices, ]))
        rm_empty_intersections <- which(lengths(strm_intersect_indices) == 0)
        strm_intersect_indices <- c(1:length(strm_intersect_indices))[-c(rm_empty_intersections)]
        
        expanding_indices <- st_intersects(stream_points_geometry,
                                           st_buffer(wells[i, ], influence_radius))
        rm_empty_intersections <- which(lengths(expanding_indices) == 0)
        expanding_indices <- c(1:length(expanding_indices))[-c(rm_empty_intersections)]
        
        strm_intersect_indices <- append(strm_intersect_indices,
                                         expanding_indices)
        strm_intersect_indices <- unique(strm_intersect_indices)
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
    w_index <- c(1:nrow(wells))
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
      strm_intersect_indices <- c(1:length(strm_intersect_indices))[-c(rm_empty_intersections)]
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
    w_index <- c(1:nrow(wells))
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
    w_index <- c(1:nrow(wells))
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
        id_list[[i]] <- rep(i, length(coords[[1]]))
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
      stream_points_geometry$ID <- unlist(id_list)
      stream_id_key <<- 'ID'
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
      stop(paste0('\ncalculate_stream_depletions.R encountered Error:    ',
                  'proximity criteria required subwatersheds (Adjacent | Adjacent+Expanding)',
                  'but none supplied',
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
      stop(paste0('\ncalculate_stream_depletions.R encountered Error:    ',
                  'proximity criteria required influence radius (Local Area | Expanding | Adjacent+Expanding)',
                  'but none supplied',
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
      stop(paste0('\ncalculate_stream_depletions.R encountered Error:    ',
                  'Identifying column for streams required to calculate impacted length',
                  'but none supplied',
                  'exiting program ...'))
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
                              'Mean | Median impacted segment length in meters: ',
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
  

  
  
  
  
  
  
  
  
  
  
  
  ############################################################################################
  ######################################### RUN FUNCTIONS ####################################
  ############################################################################################
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
    status <<- 'find_impacted_stream_segments'
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
                        'impacted_points.csv'),
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
    rm(status)
    rm(output)
    #-------------------------------------------------------------------------------
  }, error = function(e){
    #-------------------------------------------------------------------------------
    # write error to log file
    status <<- 'find_impacted_stream_segments'
    writeLines(text = sprintf('%s %s',
                              'ENCOUNTERED ERROR: ',
                              class(e)[1]),
               con = log_file)
    writeLines(text = sprintf('%s %s',
                              'ON COMMAND: ',
                              paste0(e$call,collapse = ' ')),
               con = log_file)
    writeLines(text = sprintf('%s %s',
                              'FOR REASON: ',
                              paste0(e$message, collapse = ' ')),
               con = log_file)
    writeLines(text = sprintf('%s',
                              'exiting program...'),
               con = log_file)
    close(log_file)
    rm(status)
    #-------------------------------------------------------------------------------
    
    
    #-------------------------------------------------------------------------------
    # write error to console
    stop(paste0('\ncalculate_stream_depletions.R encountered Error:    ', class(e)[1],'\n',
                'during:    ', status,'\n',
                'on command:    ', paste0(e$call,collapse = ' '),'\n',
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