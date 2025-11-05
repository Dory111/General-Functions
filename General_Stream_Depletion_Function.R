calculate_stream_depletions <- function(streams,
                                        streams_are_points = FALSE,
                                        wells,
                                        subwatersheds,
                                        proximity_criteria = 'adjacent',
                                        diag_out_dir = getwd())
{
  ############################################################################################
  ######################################### HELPER FUNCTIONS #################################
  ############################################################################################
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
  ######################################### MAIN FUNCTIONS #################################
  ############################################################################################
  #-------------------------------------------------------------------------------
  # Takes desired proximity criteria and decides which stream segments are
  # affected by each well
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
      average_length <- do.call(rbind, average_length)
      writeLines(text = sprintf('%s',
                                'Streams not preprocessed to points ... converting to points'),
                 con = log_file)
      #-------------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------------
      # getting points from stream linestrings
      stream_points_list <- list()
      average_length <- list()
      for(i in 1:nrow(streams)){
        coords <- Extract_SF_Linestring_Vertices(streams$geometry[i])
        stream_points_list[[i]] <- cbind(coords[[2]],
                                         coords[[1]])
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
      #-------------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------------
      # save memory
      rm(stream_points_list)
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
    }
    #-------------------------------------------------------------------------------
  }
  #-------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------
  # close log file
  close(log_file)
  #-------------------------------------------------------------------------------
}
#-------------------------------------------------------------------------------