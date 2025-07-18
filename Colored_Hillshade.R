###########################################################################################
############################## MAIN FUNCTION ##############################################
###########################################################################################
Create_Colored_Hillshade <- function(breaks,
                                     n_darkening_values,
                                     min_darkening_value = 0.1,
                                     colors,
                                     hillshade,
                                     raster_of_values)
{
  #-------------------------------------------------------------------------------
  # Starting variables
  raster_list <- list()
  raster_names <- list()
  counter <- 0
  #-------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------
  # correcting classes
  if(class(hillshade) == 'RasterLayer'){
    hillshade <- rast(hillshade)
  }
  if(class(raster_of_values) == 'RasterLayer'){
    raster_of_values <- rast(raster_of_values)
  }
  #-------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------
  # Getting hillshade values from the value raster
  xy <- xyFromCell(hillshade,1:ncell(hillshade))
  values_at_hillshade <- terra::extract(raster_of_values,
                                        xy) %>%
    unlist() %>%
    as.vector()
  #-------------------------------------------------------------------------------

  #-------------------------------------------------------------------------------
  # creating raster of same dimensions as hillshade but with the extracted values
  raster_values_at_hillshade <- rast(nrows = nrow(hillshade),
                                     ncols = ncol(hillshade),
                                     vals = values_at_hillshade,
                                     crs = crs(hillshade),
                                     xmin = as.vector(ext(hillshade)[1]),
                                     xmax = as.vector(ext(hillshade)[2]),
                                     ymin = as.vector(ext(hillshade)[3]),
                                     ymax = as.vector(ext(hillshade)[4]))
  #-------------------------------------------------------------------------------

  #-------------------------------------------------------------------------------
  normalized_hillshade <- (hillshade - minmax(hillshade)[1]) /
    (minmax(hillshade)[2] - minmax(hillshade)[1])
  #-------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------
  # for each range between break[i] and break[i+1] set all the values of the raster
  # not within the correct value band to NA
  for(br in 1:(length(breaks)-1)){
    raster_step <- normalized_hillshade
    step_values <- values(raster_step)
    
    step_values[values(raster_values_at_hillshade) < breaks[br]] <- NA
    step_values[values(raster_values_at_hillshade) > breaks[br + 1]] <- NA
    
    #-------------------------------------------------------------------------------
    # For each darkening value, increment the counter
    # wherever the normalized hillshade is less than the darkening value
    # set it to NA, so for example if darken was 0.9, anything less than
    # 0.9 in the normalized hillshade would be set to 0
    for(darken in seq(0, 1, length.out = n_darkening_values)){
      counter <- counter + 1
      step_values[step_values < darken] <- NA
      values(raster_step) <- step_values
      
      cap_on_darkening <- max(darken,min_darkening_value) # dont create black squares
      col <- darken(colors[br], 1 - cap_on_darkening) 
      
      raster_list[[counter]] <- raster_step
      raster_names[[counter]] <- col
      
    }
    #-------------------------------------------------------------------------------
  }
  #-------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------
  # Create raster stack, give names according to the color of that layer
  raster_stack <- rast(raster_list)
  names(raster_stack) <- unlist(raster_names)
  return(raster_stack)
  #-------------------------------------------------------------------------------
}
#-------------------------------------------------------------------------------