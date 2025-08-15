library(raster)
library(sf)
library(sp)
library(stars)
library(plotly)
library(terra)
#===========================================================================================
# Converts fveg WHRTYPE geodatabase, which was somehow exported in such a cursed format
# that it is nearly unuseable except in ESRI products,
# to more normal formats
#===========================================================================================
Veg_GDB_to_SHP <- function(fact = 4, # aggregation factor (will make files smaller)
                           veg_raster_path = 'C:/Users/ChristopherDory/LWA Dropbox/Christopher Dory/Projects/PRMS-Modeling-Training/Data/Input/fveg22_1.gdb',
                           veg_raster_projection = 3310, # epsg projection number
                           watershed_path = file.path('C:/Users/ChristopherDory/LWA Dropbox/Christopher Dory/Projects/Upland-Management-Siskiyou/Shapefiles',
                                                      'Upland_project_identification_QGIS',
                                                      'shapefiles_used',
                                                      'siskiyou_watersheds.shp'),
                           plot_diagnostics = TRUE,
                           whr_all_of_interest = NULL,
                           out_dir = 'C:/Users/ChristopherDory/LWA Dropbox/Christopher Dory/Projects/PRMS-Modeling-Training/Data/',
                           out_name = 'Scott')
{ 
  #-------------------------------------------------------------------------------
  if(plot_diagnostics == TRUE & is.null(whr_all_of_interest) == TRUE){
    stop(paste0('VEG_GDB_TO_SHP:\n\n',
                'Output plot selected but no vegetation type\n',
                'to plot passed'))
  }
  #-------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------
  Watersheds <<- st_read(watershed_path,
                         quiet = TRUE)
  #-------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------------------
  # POINT THESE TO THE FVEG GEO DATABASE
  cat('Loading Raster For Value Translation \n\n')
  raster_for_levels <- raster(veg_raster_path)
  raster_for_levels <- crop(raster_for_levels,
                            extent(st_transform(Watersheds,
                                                veg_raster_projection)))
  
  
  cat('Loading Raster For Values \n\n')
  raster_for_values <- rast(veg_raster_path)
  raster_for_values <- crop(raster_for_values,
                            extent(st_transform(Watersheds,
                                                veg_raster_projection)))
  
  cat('Aggregating Raster For Values \n\n')
  raster_for_aggregating <- aggregate(raster_for_values, fact = fact)
  #-------------------------------------------------------------------------------------------
  
  
  #-------------------------------------------------------------------------------------------
  # data frame of all the different vegetation types
  translate_df <- data.frame(shortcode = levels(raster_for_levels)[[1]]$WHRTYPE,
                             longcode = levels(raster_for_levels)[[1]]$WHRALL,
                             translation = levels(raster_for_levels)[[1]]$WHRNAME,
                             whr10 = levels(raster_for_levels)[[1]]$WHR10NAME)
  unique_short_codes <- unique(translate_df$shortcode)
  #-------------------------------------------------------------------------------------------
  
  
  #-------------------------------------------------------------------------------------------
  cat('Processing Blank Raster \n\n')
  blank_ <- raster_for_values
  blank_ <- st_as_sf(as.polygons(blank_))
  blank_$WHRALL <- rep(-9999, nrow(blank_))
  blank_$WHR10 <- rep(-9999, nrow(blank_))
  blank_ <- blank_[1, ]
  #-------------------------------------------------------------------------------------------
  
  
  #-------------------------------------------------------------------------------------------
  # process each unique vegetation type
  for(i in 1:length(unique_short_codes)){
    
    #-------------------------------------------------------------------------------------------
    # Progress
    cat('Processing Actual Raster WHR Codes ',
        i/length(unique_short_codes) * 100, ' % Done \n\n')
    #-------------------------------------------------------------------------------------------
    
    #-------------------------------------------------------------------------------------------
    # find where the value of the raster is equal to the vegetation value currently
    # being processed
    values_for_string_code <- levels(raster_for_values)[[1]]$Value[grepl(unique_short_codes[i],
                                                                         levels(raster_for_values)[[1]]$WHRALL) == TRUE]
    #-------------------------------------------------------------------------------------------
    
    #-------------------------------------------------------------------------------------------
    # what is the actual vegetation and whr10 type of the short code
    translation_for_string_code <- translate_df$translation[translate_df$shortcode == unique_short_codes[i]][1]
    whr10_for_string_code <- translate_df$whr10[translate_df$shortcode == unique_short_codes[i]][1]
    #-------------------------------------------------------------------------------------------
    
    #-------------------------------------------------------------------------------------------
    # set a temporary raster to NA where it doesnt equal the current vegetation type
    tmp <- raster_for_values
    values(tmp)[!(values(tmp) %in% values_for_string_code)] <- NA
    #-------------------------------------------------------------------------------------------
    
    #-------------------------------------------------------------------------------------------
    # convert the raster to polygons for easier compatibility with other programs
    # and bind the polygons of the current vegetation type to the overall dataframe
    tmp <- st_as_sf(as.polygons(tmp))
    tmp$WHRALL <- rep(translation_for_string_code, nrow(tmp))
    tmp$WHR10 <- rep(whr10_for_string_code, nrow(tmp))
    blank_ <- rbind(blank_, tmp)
    #-------------------------------------------------------------------------------------------
    
  }
  #-------------------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------------------
  # get rid of the blank first row
  blank_ <- blank_[-c(1), ]
  #-------------------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------------------
  # convert to final smaller versions
  # works by rasterizing the dataframe of all the converted polygons to a
  # regular raster (instead of the cursed .gdb format)
  final_rasterized <- rasterize(blank_,
                                raster_for_aggregating, c('WHRALL'))
  final_rasterized2 <- rasterize(blank_,
                                 raster_for_aggregating, c('WHR10'))
  final_polygons <- st_as_sf(as.polygons(final_rasterized))
  #-------------------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------------------
  # assign the aggregated polygons the correct vegetation type
  final_polygons$WHR10 <- rep(NA,(nrow(final_polygons)))
  for(i in 1:nrow(final_polygons)){
    
    final_polygons$WHR10[i] <- translate_df$whr10[final_polygons$WHRALL[i] == translate_df$translation][1]
    
  }
  #-------------------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------------------
  if(plot_diagnostics == TRUE){
    cat('Plotting diagnostics \n\n')
    
    plot(final_rasterized, main = 'Raster')
    plot(st_transform(Watersheds, veg_raster_projection),
         add = T,
         col = NA,
         border = 'red',
         lwd = 2)
    
    plot(st_geometry(st_transform(Watersheds, veg_raster_projection)),
         main = whr_all_of_interest,
         col = NA,
         border = 'blue',
         lwd = 2)
    plot(st_geometry(final_polygons[final_polygons$WHRALL == whr_all_of_interest, ]),
         border = NA,
         col = 'red',
         add = T)
    plot(st_geometry(st_transform(Watersheds, veg_raster_projection)),
         add = T,
         col = NA,
         border = 'blue',
         lwd = 2)
  }
  #-------------------------------------------------------------------------------------------
  

  #-------------------------------------------------------------------------------------------
  # Writeout 
  # POINT DIRECTORIES TO WRITEOUT LOCATIONS
  cat('Writeout files \n\n')
  st_write(final_polygons,
           file.path(out_dir,paste0(out_name,'_WHRALL.shp')),
           append = FALSE,
           quiet = TRUE)
  writeRaster(final_rasterized,
              file.path(out_dir,paste0(out_name,'_WHRALL.tif')), 
              overwrite = TRUE)
  writeRaster(final_rasterized2,
              file.path(out_dir,paste0(out_name,'_WHR10.tif')), 
              overwrite = TRUE)
  #-------------------------------------------------------------------------------------------

  
  cat('IMPORTANT:: TIF REQUIRES AUX FILE FOR LEVELS \n\n')

}
#-------------------------------------------------------------------------------------------