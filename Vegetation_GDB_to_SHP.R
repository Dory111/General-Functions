#===========================================================================================
# load dependencies
#===========================================================================================
library(raster)
library(sf)
library(sp)
library(stars)
library(plotly)
library(terra)
library(stringr)

#===========================================================================================
# Converts cursed format
#===========================================================================================
Veg_GDB_to_SHP <- function(fact = NULL,
                           veg_raster_path = 'C:/Users/ChristopherDory/LWA Dropbox/Christopher Dory/Projects/PRMS-Modeling-Training/Data/Input/fveg22_1.gdb',
                           watershed_path = NULL,
                           out_dir = NULL,
                           out_name = NULL)
{ 
  ####################################################################################################
  ############################################### HELPER  FUNCTIONS ##################################
  ####################################################################################################
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

  
  
  
  
  
  
  
  
  
  ####################################################################################################
  ############################################### MAIN  FUNCTION #####################################
  ####################################################################################################
  #-------------------------------------------------------------------------------------------
  # Warnings
  if(is.null(out_dir) == TRUE){
    warning(paste(' No output directory specified\n',
                  'using current working directory of\n',
                  getwd(),'\n'))
    out_dir <- getwd()
    
  }
  if(is.null(out_name) == TRUE){
    warning(paste(' No output name specified\n',
                  'using name: \'test\'',
                  '\n'))
    out_name <- 'test'
  }
  if(is.null(watershed_path) == TRUE){
    warning(paste(' No watershed specified to crop raster\n',
                  'entire state of California will be summarized'))
    Watersheds <- NULL
  } else {
    Watersheds <- st_read(watershed_path,
                          quiet = TRUE)
  }
  #-------------------------------------------------------------------------------------------
  

  cat('Loading Raster Data... Step (1/3)')
  #-------------------------------------------------------------------------------------------
  # load raster for level translation
  loading_bar(iter = 1,
              total = 100,
              width = 50,
              optional_text = str_pad('Raster for Level Translation',width = 30, side = 'right'))
  raster_for_levels <- raster(veg_raster_path)
  if(is.null(Watersheds) == FALSE){
    raster_for_levels <- crop(raster_for_levels,
                              extent(st_transform(Watersheds,
                                                  crs(raster_for_levels))))
  }
  #-------------------------------------------------------------------------------------------
  
  
  #-------------------------------------------------------------------------------------------
  # load raster for value translation
  loading_bar(iter = 33,
              total = 100,
              width = 50,
              optional_text = str_pad('Raster for Value Translation', width = 30, side = 'right'))
  raster_for_values <- rast(veg_raster_path)
  if(is.null(Watersheds) == FALSE){
    raster_for_values <- crop(raster_for_values,
                              extent(st_transform(Watersheds,
                                                  crs(raster_for_levels))))
  }
  #-------------------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------------------
  # aggregate raster if factor supplied
  if(is.null(fact) == FALSE){
    loading_bar(iter = 66,
                total = 100,
                width = 50,
                optional_text = str_pad(paste0('Aggregate Raster by ', fact), side = 'right',width = 30))
    raster_for_aggregating <- aggregate(raster_for_values, fact = fact)
    loading_bar(iter = 100,
                total = 100,
                width = 50,
                optional_text = str_pad('Done', side = 'right', width = 30))
  } else {
    raster_for_aggregating <- raster_for_values
    loading_bar(iter = 100,
                total = 100,
                width = 50,
                optional_text = str_pad('Done', side = 'right', width = 30))
  }
  #-------------------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------------------
  # data frame of all the different vegetation types
  translate_df <- data.frame(shortcode = levels(raster_for_levels)[[1]]$WHRTYPE,
                             longcode = levels(raster_for_levels)[[1]]$WHRALL,
                             translation = levels(raster_for_levels)[[1]]$WHRNAME,
                             whr10 = levels(raster_for_levels)[[1]]$WHR10NAME)
  unique_short_codes <- unique(translate_df$shortcode)
  unique_long_codes <- unique(translate_df$longcode)
  #-------------------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------------------
  blank_ <- list()
  #-------------------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------------------
  # process each unique vegetation type
  cat('Extracting Raster Data... Step (2/3)')
  for(i in 1:length(unique_long_codes)){
    
    #-------------------------------------------------------------------------------------------
    loading_bar(iter = i,
                total = length(unique_long_codes),
                width = 50,
                optional_text = 'Processing WHR Codes')
    #-------------------------------------------------------------------------------------------
    
    #-------------------------------------------------------------------------------------------
    # find where the value of the raster is equal to the vegetation value currently
    # being processed
    values_for_string_code <- levels(raster_for_values)[[1]]$Value[levels(raster_for_values)[[1]]$WHRALL == unique_long_codes[i]]
    #-------------------------------------------------------------------------------------------
    
    #-------------------------------------------------------------------------------------------
    # what is the actual vegetation and whr10 type of the short code
    translation_for_string_code <- translate_df$translation[translate_df$longcode == unique_long_codes[i]][1]
    whr10_for_string_code <- translate_df$whr10[translate_df$longcode == unique_long_codes[i]][1]
    short_code <- substr(unique_long_codes[i],1,3)
    size <- substr(unique_long_codes[i],4,4)
    density <- substr(unique_long_codes[i],5,5)
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
    tmp$WHRALL <- rep(unique_long_codes[i], nrow(tmp))
    tmp$WHRNAME <- rep(translation_for_string_code, nrow(tmp))
    tmp$WHR10 <- rep(whr10_for_string_code, nrow(tmp))
    tmp$WHRTYPE <- rep(short_code, nrow(tmp))
    tmp$DENSITY <- rep(density,nrow(tmp))
    tmp$SIZE <- rep(size, nrow(tmp))
    blank_[[i]] <- tmp
    #-------------------------------------------------------------------------------------------
  }
  blank_ <- do.call(rbind, blank_)
  #-------------------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------------------
  cat('Rasterizing and Exporting Results... Step (3/3)')
  loading_bar(iter = 1,
              total = 100,
              width = 50,
              optional_text = str_pad('Rasterizing', side = 'right', width = 30))
  #-------------------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------------------
  # convert to final smaller versions
  # works by rasterizing the dataframe of all the converted polygons to a
  # regular raster (instead of the cursed .gdb format)
  final_rasterized <- rasterize(blank_,
                                raster_for_aggregating, c('WHRALL'))
  final_rasterized2 <- rasterize(blank_,
                                 raster_for_aggregating, c('WHRNAME'))
  final_rasterized3 <- rasterize(blank_,
                                 raster_for_aggregating, c('WHR10'))
  final_rasterized4 <- rasterize(blank_,
                                 raster_for_aggregating, c('WHRTYPE'))
  final_rasterized5 <- rasterize(blank_,
                                 raster_for_aggregating, c('DENSITY'))
  final_rasterized6 <- rasterize(blank_,
                                 raster_for_aggregating, c('SIZE'))
  raster_stack <- c(final_rasterized,
                    final_rasterized2,
                    final_rasterized3,
                    final_rasterized4,
                    final_rasterized5,
                    final_rasterized6)
  #-------------------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------------------
  if(is.null(fact) == FALSE){
    
    loading_bar(iter = 33,
                total = 100,
                width = 50,
                optional_text = str_pad('Aggregated Raster -> Polygons', side = 'right', width = 30))
    #-------------------------------------------------------------------------------------------
    final_polygons <- st_as_sf(as.polygons(final_rasterized))
    #-------------------------------------------------------------------------------------------
    
    #-------------------------------------------------------------------------------------------
    # assign the aggregated polygons the correct vegetation type
    final_polygons$WHR10 <- rep(NA,(nrow(final_polygons)))
    for(i in 1:nrow(final_polygons)){
      
      final_polygons$WHR10[i] <- translate_df$whr10[final_polygons$WHRALL[i] == translate_df$translation][1]
      
    }
    #-------------------------------------------------------------------------------------------
  }
  #-------------------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------------------
  # create dirs for numerous outputs
  dir.create(file.path(out_dir,'WHRSHP'))
  dir.create(file.path(out_dir,'WHRRAST'))
  #-------------------------------------------------------------------------------------------

  #-------------------------------------------------------------------------------------------
  # Writeout 
  # POINT DIRECTORIES TO WRITEOUT LOCATIONS
  loading_bar(iter = 66,
              total = 100,
              width = 50,
              optional_text = str_pad('Writeout', side = 'right', width = 30))
  st_write(blank_,
           file.path(out_dir,'WHRSHP', paste0(out_name,'_WHRSHP.shp')),
           append = FALSE,
           quiet = TRUE)
  writeRaster(raster_stack,
              file.path(out_dir,'WHRRAST',paste0(out_name,'_WHRRAST.tif')), 
              overwrite = TRUE)
  #-------------------------------------------------------------------------------------------

  #-------------------------------------------------------------------------------------------
  loading_bar(iter = 100,
              total = 100,
              width = 50,
              optional_text = str_pad('Done', side = 'right', width = 30))
  cat('IMPORTANT:: TIF REQUIRES AUX FILE FOR LEVELS \n\n')
  invisible(gc())
  #-------------------------------------------------------------------------------------------
}
#-------------------------------------------------------------------------------------------