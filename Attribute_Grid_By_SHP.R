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
# 7/15/2025
#===============================================================================
# Runs at approximately 1 second per 1000 grid cells
##### Description of Variables #####
# out_dir: string with forward slashes, describes where to save files
# out_name: string with no '.' extension, describes name to give to output files
# shapefile: Anything with a '$geometry' column. Describes a shapefile to intersect with model grid
# model_grid: Anything with a '$geometry' column and column of ID values for the grid. Describes polygons of model grid (5 vertices)[v1,v2,v3,v4,v1]
# cell_area: numeric value [m^2] describing the area of each cell. Used when calculating what percent of cell is intersected by shapefile.
# hru_id_column: numeric, describes what column number in the model_grid to find the IDs of the grid cells
# replacement_value: numeric, describes a single number to replace the starting values with
# starting_values: either SpatRaster, RasterLayer, or vector. length of vector or number of raster cells must be equal to number of model grid cells.
#                     Describes the starting cell values to modify
# export_as_grid: boolean, describes whether to export results as a '.tif' raster
# export_long_format: boolean, describes whether to export results as a two column csv (ID,Values)
# is_raster: boolean, describes whether the starting_values is a raster or a vector
# grid_dims: vector of numeric, described in c(rows, columns) the dimensions of the model grid
# null_value: describes the value to give cells with no intersection with the shape file if no starting values are supplied
# engulf: boolean, describes whether model gridcells need to be completly covered by shapefile in order for their values to be updated
# partial_fact: numeric range 0-100, describes above what percentage the cell needs to be covered in order to have its values updated
#                 if engulf == FALSE
# scale_by_intersected_area: boolean, if false all cells above partial_fact coverage will be set to the replacement value. If true all cells
#                                above partial fact will have value scaled between starting and replacement value based on how much of cell is covered
# example: boolean, if running example code set to true, else set to false
###### Purpose of Program #####
#
#
#
#
#
#===============================================================================
###########################################################################################
############################## MAIN FUNCTION ##############################################
###########################################################################################
Attribute_Grid_By_SHP <- function(out_dir,
                                  out_name,
                                  shapefile,
                                  model_grid,
                                  cell_area = NULL,
                                  hru_id_column,
                                  replacement_value = NULL,
                                  starting_values = NULL,
                                  export_as_grid = FALSE,
                                  export_long_format = TRUE,
                                  is_raster = TRUE,
                                  grid_dims = c(NULL,NULL),
                                  null_value = -9999,
                                  engulf = TRUE,
                                  partial_fact = NULL,
                                  scale_by_intersected_area = FALSE,
                                  example = FALSE)
{
  # -------------------------------------------------------------------------------------
  # console log delineator
  cat('\n#####################################################################\n',
      'Starting Attribute_Grid_By_SHP\n\n\n')
  # -------------------------------------------------------------------------------------
  
  # -------------------------------------------------------------------------------------
  ################# ERROR HANDLING ######################################################
  if(engulf == FALSE & scale_by_intersected_area == TRUE & is.null(partial_fact) == TRUE){
    stop(paste0('\nAttribute_Grid_By_SHP: SCALE BY INTERSECTED AREA SPECIFIED WITHOUT SPECIFYING MINIMUM',
                '\nCELL VALUE (partial_fact)'))
  }
  
  if(is_raster == FALSE & is.null(grid_dims) == TRUE & is.null(starting_values) == FALSE){
    stop(paste0('\nAttribute_Grid_By_SHP: GRID VALUES SUPPLIED IN NON-RASTER FORMAT WITHOUT',
                '\nSPECIFYING DIMENSIONS OF GRID'))
  }
  
  if(is.null(grid_dims) == TRUE & export_as_grid == TRUE){
    stop(paste0('\nAttribute_Grid_By_SHP: EXPORTING AS GRID SELECTED WITHOUT',
                '\nSPECIFYING DIMENSIONS OF EXPORT GRID WITH WHICH TO CREATE RASTER'))
  }
  
  if(is.null(replacement_value) == TRUE){
    stop(paste0('\nAttribute_Grid_By_SHP: REPLACEMENT VALUE NOT SPECIFIED'))
  }
  
  if(crs(shapefile) != crs(model_grid)){
    stop(paste0('\nAttribute_Grid_By_SHP: CRS OF SHAPEFILE != CRS OF MODEL GRID'))
  }
  # -------------------------------------------------------------------------------------
  
  # -------------------------------------------------------------------------------------
  ################# WARNING HANDLING ####################################################
  if(is.null(starting_values) == TRUE){
    warning(paste0('\nAttribute_Grid_By_SHP: No starting values specified, will return grid',
                   '\nof user specified null values where shapefile not intersected'))
  }
  
  if(length(grid_dims) == 1){
    warning(paste0('\nAttribute_Grid_By_SHP: Only one grid dimension specified',
                   '\ngrid_dims[1] will be used for both dimensions'))
  }
  
  if(is.null(cell_area) == TRUE){
    warning(paste0('\nAttribute_Grid_By_SHP: No Cell area supplied',
                   '\ncalculating based on first grid cell encountered (grid[1, ])'))
  }
  # -------------------------------------------------------------------------------------
  
  
  
  
  # -------------------------------------------------------------------------------------
  ################# DECIDING WHICH FUNCTIONS TO TRIGGER #################################
  if(is_raster == FALSE){
    # Case for no starting values supplied, and no starting raster supplied for the attribute
    # needing modification
    if(is.null(starting_values) == TRUE){
      cat('\nis_raster == FALSE & starting_values == NULL',
          '\nstarting from null grid\n\n\n')
      Start_From_Null_Grid(out_dir,
                           out_name,
                           shapefile,
                           model_grid,
                           cell_area,
                           hru_id_column,
                           replacement_value,
                           starting_values,
                           export_as_grid,
                           export_long_format,
                           is_raster,
                           grid_dims,
                           null_value,
                           engulf,
                           partial_fact,
                           scale_by_intersected_area,
                           example)
    }
    # Case for starting values supplied in long format, and no starting raster supplied for the attribute
    # needing modification
    if(is.null(starting_values) == FALSE){
      cat('\nis_raster == FALSE & starting_values != NULL',
          '\nstarting from supplied long format values\n\n\n')
      Start_From_Supplied_Long_Format(out_dir,
                                      out_name,
                                      shapefile,
                                      model_grid,
                                      cell_area,
                                      hru_id_column,
                                      replacement_value,
                                      starting_values,
                                      export_as_grid,
                                      export_long_format,
                                      is_raster,
                                      grid_dims,
                                      null_value,
                                      engulf,
                                      partial_fact,
                                      scale_by_intersected_area,
                                      example)
    }
  }
  # -------------------------------------------------------------------------------------
  
  
  # -------------------------------------------------------------------------------------
  # if starting values are in the format of a raster of the same dimensionality of
  # the model grid
  if(is_raster == TRUE){
    Start_From_Supplied_Raster(out_dir,
                              out_name,
                              shapefile,
                              model_grid,
                              cell_area,
                              hru_id_column,
                              replacement_value,
                              starting_values,
                              export_as_grid,
                              export_long_format,
                              is_raster,
                              grid_dims,
                              null_value,
                              engulf,
                              partial_fact,
                              scale_by_intersected_area,
                              example)
  }
  # -------------------------------------------------------------------------------------
  
  
  
  # -------------------------------------------------------------------------------------
  # End console log delineator
  cat(paste0('\nExited Attribute_Grid_By_SHP without error\n',
             '#####################################################################\n\n\n'))
  # -------------------------------------------------------------------------------------
}
# -------------------------------------------------------------------------------------
















###########################################################################################
############################## HELPER FUNCTIONS ###########################################
###########################################################################################
#===============================================================================
# Placeholder
#===============================================================================
Start_From_Null_Grid <- function(out_dir,
                                 out_name,
                                 shapefile,
                                 model_grid,
                                 cell_area,
                                 hru_id_column,
                                 replacement_value,
                                 starting_values,
                                 export_as_grid,
                                 export_long_format,
                                 is_raster,
                                 grid_dims,
                                 null_value,
                                 engulf,
                                 partial_fact,
                                 scale_by_intersected_area,
                                 example)
{
  # -------------------------------------------------------------------------------------
  # Starting values and area to reach 100% engulf
  cat('\nSetting starting values and intersecting model grid by shapefile\n\n\n')
  original_colnames <- colnames(model_grid)
  original_colnames <- append(original_colnames, 'UPDATED_VALUE')
  model_grid$DUMMY_ID <- c(1:nrow(model_grid))
  model_grid$UPDATED_VALUE <- rep(null_value,nrow(model_grid))
  
  if(is.null(cell_area) == TRUE){
    cell_area <- st_area(model_grid[1,])
  }
  
  starting_raster <- Reconstruct_Grid(grid_dims,
                                      null_value,
                                      model_grid)
  intersected_model_grid <- st_intersection(model_grid,
                                            shapefile)
  intersected_model_grid$Percent_Area <- as.numeric(round((st_area(intersected_model_grid)/cell_area) * 100,1))
  # -------------------------------------------------------------------------------------

  
  
  
  ################################### DECIDING HOW TO UPDATE VALUES #####################
  # -------------------------------------------------------------------------------------
  # if entire cell needs to be engulfed then only set cells engulfed to the replacement value
  
  # if entire cell DOES NOT need to be engulfed and scale_by_intersected_area is specified
  # then only set cells covered above some value
  # of their area to the replacement value multiplied by how much of the cell is covered
  
  # if entire cell DOES NOT need to be engulfed and scale_by_intersected_area is  NOT specified
  # then only set cells covered above some value
  # of their area to the replacement value
  cat('\nUpdating values in intersected grid\n\n\n')
  if(engulf == TRUE){
    
    cat('\nengulf == TRUE\n\n\n')
    intersected_model_grid$UPDATED_VALUE[intersected_model_grid$Percent_Area == 100] <- replacement_value
    
  } else if(is.null(partial_fact) == FALSE & scale_by_intersected_area == TRUE){
    
    cat(paste0('\nscaling by intersected area above ','factor of ',partial_fact,' % area\n\n\n'))
    intersected_model_grid$UPDATED_VALUE[intersected_model_grid$Percent_Area >= partial_fact] <-
      replacement_value*(intersected_model_grid$Percent_Area[intersected_model_grid$Percent_Area >= partial_fact]/100)
    
  } else if(is.null(partial_fact) == FALSE & scale_by_intersected_area == FALSE){
    
    cat(paste0('\nassigning all cells above ','factor of ',partial_fact,' % area to replacement value\n\n\n'))
    intersected_model_grid$UPDATED_VALUE[intersected_model_grid$Percent_Area >= partial_fact] <- replacement_value
      
  }
  # -------------------------------------------------------------------------------------
  
  # -------------------------------------------------------------------------------------
  # set model grid equal to replacement values
  cat('\nSetting model grid to replacement values\n\n\n')
  model_grid$UPDATED_VALUE[model_grid$DUMMY_ID %in% intersected_model_grid$DUMMY_ID] <- intersected_model_grid$UPDATED_VALUE
  model_grid <- model_grid[ ,-c(which(!colnames(model_grid) %in% original_colnames))]
  # -------------------------------------------------------------------------------------


  
  
  
  
  ################################### EXPORT ############################################
  
  # -------------------------------------------------------------------------------------
  if(example == FALSE){
    if(export_as_grid == TRUE){
      
      cat(paste0('\nExporting as raster at dsn\n',file.path(out_dir,out_name),'\n\n\n'))
      
      # -------------------------------------------------------------------------------------
      xy <- xyFromCell(starting_raster,1:ncell(starting_raster)) %>%
        as.data.frame() %>%
        st_as_sf(coords = c('x','y'), crs = crs(model_grid))
      values <- st_intersection(model_grid,xy)$UPDATED_VALUE
      values(starting_raster) <- values
      # -------------------------------------------------------------------------------------
      
      # -------------------------------------------------------------------------------------
      writeRaster(x = starting_raster,
                  filename = file.path(out_dir,paste0(out_name,'.tif')),
                  overwrite = TRUE,
                  progress = 0)
      st_write(obj = model_grid,
               dsn = file.path(out_dir,paste0(out_name,'.shp')),
               append = FALSE)
      # -------------------------------------------------------------------------------------
      
    } else if(export_long_format == TRUE){
      
      # -------------------------------------------------------------------------------------
      cat(paste0('\nExporting as csv at dsn\n',file.path(out_dir,out_name),'\n\n\n'))
      export_long <- data.frame(HRU_ID = as.vector(unlist(st_drop_geometry(model_grid[hru_id_column]))),
                                UPDATED_VALUE = as.vector(unlist(model_grid$UPDATED_VALUE)))
      export_long <- export_long[order(export_long$HRU_ID, decreasing = FALSE)]
      write.csv(x = export_long,
                file = file.path(out_dir,paste0(out_name,'.csv')),
                row.names = FALSE)
      st_write(obj = model_grid,
               dsn = file.path(out_dir,paste0(out_name,'.shp')),
               append = FALSE)
      # -------------------------------------------------------------------------------------
    }
    # -------------------------------------------------------------------------------------
  } else {
    if(export_long_format == TRUE){
      export_long <<- data.frame(HRU_ID = as.vector(unlist(st_drop_geometry(model_grid[hru_id_column]))),
                                 UPDATED_VALUE = as.vector(unlist(model_grid$UPDATED_VALUE)))
    }
    if(export_as_grid == TRUE){
      # -------------------------------------------------------------------------------------
      xy <- xyFromCell(starting_raster,1:ncell(starting_raster)) %>%
        as.data.frame() %>%
        st_as_sf(coords = c('x','y'), crs = crs(model_grid))
      values <- st_intersection(model_grid,xy)$UPDATED_VALUE
      
      values(starting_raster) <- values
      raster <<- raster
      # -------------------------------------------------------------------------------------
    }
  }
}
# -------------------------------------------------------------------------------------





#===============================================================================
# Placeholder
#===============================================================================
Start_From_Supplied_Long_Format <- function(out_dir,
                                            out_name,
                                            shapefile,
                                            model_grid,
                                            cell_area,
                                            hru_id_column,
                                            replacement_value,
                                            starting_values,
                                            export_as_grid,
                                            export_long_format,
                                            is_raster,
                                            grid_dims,
                                            null_value,
                                            engulf,
                                            partial_fact,
                                            scale_by_intersected_area,
                                            example)
{
  # -------------------------------------------------------------------------------------
  # Starting values and area to reach 100% engulf
  cat('\nRetrieving starting values and intersecting model grid by shapefile\n\n\n')
  original_colnames <- colnames(model_grid)
  original_colnames <- append(original_colnames, 'UPDATED_VALUE')
  model_grid$DUMMY_ID <- c(1:nrow(model_grid))
  model_grid$UPDATED_VALUE <- starting_values
  model_grid$STARTING_VALUES <- starting_values
  
  if(is.null(grid_dims) == FALSE){
    starting_raster <- Reconstruct_Grid(grid_dims,
                                        starting_values,
                                        model_grid)
  }
  
  if(is.null(cell_area) == TRUE){
    cell_area <- st_area(model_grid[1,])
  }
  
  intersected_model_grid <- st_intersection(model_grid,
                                            shapefile)
  intersected_model_grid$Percent_Area <- as.numeric(round((st_area(intersected_model_grid)/cell_area) * 100,1))
  # -------------------------------------------------------------------------------------
  
  
  
  
  ################################### DECIDING HOW TO UPDATE VALUES #####################
  # -------------------------------------------------------------------------------------
  # if entire cell needs to be engulfed then only set cells engulfed to the replacement value
  
  # if entire cell DOES NOT need to be engulfed and scale_by_intersected_area is specified
  # then only set cells covered above some value
  # of their area to the replacement value multiplied by how much of the cell is covered
  
  # if entire cell DOES NOT need to be engulfed and scale_by_intersected_area is  NOT specified
  # then only set cells covered above some value
  # of their area to the replacement value
  cat('\nUpdating values in intersected grid\n\n\n')
  if(engulf == TRUE){
    
    cat('\nengulf == TRUE\n\n\n')
    intersected_model_grid$UPDATED_VALUE[intersected_model_grid$Percent_Area == 100] <- replacement_value
    
  } else if(is.null(partial_fact) == FALSE & scale_by_intersected_area == TRUE){
    
    cat(paste0('\nscaling by intersected area above ','factor of ',partial_fact,' % area\n\n\n'))
    # -------------------------------------------------------------------------------------
    for(i in 1:nrow(intersected_model_grid)){
      if(intersected_model_grid$Percent_Area[i] >= partial_fact){
        lerp <- abs(replacement_value - intersected_model_grid$STARTING_VALUES[i]) # how much distance is being covered
        lerp <- lerp * intersected_model_grid$Percent_Area[i]/100 # what is distance * percent area
        
        # -------------------------------------------------------------------------------------
        # if value is being increased
        if(intersected_model_grid$STARTING_VALUES[i] < replacement_value){
          intersected_model_grid$UPDATED_VALUE[i] <- intersected_model_grid$STARTING_VALUES[i] + lerp
        }
        # -------------------------------------------------------------------------------------
        
        # -------------------------------------------------------------------------------------
        # if value is being decreased
        if(intersected_model_grid$STARTING_VALUES[i] > replacement_value){
          intersected_model_grid$UPDATED_VALUE[i] <- intersected_model_grid$STARTING_VALUES[i] - lerp
        }
        # -------------------------------------------------------------------------------------
      }
      # -------------------------------------------------------------------------------------
    }
    # -------------------------------------------------------------------------------------
  } else if(is.null(partial_fact) == FALSE & scale_by_intersected_area == FALSE){
    
    cat(paste0('\nassigning all cells above ','factor of ',partial_fact,' % area to replacement value\n\n\n'))
    intersected_model_grid$UPDATED_VALUE[intersected_model_grid$Percent_Area >= partial_fact] <- replacement_value
    
  }
  # -------------------------------------------------------------------------------------
  
  # -------------------------------------------------------------------------------------
  # set model grid equal to replacement values
  cat('\nSetting model grid to replacement values\n\n\n')
  model_grid$UPDATED_VALUE[model_grid$DUMMY_ID %in% intersected_model_grid$DUMMY_ID] <- intersected_model_grid$UPDATED_VALUE
  model_grid <- model_grid[ ,-c(which(!colnames(model_grid) %in% original_colnames))]
  # -------------------------------------------------------------------------------------
  
  
  
  
  
  
  ################################### EXPORT ############################################
  
  # -------------------------------------------------------------------------------------
  if(example == FALSE){
    if(export_as_grid == TRUE){
      
      cat(paste0('\nExporting as raster at dsn\n',file.path(out_dir,out_name),'\n\n\n'))
      
      # -------------------------------------------------------------------------------------
      xy <- xyFromCell(starting_raster,1:ncell(starting_raster)) %>%
        as.data.frame() %>%
        st_as_sf(coords = c('x','y'), crs = crs(model_grid))
      values <- st_intersection(model_grid,xy)$UPDATED_VALUE
      values(starting_raster) <- values
      # -------------------------------------------------------------------------------------
      
      # -------------------------------------------------------------------------------------
      writeRaster(x = starting_raster,
                  filename = file.path(out_dir,paste0(out_name,'.tif')),
                  overwrite = TRUE,
                  progress = 0)
      st_write(obj = model_grid,
               dsn = file.path(out_dir,paste0(out_name,'.shp')),
               append = FALSE)
      # -------------------------------------------------------------------------------------
      
    } else if(export_long_format == TRUE){
      
      # -------------------------------------------------------------------------------------
      cat(paste0('\nExporting as csv at dsn\n',file.path(out_dir,out_name),'\n\n\n'))
      export_long <- data.frame(HRU_ID = as.vector(unlist(st_drop_geometry(model_grid[hru_id_column]))),
                                UPDATED_VALUE = as.vector(unlist(model_grid$UPDATED_VALUE)))
      export_long <- export_long[order(export_long$HRU_ID, decreasing = FALSE)]
      write.csv(x = export_long,
                file = file.path(out_dir,paste0(out_name,'.csv')),
                row.names = FALSE)
      st_write(obj = model_grid,
               dsn = file.path(out_dir,paste0(out_name,'.shp')),
               append = FALSE)
      # -------------------------------------------------------------------------------------
    }
    # -------------------------------------------------------------------------------------
  } else { # example mode
    if(export_long_format == TRUE){
      export_long <<- data.frame(HRU_ID = as.vector(unlist(st_drop_geometry(model_grid[hru_id_column]))),
                                 UPDATED_VALUE = as.vector(unlist(model_grid$UPDATED_VALUE)))
    }
    if(export_as_grid == TRUE){
      # -------------------------------------------------------------------------------------
      xy <- xyFromCell(starting_raster,1:ncell(starting_raster)) %>%
        as.data.frame() %>%
        st_as_sf(coords = c('x','y'), crs = crs(model_grid))
      values <- st_intersection(model_grid,xy)$UPDATED_VALUE

      values(starting_raster) <- values
      raster <<- starting_raster
      # -------------------------------------------------------------------------------------
    }
  }
}
# -------------------------------------------------------------------------------------


#===============================================================================
# Placeholder
#===============================================================================
Start_From_Supplied_Raster <- function(out_dir,
                                       out_name,
                                       shapefile,
                                       model_grid,
                                       cell_area,
                                       hru_id_column,
                                       replacement_value,
                                       starting_values,
                                       export_as_grid,
                                       export_long_format,
                                       is_raster,
                                       grid_dims,
                                       null_value,
                                       engulf,
                                       partial_fact,
                                       scale_by_intersected_area,
                                       example)
{
  # -------------------------------------------------------------------------------------
  # Starting values and area to reach 100% engulf
  cat('\nRetrieving starting values and intersecting model grid by shapefile\n\n\n')
  original_colnames <- colnames(model_grid)
  original_colnames <- append(original_colnames, 'UPDATED_VALUE')
  model_grid$DUMMY_ID <- c(1:nrow(model_grid))
  model_grid$STARTING_VALUES <- rep(0,nrow(model_grid))
  starting_raster <- starting_values
  # -------------------------------------------------------------------------------------
  
  # -------------------------------------------------------------------------------------
  if(class(starting_raster) == 'RasterLayer'){
    starting_raster <- rast(starting_raster)
  } else{}
  # -------------------------------------------------------------------------------------
  
  # -------------------------------------------------------------------------------------
  # Attributing starting value raster to model grid
  # wherever raster does not intersect is set to 0
  # unsure why terra reverses the order of extracted cells
  xy <- xyFromCell(starting_raster,1:ncell(starting_raster)) %>%
    as.data.frame() %>%
    st_as_sf(coords = c('x','y'), crs = crs(model_grid))
  values <- terra::extract(terra::flip(starting_raster,
                                       direction = 'horizontal'),
                           xyFromCell(terra::flip(starting_raster,
                                                  direction = 'horizontal'),1:ncell(starting_raster))) %>% 
    unlist() %>% as.vector()
  values <- rev(values)
  int <- st_intersection(model_grid,xy)$DUMMY_ID
  if(length(int) > 0){
    model_grid$STARTING_VALUES[model_grid$DUMMY_ID %in% int] <- values
  }
  model_grid$UPDATED_VALUE <- model_grid$STARTING_VALUES
  # -------------------------------------------------------------------------------------
  
 
  # -------------------------------------------------------------------------------------
  if(is.null(cell_area) == TRUE){
    cell_area <- st_area(model_grid[1,])
  }
  # -------------------------------------------------------------------------------------

  # -------------------------------------------------------------------------------------
  intersected_model_grid <- st_intersection(model_grid,
                                            shapefile)
  intersected_model_grid$Percent_Area <- as.numeric(round((st_area(intersected_model_grid)/cell_area) * 100,1))
  # -------------------------------------------------------------------------------------




  ################################### DECIDING HOW TO UPDATE VALUES #####################
  # -------------------------------------------------------------------------------------
  # if entire cell needs to be engulfed then only set cells engulfed to the replacement value

  # if entire cell DOES NOT need to be engulfed and scale_by_intersected_area is specified
  # then only set cells covered above some value
  # of their area to the replacement value multiplied by how much of the cell is covered

  # if entire cell DOES NOT need to be engulfed and scale_by_intersected_area is  NOT specified
  # then only set cells covered above some value
  # of their area to the replacement value
  cat('\nUpdating values in intersected grid\n\n\n')
  if(engulf == TRUE){

    cat('\nengulf == TRUE\n\n\n')
    intersected_model_grid$UPDATED_VALUE[intersected_model_grid$Percent_Area == 100] <- replacement_value

  } else if(is.null(partial_fact) == FALSE & scale_by_intersected_area == TRUE){

    cat(paste0('\nscaling by intersected area above ','factor of ',partial_fact,' % area\n\n\n'))
    # -------------------------------------------------------------------------------------
    for(i in 1:nrow(intersected_model_grid)){
      if(intersected_model_grid$Percent_Area[i] >= partial_fact){
        lerp <- abs(replacement_value - intersected_model_grid$STARTING_VALUES[i]) # how much distance is being covered
        lerp <- lerp * intersected_model_grid$Percent_Area[i]/100 # what is distance * percent area

        # -------------------------------------------------------------------------------------
        # if value is being increased
        if(intersected_model_grid$STARTING_VALUES[i] < replacement_value){
          intersected_model_grid$UPDATED_VALUE[i] <- intersected_model_grid$STARTING_VALUES[i] + lerp
        }
        # -------------------------------------------------------------------------------------

        # -------------------------------------------------------------------------------------
        # if value is being decreased
        if(intersected_model_grid$STARTING_VALUES[i] > replacement_value){
          intersected_model_grid$UPDATED_VALUE[i] <- intersected_model_grid$STARTING_VALUES[i] - lerp
        }
        # -------------------------------------------------------------------------------------
      }
      # -------------------------------------------------------------------------------------
    }
    # -------------------------------------------------------------------------------------

  } else if(is.null(partial_fact) == FALSE & scale_by_intersected_area == FALSE){

    cat(paste0('\nassigning all cells above ','factor of ',partial_fact,' % area to replacement value\n\n\n'))
    intersected_model_grid$UPDATED_VALUE[intersected_model_grid$Percent_Area >= partial_fact] <- replacement_value

  }
  # -------------------------------------------------------------------------------------

  # -------------------------------------------------------------------------------------
  # set model grid equal to replacement values
  cat('\nSetting model grid to replacement values\n\n\n')
  model_grid$UPDATED_VALUE[model_grid$DUMMY_ID %in% intersected_model_grid$DUMMY_ID] <- intersected_model_grid$UPDATED_VALUE
  model_grid <- model_grid[ ,-c(which(!colnames(model_grid) %in% original_colnames))]
  # -------------------------------------------------------------------------------------






  ################################### EXPORT ############################################
  # -------------------------------------------------------------------------------------
  if(example == FALSE){
    if(export_as_grid == TRUE){

      cat(paste0('\nExporting as raster at dsn\n',file.path(out_dir,out_name),'\n\n\n'))

      # -------------------------------------------------------------------------------------
      xy <- xyFromCell(starting_raster,1:ncell(starting_raster)) %>%
        as.data.frame() %>%
        st_as_sf(coords = c('x','y'), crs = crs(model_grid))
      values <- st_intersection(model_grid,xy)$UPDATED_VALUE
      values(starting_raster) <- values
      # -------------------------------------------------------------------------------------

      # -------------------------------------------------------------------------------------
      writeRaster(x = starting_raster,
                  filename = file.path(out_dir,paste0(out_name,'.tif')),
                  overwrite = TRUE,
                  progress = 0)
      st_write(obj = model_grid,
               dsn = file.path(out_dir,paste0(out_name,'.shp')),
               append = FALSE)
      # -------------------------------------------------------------------------------------

    } else if(export_long_format == TRUE){

      # -------------------------------------------------------------------------------------
      cat(paste0('\nExporting as csv at dsn\n',file.path(out_dir,out_name),'\n\n\n'))
      export_long <- data.frame(HRU_ID = as.vector(unlist(st_drop_geometry(model_grid[hru_id_column]))),
                                UPDATED_VALUE = as.vector(unlist(model_grid$UPDATED_VALUE)))
      export_long <- export_long[order(export_long$HRU_ID, decreasing = FALSE)]
      write.csv(x = export_long,
                file = file.path(out_dir,paste0(out_name,'.csv')),
                row.names = FALSE)
      st_write(obj = model_grid,
               dsn = file.path(out_dir,paste0(out_name,'.shp')),
               append = FALSE)
      # -------------------------------------------------------------------------------------
    }
    # -------------------------------------------------------------------------------------
  } else { # example mode
    if(export_long_format == TRUE){
      export_long <<- data.frame(HRU_ID = as.vector(unlist(st_drop_geometry(model_grid[hru_id_column]))),
                                 UPDATED_VALUE = as.vector(unlist(model_grid$UPDATED_VALUE)))
    }
    if(export_as_grid == TRUE){
      # -------------------------------------------------------------------------------------
      xy <- xyFromCell(starting_raster,1:ncell(starting_raster)) %>%
        as.data.frame() %>%
        st_as_sf(coords = c('x','y'), crs = crs(model_grid))
      values <- st_intersection(model_grid,xy)$UPDATED_VALUE
      values(starting_raster) <- values
      raster <<- starting_raster
      # -------------------------------------------------------------------------------------
    }
  }
}
# -------------------------------------------------------------------------------------





#===============================================================================
# Takes grid dims and returns raster of null values
#===============================================================================
Reconstruct_Grid <- function(grid_dims,
                             value,
                             model_grid){
  if(length(grid_dims) == 1){
    raster <- rast(nrow = grid_dims[1],
                   ncol = grid_dims[1],
                   xmin = extent(model_grid)[1],
                   xmax = extent(model_grid)[2],
                   ymin = extent(model_grid)[3],
                   ymax = extent(model_grid)[4],
                   crs = crs(model_grid),
                   vals = value)
  } else {
    raster <- rast(nrow = grid_dims[1],
                   ncol = grid_dims[2],
                   xmin = extent(model_grid)[1],
                   xmax = extent(model_grid)[2],
                   ymin = extent(model_grid)[3],
                   ymax = extent(model_grid)[4],
                   crs = crs(model_grid),
                   vals = value)
  }
  return(raster)
}
# -------------------------------------------------------------------------------------