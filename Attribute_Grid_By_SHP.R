###########################################################################################
############################## MAIN FUNCTION ##############################################
###########################################################################################
#===============================================================================
# Placeholder
#===============================================================================
Attribute_Grid_By_SHP <- function(out_dir,
                                  out_name,
                                  shapefile,
                                  model_grid,
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
                                  scale_by_intersected_area = FALSE)
{
  # -------------------------------------------------------------------------------------
  # console log delineator
  cat('\n#####################################################################\n',
      'Starting Attribute_Grid_By_SHP\n\n\n')
  # -------------------------------------------------------------------------------------
  
  # -------------------------------------------------------------------------------------
  # Error handling
  if(engulf == FALSE & scale_by_intersected_area == TRUE & is.null(partial_fact) == TRUE){
    stop(paste0('\nAttribute_Grid_By_SHP: SCALE BY INTERSECTED AREA SPECIFIED WITHOUT SPECIFYING MINIMUM',
                '\nCELL VALUE (partial_fact)'))
  }
  
  if(is_raster == FALSE & is.null(grid_dims) == TRUE & is.null(starting_values) == FALSE){
    stop(paste0('\nAttribute_Grid_By_SHP: GRID VALUES SUPPLIED IN NON-RASTER FORMAT WITHOUT',
                '\nSPECIFYING DIMENSIONS OF GRID'))
  }
  
  if(is.null(replacement_value) == TRUE){
    stop(paste0('\nAttribute_Grid_By_SHP: REPLACEMENT VALUE NOT SPECIFIED'))
    
  }
  
  if(crs(shapefile) != crs(model_grid)){
    stop(paste0('\nAttribute_Grid_By_SHP: CRS OF SHAPEFILE != CRS OF MODEL GRID'))
    
  }
  # -------------------------------------------------------------------------------------
  
  # -------------------------------------------------------------------------------------
  # Warning messages
  if(is.null(starting_values) == TRUE){
    warning(paste0('\nAttribute_Grid_By_SHP: No starting values specified, will return grid',
                   '\nof \"-9999\" values where shapefile not intersected'))
  }
  
  if(length(grid_dims) == 1){
    warning(paste0('\nAttribute_Grid_By_SHP: Only one grid dimension specified',
                   '\ngrid_dims[1] will be used for both dimensions'))
    
  }
  # -------------------------------------------------------------------------------------
  
  # -------------------------------------------------------------------------------------
  if(is_raster == FALSE & is.null(starting_values) == TRUE){
    cat('\nis_raster == FALSE & starting_values == NULL',
        '\nstarting from null grid\n\n\n')
    Start_From_Null_Grid(out_dir,
                         out_name,
                         shapefile,
                         model_grid,
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
                         scale_by_intersected_area)
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
                                 scale_by_intersected_area)
{
  # -------------------------------------------------------------------------------------
  # Starting values and area to reach 100% engulf
  cat('\nSetting starting values and intersecting model grid by shapefile\n\n\n')
  original_colnames <- colnames(model_grid)
  original_colnames <- append(original_colnames, 'UPDATED_VALUE')
  model_grid$DUMMY_ID <- c(1:nrow(model_grid))
  model_grid$UPDATED_VALUE <- rep(-9999,nrow(model_grid))
  cell_area <- st_area(model_grid[1,])
  starting_values <- Reconstruct_Null_Grid(grid_dims,
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
    
  } else if(partial_fact == TRUE & scale_by_intersected_area == TRUE){
    
    cat(paste0('\nscaling by intersected area above ','factor of ',partial_fact,' % area\n\n\n'))
    intersected_model_grid$UPDATED_VALUE[intersected_model_grid$Percent_Area >= partial_fact] <-
      replacement_value*intersected_model_grid$Percent_Area[intersected_model_grid$Percent_Area >= partial_fact]
    
  } else if(partial_fact == TRUE & scale_by_intersected_area == FALSE){
    
    cat(paste0('\nassigning all cells above ','factor of ',partial_fact,' % area to replacement value\n\n\n'))
    intersected_model_grid$UPDATED_VALUE[intersected_model_grid$Percent_Area >= partial_fact] <- replacement_value
      
  }
  # -------------------------------------------------------------------------------------
  
  # -------------------------------------------------------------------------------------
  # set model grid equal to replacement values
  cat('\nSetting model grid to replacement values\n\n\n')
  for(i in 1:nrow(model_grid)){
    if(model_grid$DUMMY_ID[i] %in% intersected_model_grid$DUMMY_ID){
      model_grid$UPDATED_VALUE[i] <- intersected_model_grid$UPDATED_VALUE[intersected_model_grid$DUMMY_ID == model_grid$DUMMY_ID[i]]
    }
  }
  model_grid <- model_grid[-c(!which(colnames(model_grid) %in% original_colnames)), ]
  # -------------------------------------------------------------------------------------


  
  
  
  
  ################################### EXPORT ############################################
  # -------------------------------------------------------------------------------------
  if(export_as_grid == TRUE){
    
    cat(paste0('\nExporting as raster at dsn\n',file.path(out_dir,out_name),'\n\n\n'))
    
    # -------------------------------------------------------------------------------------
    values <- c()
    for(i in 1:ncell(starting_values)){
      
      xy <- st_sfc(st_point(xyFromCell(starting_values,i)), crs = crs(model_grid))
      values <- append(values, st_intersection(model_grid,xy)$UPDATED_VALUE)
      
    }
    # -------------------------------------------------------------------------------------
    
    # -------------------------------------------------------------------------------------
    writeRaster(x = starting_values,
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
    export_long <- data.frame(HRU_ID = as.vector(unlist(model_grid[hru_id_column])),
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
}
# -------------------------------------------------------------------------------------


#===============================================================================
# Placeholder
#===============================================================================
Reconstruct_Null_Grid <- function(grid_dims,
                                  null_value,
                                  model_grid){
  if(length(grid_dims) == 1){
    raster <- rast(nrow = grid_dims[1],
                   ncol = grid_dims[1],
                   xmin = extent(model_grid)[1],
                   xmax = extent(model_grid)[2],
                   ymin = extent(model_grid)[3],
                   ymax = extent(model_grid)[4],
                   crs = crs(model_grid),
                   vals = null_value)
  } else {
    raster <- rast(nrow = grid_dims[1],
                   ncol = grid_dims[2],
                   xmin = extent(model_grid)[1],
                   xmax = extent(model_grid)[2],
                   ymin = extent(model_grid)[3],
                   ymax = extent(model_grid)[4],
                   crs = crs(model_grid),
                   vals = null_value)
  }
  return(raster)
}
# -------------------------------------------------------------------------------------