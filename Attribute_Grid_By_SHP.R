Attribute_Grid_By_SHP <- function(wd,
                                  out_dir,
                                  out_name,
                                  shapefile,
                                  model_grid,
                                  replacement_value = NULL,
                                  starting_values = NULL,
                                  is_raster = TRUE,
                                  grid_dims = NULL,
                                  null_value = -9999,
                                  engulf = TRUE,
                                  partial_fact = NULL,
                                  scale_by_intersected_area = TRUE)
{
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
  # -------------------------------------------------------------------------------------
  
  # -------------------------------------------------------------------------------------
  # Warning messages
  if(is.null(starting_values) == TRUE){
    warning(paste0('\nAttribute_Grid_By_SHP: No starting values specified, will return grid',
                   '\nof \"-9999\" values where shapefile not intersected'))
  }
  # -------------------------------------------------------------------------------------
}
# -------------------------------------------------------------------------------------