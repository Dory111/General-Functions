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
  if(is_raster == TRUE){
    
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
  for(i in 1:nrow(model_grid)){
    if(model_grid$DUMMY_ID[i] %in% intersected_model_grid$DUMMY_ID){
      model_grid$UPDATED_VALUE[i] <- intersected_model_grid$UPDATED_VALUE[intersected_model_grid$DUMMY_ID == model_grid$DUMMY_ID[i]]
    }
  }
  model_grid <- model_grid[ ,-c(which(!colnames(model_grid) %in% original_colnames))]
  # -------------------------------------------------------------------------------------


  
  
  
  
  ################################### EXPORT ############################################
  
  # -------------------------------------------------------------------------------------
  if(example == FALSE){
    if(export_as_grid == TRUE){
      
      cat(paste0('\nExporting as raster at dsn\n',file.path(out_dir,out_name),'\n\n\n'))
      
      # -------------------------------------------------------------------------------------
      values <- c()
      for(i in 1:ncell(starting_raster)){
        
        xy <- st_sfc(st_point(xyFromCell(starting_raster,i)), crs = crs(model_grid))
        values <- append(values, st_intersection(model_grid,xy)$UPDATED_VALUE)
        
      }
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
  } else {
    if(export_long_format == TRUE){
      export_long <<- data.frame(HRU_ID = as.vector(unlist(st_drop_geometry(model_grid[hru_id_column]))),
                                 UPDATED_VALUE = as.vector(unlist(model_grid$UPDATED_VALUE)))
    }
    if(export_as_grid == TRUE){
      # -------------------------------------------------------------------------------------
      values <- c()
      for(i in 1:ncell(starting_raster)){
        
        xy <- st_sfc(st_point(xyFromCell(starting_raster,i)), crs = crs(model_grid))
        values <- append(values, st_intersection(model_grid,xy)$UPDATED_VALUE)
        
      }
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
  for(i in 1:nrow(model_grid)){
    if(model_grid$DUMMY_ID[i] %in% intersected_model_grid$DUMMY_ID){
      model_grid$UPDATED_VALUE[i] <- intersected_model_grid$UPDATED_VALUE[intersected_model_grid$DUMMY_ID == model_grid$DUMMY_ID[i]]
    }
  }
  model_grid <- model_grid[ ,-c(which(!colnames(model_grid) %in% original_colnames))]
  # -------------------------------------------------------------------------------------
  
  
  
  
  
  
  ################################### EXPORT ############################################
  
  # -------------------------------------------------------------------------------------
  if(example == FALSE){
    if(export_as_grid == TRUE){
      
      cat(paste0('\nExporting as raster at dsn\n',file.path(out_dir,out_name),'\n\n\n'))
      
      # -------------------------------------------------------------------------------------
      values <- c()
      for(i in 1:ncell(starting_raster)){
        
        xy <- st_sfc(st_point(xyFromCell(starting_raster,i)), crs = crs(model_grid))
        values <- append(values, st_intersection(model_grid,xy)$UPDATED_VALUE)
        
      }
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
  } else { # example mode
    if(export_long_format == TRUE){
      export_long <<- data.frame(HRU_ID = as.vector(unlist(st_drop_geometry(model_grid[hru_id_column]))),
                                 UPDATED_VALUE = as.vector(unlist(model_grid$UPDATED_VALUE)))
    }
    if(export_as_grid == TRUE){
      # -------------------------------------------------------------------------------------
      values <- c()
      for(i in 1:ncell(starting_raster)){
        
        xy <- st_sfc(st_point(xyFromCell(starting_raster,i)), crs = crs(model_grid))
        values <- append(values, st_intersection(model_grid,xy)$UPDATED_VALUE)
        
      }
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




###########################################################################################
############################## EXAMPLE ####################################################
###########################################################################################
polygon <- Create_Example_Shapefile()
grid <- Create_Example_Model_Grid()

# -------------------------------------------------------------------------------------
# Plot 1
# Displays prior condition
plot(st_geometry(grid),
     border = 'gray60',
     main = 'Starting Condition',
     axes = T)

plot(st_geometry(polygon),
     border = NA,
     col = Hex_to_RGBA('dodgerblue3', Alpha = 0.6),
     add = T)
for(i in 1:nrow(grid)){
  text(x = st_centroid(grid$geometry[i])[[1]][1],
       y = st_centroid(grid$geometry[i])[[1]][2],
       labels = '1')
}
legend(x = 'topleft',
       legend = c('Model Grid', 'Intersecting Shapefile'),
       col = c('gray60','dodgerblue3'),
       lwd = c(3,NA),
       pch = c(NA,15),
       cex = 1.3)
# -------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------
# Run function
Attribute_Grid_By_SHP(out_dir = '',
                      out_name = '',
                      shapefile = polygon,
                      model_grid = grid,
                      hru_id_column = 1,
                      replacement_value = 3,
                      starting_values = rep(1,100),
                      export_as_grid = TRUE,
                      export_long_format = TRUE,
                      is_raster = FALSE,
                      grid_dims = c(10,10),
                      null_value = 0,
                      engulf = FALSE,
                      partial_fact = 10,
                      scale_by_intersected_area = TRUE,
                      example = TRUE)
# -------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------
# Plot 2
# Displays condition after assigning new values
plot(st_geometry(grid),
     border = 'gray60',
     main = 'Ending Condition',
     axes = T)

plot(st_geometry(polygon),
     border = NA,
     col = Hex_to_RGBA('dodgerblue3', Alpha = 0.6),
     add = T)
for(i in 1:nrow(grid)){
  if(round(export_long$UPDATED_VALUE[i],1) == 1){
    text(x = st_centroid(grid$geometry[i])[[1]][1],
         y = st_centroid(grid$geometry[i])[[1]][2],
         labels = round(export_long$UPDATED_VALUE[i],1))
  } else {
    text(x = st_centroid(grid$geometry[i])[[1]][1],
         y = st_centroid(grid$geometry[i])[[1]][2],
         labels = round(export_long$UPDATED_VALUE[i],1),
         col = 'red')
  }
  
}
legend(x = 'topleft',
       legend = c('Model Grid', 'Intersecting Shapefile'),
       col = c('gray60','dodgerblue3'),
       lwd = c(3,NA),
       pch = c(NA,15),
       cex = 1.3)
# -------------------------------------------------------------------------------------


# -------------------------------------------------------------------------------------
# Plot 3
plot(raster, col = rev(sequential_hcl(n = 100, palette = 'Reds')),
     main = 'Exported As Raster')
plot(st_geometry(grid),
     border = 'gray60',
     add = T)
plot(st_geometry(polygon),
     border = 'black',
     col = NA,
     add = T, lwd = 3)
for(i in 1:nrow(grid)){
  text(x = st_centroid(grid$geometry[i])[[1]][1],
       y = st_centroid(grid$geometry[i])[[1]][2],
       labels = round(export_long$UPDATED_VALUE[i],1))
}
legend(x = 'topleft',
       legend = c('Model Grid', 'Intersecting Shapefile','Raster Values'),
       col = c('gray60','black','red'),
       lwd = c(3,3,NA),
       pch = c(NA,NA,15),
       cex = 1.3)
# -------------------------------------------------------------------------------------













###########################################################################################
############################## EXAMPLE FUNCTIONS ##########################################
###########################################################################################
#===============================================================================
# Creates grid for the example
#===============================================================================
Create_Example_Model_Grid <- function(){
  test_grid <- list(rbind(c(-243426, 395081),
                          c(-243431, 395081),
                          c(-243431, 395093),
                          c(-243426, 395093),
                          c(-243426, 395081))) %>% st_polygon()
  l <- list()
  
  start_pos <- rbind(c(-243426, 395081),
                     c(-243431, 395081),
                     c(-243431, 395093),
                     c(-243426, 395093),
                     c(-243426, 395081))
  new_rbind <- start_pos
  mult <- 1
  counter <- 1
  for(i in 1:99){
    if(i == 1){
      l[[counter]] <- test_grid
      counter <- counter + 1
    }
    if(i %% 10 == 0 &
       i > 1 & i != 10){
      new_rbind <- start_pos
      new_rbind[,2] <- new_rbind[,2] + 12 * mult
      new_polygon <- list(new_rbind) %>% st_polygon()
      mult <- mult + 1
      l[[counter]] <- new_polygon
      counter <- counter + 1
    }else if (i %% 10 == 0 &
              i > 1 & i == 10){
      new_rbind[,1] <- new_rbind[,1] + 5
      new_polygon <- list(new_rbind) %>% st_polygon()
      l[[counter]] <- new_polygon
      counter <- counter + 1
      new_rbind <- start_pos
      new_rbind[,2] <- new_rbind[,2] + 12 * mult
      new_polygon <- list(new_rbind) %>% st_polygon()
      mult <- mult + 1
      l[[counter]] <- new_polygon
      counter <- counter + 1
    }else if (i > 1) {
      new_rbind[,1] <- new_rbind[,1] + 5
      new_polygon <- list(new_rbind) %>% st_polygon()
      l[[counter]] <- new_polygon
      counter <- counter + 1
    }
  }
  grid <- st_sfc(l,crs = 3310)
  
  
  
  df <- data.frame(HRUID = c(1:100),
                   lat = rep(0,100),
                   lon = rep(1,100))
  df <- st_as_sf(df, coords = c('lon','lat'), crs = 3310)
  df$geometry <- grid
  grid <- df
  return(grid)
}



#===============================================================================
# Creates shapefile for the example
#===============================================================================
Create_Example_Shapefile <- function(){
  x0 <- -243426
  y0 <- 395081
  polygon <- rbind(c(x0, y0),
                  c(x0 + 45, y0 + 20),
                  c(x0 + 41, y0 + 32),
                  c(x0 + 20, y0 + 50),
                  c(x0, y0)) %>% list() %>% st_polygon() %>% st_sfc(crs = 3310)
  df <- data.frame(ID = 1,
                   lat = 0,
                   lon = 1)
  df <- st_as_sf(df, coords = c('lon','lat'), crs = 3310)
  df$geometry <- polygon
  polygon <- df
  return(polygon)
}