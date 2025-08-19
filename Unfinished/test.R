library(raster)
library(sf)
library(terra)
library(dplyr)
library(tidyverse)

# Example use
# Extract_100yr_NETCDF(raster_name = 'Test',
#                      raster_path = file.path(path_wgen,'25_WGEN-A-100yr_Scott Valley.nc'),
#                      start_date = '2025/01/01', # insert start_date here in YY/MM/DD
#                      upscale = 365.25)


#===============================================================================
# Assumes data is in daily format
# assumes second element of a name of a raster layer is its date in numeric days

# Exports the centroid of each gridcell, assumes raster layer 1 represents
# the dimensions for all other raster layers (normally the case).
# this would not be the case if a vector of file paths were passed where some represented
# scott and some shasta.

# Exports a summary in csv format of the mean, max, min, and sum of each variable contained within the raster
# The summary happens on a default yearly timestep
# for example if the variable being considered is Tmin, and the upscale is set to 365.25:
# the min, mean, max, and sum of raster[[1:366]] will be added as a value to the the appropriate csv
#===============================================================================
Extract_100yr_NETCDF <- function(raster_name, # name of exported .csv files
                                 raster_path, # raster object to be passed to the function 
                                 start_date, # input start date from DWR, must be in format YY/MM/DD 
                                 upscale = 365.25) # upscale daily data to yearly (expedites processing)
{
  # ------------------------------------------------------------------------------------------------
  raster_name <- as.vector(unlist(raster_name)) # in case passed as df column
  raster_path <- as.vector(unlist(raster_path))
  for(i in 1:length(raster_path)){
    raster_path[i] <- gsub('\\\\','/', x = raster_path[i])
  }
  warning('\nExtract_100yr_NETCDF: Assumes all rasters on same grid \n vertices only represent raster[1]')
  # ------------------------------------------------------------------------------------------------
  
  # ------------------------------------------------------------------------------------------------
  # sysinfo
  username=Sys.info()[["user"]]
  dropbox_dir=paste0("C:/Users/",username,"/LWA Dropbox/")
  path_prms <- file.path(dropbox_dir,'00_Project-Repositories','00598-PRMS-Modeling')
  path_gsp_data <- file.path(dropbox_dir,'00_Project-Repositories','00598-Siskiyou-GSP-data')
  path_wgen <- file.path(path_prms,'shared_data','WGEN')
  # ------------------------------------------------------------------------------------------------
  
  # ------------------------------------------------------------------------------------------------
  # Length warning
  if(length(raster_path) != length(raster_name)){
    warning('\nExtract_100yr_NETCDF: # names != # rasters \nfirst name will be iterated')
    
    raster_name <- paste0(raster_name[1],'_',str_pad(string = seq(1:length(raster_path)),
                                                     width = 4, side = 'left', pad = 0))
  }
  # ------------------------------------------------------------------------------------------------
  
  # ------------------------------------------------------------------------------------------------
  # for each raster to be loaded
  xy <- rast(raster_path[1])
  xy <- xyFromCell(xy[[1]],0:ncell(xy[[1]])) %>% as.data.frame()
  xy <- xy[-c(1),]
  for(i in 1:length(raster_path)){
    #-------------------------------------------------------------------------------
    # Check data is raster
    raster <- rast(raster_path[i])
    
    if(class(raster)[1] != 'SpatRaster' &
       class(raster)[1] != 'RasterLayer'){
      stop('\nExtract_100yr_NETCDF: DATA NOT IN FORM OF RASTER')
    } else if(class(raster)[1] != 'SpatRaster') {
      raster <- as(raster, 'SpatRaster')
    }
    #-------------------------------------------------------------------------------
    
    #-------------------------------------------------------------------------------
    # getting number of variables and number of dates
    variable_names <- varnames(raster)
    date_table <- sapply(strsplit(names(raster),'='), function(x) x[2]) %>% # gets second element (date) of each list object
      as.numeric() %>%
      table()
    date_seq <- as.numeric(names(date_table))
    #-------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # Initializes the progress bar
    max_pb <- 100
    pb <- winProgressBar(title = "Windows progress bar", # Window title
                         label = "Percentage completed", # Window label
                         min = 0,      # Minimum value of the bar
                         max = max_pb, # Maximum value of the bar
                         initial = 0,  # Initial value of the bar
                         width = 500L) # Width of the window
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # Update progress bar
    pctg <- paste('Reading Raster Outputs...',raster_name[i])
    setWinProgressBar(pb,
                      0,
                      label = pctg)
    # ------------------------------------------------------------------------------------------------

    
    #-------------------------------------------------------------------------------
    # code loops over every variable name,
    for(j in 1:length(variable_names)){
      #-------------------------------------------------------------------------------
      # fetching data
      output <- list()
      start <- ((j-1) * length(date_seq) + 1) # where to start stacked extraction
      rows <- matrix(nrow = length(date_seq),
                     ncol = (ncell(raster)),
                     data = as.vector(unlist(raster[[start:(length(date_seq)*j)]][1:ncell(raster)])),
                     byrow = T) %>% as.data.frame()
      rows$date <- date_seq
      rows$date <- as.Date(rows$date + as.numeric(as.Date(start_date)))
      rows <- rows[,c(ncol(rows),2:(ncol(rows)-1))]
      colnames(rows) <- c('Date',1:ncell(raster))
      #-------------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------------
      # pb elements
      math <- (((j/length(variable_names)) * 100) * (1/length(raster_name))) + (i/length(raster_name) * 100)
      math <- round(math,0)
      setWinProgressBar(pb,
                        math,
                        label = pctg)
      #-------------------------------------------------------------------------------
      
      #-------------------------------------------------------------------------------
      # Bind Results and writeout
      dir.create(file.path(path_wgen,'CSV Summaries',raster_name))
      write.csv(rows,
                file.path(path_wgen,'CSV Summaries',raster_name,paste0(raster_name,'_',variable_names[j],'.csv')),
                row.names = FALSE)
      #-------------------------------------------------------------------------------
    }
    #-------------------------------------------------------------------------------
  }
  close(pb)
  # ------------------------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------
  # write raster vertices
  write.csv(xy,
            file.path(path_wgen,paste0('WGEN_Vertices.csv')),
            row.names = FALSE)
  #-------------------------------------------------------------------------------
}
#-------------------------------------------------------------------------------