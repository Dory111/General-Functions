library(raster)
library(sf)
library(terra)

# Example use
# Extract_100yr_NETCDF(raster_name = 'Test',
#                      raster_path = file.path(path_wgen,'25_WGEN-A-100yr_Scott Valley.nc'),
#                      start_date = '2025/01/01', # insert start_date here in YY/MM/DD
#                      upscale = 365.25)

raster <- rast(file.path(path_wgen,'25_WGEN-A-100yr_Scott Valley.nc'))
row <- raster[[1]][1:ncell(raster)]
plot(raster[[1]])
x <- raster(ncol=10, nrow=10, xmn=-1000, xmx=1000, ymn=-100, ymx=900)
x <- as(x,'SpatRaster')
values(x) <- runif(n = ncell(x), min = 1, max = 100)
plot(x)
x[5]
length(names(raster))/3
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
    date_seq <- seq(from = 1, to = length(date_table), by = upscale) %>%
      floor()
    #-------------------------------------------------------------------------------
    
    #-------------------------------------------------------------------------------
    # Setting up dataframe
    output1 <- matrix(nrow = length(date_seq),
                      ncol = length(variable_names)) %>%
      as.data.frame()
    
    units <- units(raster[[seq(from = 1,
                               to = length(date_table)*length(variable_names),
                               by = length(date_table))]])
    rownames(output1) <- date_seq
    colnames(output1) <- paste0(variable_names,' (',units,')')
    #-------------------------------------------------------------------------------
    
    #-------------------------------------------------------------------------------
    # copies for min and max and sum
    output2 <- output1
    output3 <- output1
    output4 <- output1
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
    # loop over every date and variable
    # index 1 and index 2 are defined as a multiple of the data length
    # netcdf files are stored as var1: 1-10, var2: 11-20 etc
    # for example in 10 years of data the first year of precipitation might be stored at
    # index 1:366, while the first year of tmax might be stored at index 3660:4026
    counter <- 1
    log <- c()
    for(date in 1:(length(date_seq)-1)){
      for(varnam in 1:length(variable_names)){
        ind1 <- (date_seq[date]+((varnam-1)*length(date_table))) 
        ind2 <- (date_seq[date+1]+((varnam-1)*length(date_table)))
        seq <- c(ind1:ind2)
        output1[date,varnam] <- mean(as.vector(raster[[seq]]), na.rm = T)
        output2[date,varnam] <- max(as.vector(raster[[seq]]), na.rm = T)
        output3[date,varnam] <- min(as.vector(raster[[seq]]), na.rm = T)
        output4[date,varnam] <- sum(as.vector(raster[[seq]]), na.rm = T)
        log <- append(log, unique(varnames(raster[[seq]])))
        
        #-------------------------------------------------------------------------------
        # Error message in rare case a sequence that is supposed to contain
        # a full year (default) of data for a single variable instead overflows into a different
        if(date > 1){
          if(log[date-1] == log[date] |
             length(log[date]) > 1){
            stop('\nExtract_100yr_NETCDF: SEQUENCE DOES NOT RETURN UNIQUE VARIABLES')
          }
        }
        #-------------------------------------------------------------------------------
      }
      setWinProgressBar(pb,
                        round((date/length(date_seq)) * 100,0),
                        label = pctg)
      counter <- counter + 1
    }
    close(pb)
    #-------------------------------------------------------------------------------
    
   
    #-------------------------------------------------------------------------------
    # Writeout
    output1$Date <- as.Date((date_seq - 1) + as.numeric(as.Date(start_date)))
    output2$Date <- as.Date((date_seq - 1) + as.numeric(as.Date(start_date)))
    output3$Date <- as.Date((date_seq - 1) + as.numeric(as.Date(start_date)))
    output4$Date <- as.Date((date_seq - 1) + as.numeric(as.Date(start_date)))
    write.csv(output1,
              file.path(path_wgen,'CSV Summaries',paste0('Summary_of_Means_By_',floor(upscale),'_',raster_name,'.csv')),
              row.names = FALSE)
    write.csv(output2,
              file.path(path_wgen,'CSV Summaries',paste0('Summary_of_Maxes_By_',floor(upscale),'_',raster_name,'.csv')),
              row.names = FALSE)
    write.csv(output3,
              file.path(path_wgen,'CSV Summaries',paste0('Summary_of_Mins_By_',floor(upscale),'_',raster_name,'.csv')),
              row.names = FALSE)
    write.csv(output4,
              file.path(path_wgen,'CSV Summaries',paste0('Summary_of_Sums_By_',floor(upscale),'_',raster_name,'.csv')),
              row.names = FALSE)
    #-------------------------------------------------------------------------------
  }
  # ------------------------------------------------------------------------------------------------
  write.csv(xy,
            file.path(path_wgen,paste0('WGEN_Vertices.csv')),
            row.names = FALSE)
}
#-------------------------------------------------------------------------------