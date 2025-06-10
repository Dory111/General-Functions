#===========================================================================================
# Only reads in a certain number of rows at a time so
# user can be notified 
# NOTE: assumes each row is a date and HRUs are listed in long_pivot format
# function not valid for other data structures
#===========================================================================================
Read_HRU_Outputs_By_Row_Byte <- function(model_name_long,
                                        model_name_short,
                                        csv_name,
                                        parameter,
                                        nrow_read = 1e6,
                                        read_mode = 'Long')
{
  write_mode <- str_to_title(write_mode) # account for lowercase
  
  # ------------------------------------------------------------------------------------------------
  if(write_mode == 'Long'){
    output <- Long_Read(model_name_long = model_name_long,
                        model_name_short = model_name_short,
                        csv_name = csv_name,
                        parameter = parameter,
                        nrow_read = nrow_read)
  }
  # ------------------------------------------------------------------------------------------------
  
  # ------------------------------------------------------------------------------------------------
  if(read_mode == 'Wide'){
    
  }
  # ------------------------------------------------------------------------------------------------
  return(output)
}
# ------------------------------------------------------------------------------------------------





#===========================================================================================
# PLACEHOLDER
#===========================================================================================
Long_Read <- function(model_name_long,
                      model_name_short,
                      csv_name,
                      parameter,
                      nrow_read)
{
  # ------------------------------------------------------------------------------------------------
  # read header information
  output_name <- paste(strsplit(csv_name,'\\.')[[1]][2],'_activeonly_longpivot.ltab', sep = '')
  con <- file(file.path(db_path, model_name_long, output_name), 'r+b')
  head <- readBin(con, what = 'raw', n = 10) %>%
    rawToChar()
  seek(con, where = 10, origin = 'start')
  model <- readBin(con, what = 'raw', n = 10) %>%
    rawToChar()
  seek(con, where = 20, origin = 'start')
  nrec <- readBin(con, integer(), size = 8, n = 1)
  close(con)
  # ------------------------------------------------------------------------------------------------
  
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
  pctg <- paste('Reading outputs: ', model_name_short,
                '     parameter: ', parameter,
                '     row ',start_pos,
                sep = '')
  setWinProgressBar(pb,
                    0,
                    label = pctg)
  # ------------------------------------------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------
  con <- file(file.path(db_path, model_name_long, output_name), 'rb')
  record_size <- 12
  seek(con, where = 28, origin = 'start')
  
  
  results <- list()
  chunk_index <- 1
  # ------------------------------------------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------
  # continue reading chunks until the end of the file
  repeat {
    # ------------------------------------------------------------------------------------------------
    # Read a full chunk of raw data
    raw_chunk <- readBin(con, what = "raw", n = nrow_read * record_size)
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # update progress bar
    pos <- (chunk_index * nrow_read)
    pct <- min(((pos/nrec) * 100),100)
    pctg <- paste('Reading outputs: ', model_name_short,
                  '     parameter: ', parameter,
                  '     row ',pos,
                  sep = '')
    setWinProgressBar(pb,
                      round(pct, 0),
                      label = pctg)
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # break loop if no data
    if (length(raw_chunk) == 0){
      setWinProgressBar(pb,
                        100,
                        label = paste('Model: ',model_name_short,
                                      '     parameter:', parameter,
                                      '     READ = DONE',
                                      sep = ''))
      Sys.sleep(5)
      close(pb)
      break # End of file
    } 
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # null data to fill
    num_records <- length(raw_chunk) %/% record_size
    
    Date    <- numeric(num_records)
    HRU <- numeric(num_records)
    Result  <- integer(num_records)
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # reading in data according to its location
    for (i in 1:num_records) {
      offset <- (i - 1) * record_size
      
      # ------------------------------------------------------------------------------------------------
      # Extract fields using subsetting
      Date_raw    <- raw_chunk[(offset + 1):(offset + 4)]
      HRU_raw <- raw_chunk[(offset + 5):(offset + 8)]
      Result_raw  <- raw_chunk[(offset + 9):(offset + 12)]
      # ------------------------------------------------------------------------------------------------
      
      # ------------------------------------------------------------------------------------------------
      # filling null data
      Date[i]    <- readBin(Date_raw, what = "integer", size = 4, endian = "little")
      HRU[i] <- readBin(HRU_raw, what = "integer", size = 4, endian = "little")
      Result[i]  <- readBin(Result_raw, what = "integer", size = 4, endian = "little")
      # ------------------------------------------------------------------------------------------------
    }
    # ------------------------------------------------------------------------------------------------
    results[[chunk_index]] <- data.frame(Date = Date, HRU = HRU, Result = Result)
    chunk_index <- chunk_index + 1
  }
  # ------------------------------------------------------------------------------------------------
  # Combine all chunks into one data frame and return
  output <- do.call(rbind, results)
  output <- output[order(output$Date),]
  return(output)
  # ------------------------------------------------------------------------------------------------
}
# ------------------------------------------------------------------------------------------------


