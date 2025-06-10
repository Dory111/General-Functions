#===========================================================================================
# Only writes out active cells of the requested parameter by incrementing the number of read columns
# NOTE: assumes each row is a date and each column is an HRU
# function not valid for other data structures
#===========================================================================================
Write_HRU_Outputs_By_Column_Byte <- function(model_name_long,
                                             model_name_short,
                                             csv_name,
                                             parameter,
                                             ncol_read = 1000,
                                             write_mode = 'Long')
{
  write_mode <- str_to_title(write_mode) # account for lowercase
  
  # ------------------------------------------------------------------------------------------------
  if(write_mode == 'Long'){
    Long_Write(model_name_long = model_name_long,
               model_name_short = model_name_short,
               csv_name = csv_name,
               parameter = parameter,
               ncol_read = ncol_read)
  }
  # ------------------------------------------------------------------------------------------------
  
  # ------------------------------------------------------------------------------------------------
  if(write_mode == 'Wide'){
    
  }
  # ------------------------------------------------------------------------------------------------
}
# ------------------------------------------------------------------------------------------------









#===========================================================================================
# Writing a .ltab file where each row represents a triplet of (date,hru,result)
# so a file will have n_dates * nhru rows
#===========================================================================================
Long_Write <- function(model_name_long,
                       model_name_short,
                       csv_name,
                       parameter,
                       ncol_read)
{
  # ------------------------------------------------------------------------------------------------
  # starting values for the while loop
  error <- F
  ncol_read <- ncol_read
  start_pos <- 2
  ncol <- fread(file = file.path(db_path, model_name_long, csv_name),
                nrows = 1) %>%
    as.data.frame() %>% length()
  output_name <- paste(strsplit(csv_name,'\\.')[[1]][2],'_activeonly_longpivot.ltab', sep = '')
  init_file <- T
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
  # while the columns attempting to be read is less than the total number of columns
  counter <- 0
  while (error == F) {
    tryCatch({
      # ------------------------------------------------------------------------------------------------
      # Update progress bar
      pctg <- paste('Processing model: ', model_name_short,
                    '     parameter: ', parameter,
                    '     column ',start_pos,';',start_pos+ncol_read,
                    sep = '')
      setWinProgressBar(pb,
                        round(((start_pos+ncol_read)/ncol) * 100,0),
                        label = pctg)
      # ------------------------------------------------------------------------------------------------
      
      
      
      # ------------------------------------------------------------------------------------------------
      # read in the new data, rbind it to old data
      output <- fread(file = file.path(db_path, model_name_long, csv_name),
                      nrows = Inf) %>%
        as.data.frame() %>%
        dplyr::select(c(1,start_pos:(start_pos+ncol_read))) %>%
        pivot_longer(!Date, names_to="HRU_ID", values_to = "Results")%>%
        dplyr::filter(HRU_ID %in% hru_active$HRU_ID)
      start_pos <- start_pos + ncol_read + 1
      
      if(nrow(output) > 0){
        counter <- counter + nrow(output)
        data <- (integer((nrow(output)* 3))) 
        
        if(init_file == T){
          con <- file(file.path(db_path,model_name_long,output_name), "wb")
          header <- sprintf("%-10s", parameter)     # Pad to 10 chars
          model <- sprintf("%-10s", model_name_short)
          writeBin(charToRaw(header), con)
          writeBin(charToRaw(model),con)
          writeBin(as.integer(0), con, size = 8)
          
          
          data[seq(from = 1, to = length(data), by = 3)] <- as.integer(as.POSIXct(output$Date))
          data[seq(from = 2, to = length(data), by = 3)] <- as.integer(output$HRU_ID)
          data[seq(from = 3, to = length(data), by = 3)] <- as.integer(output$Results*1e4)
          writeBin(data, con, size = 4)
          
          init_file <<- F
        } else {
          data[seq(from = 1, to = length(data), by = 3)] <- as.integer(as.POSIXct(output$Date))
          data[seq(from = 2, to = length(data), by = 3)] <- as.integer(output$HRU_ID)
          data[seq(from = 3, to = length(data), by = 3)] <- as.integer(output$Results*1e4)
          writeBin(data, con, size = 4)
        }
      }
      # ------------------------------------------------------------------------------------------------
      
      # ------------------------------------------------------------------------------------------------
      # once error thrown then the function must be complete, read to last column
    }, error = function(e){
      # ------------------------------------------------------------------------------------------------
      if(start_pos != ncol){
        # ------------------------------------------------------------------------------------------------
        # read in the new data, rbind it to old data
        output <- fread(file = file.path(db_path, model_name_long, csv_name),
                        nrows = Inf) %>%
          as.data.frame() %>%
          dplyr::select(c(1,start_pos:ncol)) %>%
          pivot_longer(!Date, names_to="HRU_ID", values_to = "Results")%>%
          dplyr::filter(HRU_ID %in% hru_active$HRU_ID)
        if(nrow(output) > 0){
          counter <- counter + nrow(output)
          data <- (integer((nrow(output)* 3))) 
          if(init_file == T){
            con <- file(file.path(db_path,model_name_long,output_name), "wb")
            header <- sprintf("%-10s", parameter)     # Pad to 10 chars
            model <- sprintf("%-10s", model_name_short)
            writeBin(charToRaw(header), con)
            writeBin(charToRaw(model),con)
            writeBin(as.integer(0), con, size = 8)
            
            data[seq(from = 1, to = length(data), by = 3)] <- as.integer(as.POSIXct(output$Date))
            data[seq(from = 2, to = length(data), by = 3)] <- as.integer(output$HRU_ID)
            data[seq(from = 3, to = length(data), by = 3)] <- as.integer(output$Results*1e4)
            writeBin(data, con, size = 4)
            
            init_file <<- F
          } else {
            data[seq(from = 1, to = length(data), by = 3)] <- as.integer(as.POSIXct(output$Date))
            data[seq(from = 2, to = length(data), by = 3)] <- as.integer(output$HRU_ID)
            data[seq(from = 3, to = length(data), by = 3)] <- as.integer(output$Results*1e4)
            writeBin(data, con, size = 4)
          }
        }
        # ------------------------------------------------------------------------------------------------
        
        # ------------------------------------------------------------------------------------------------
        # in rare case where number of columns to read is a divisor of total column number
      } else {
        # ------------------------------------------------------------------------------------------------
        # read in the new data, rbind it to old data
        output <- fread(file = file.path(db_path, model_name_long, csv_name),
                        nrows = Inf) %>%
          as.data.frame() %>%
          dplyr::select(c(1,ncol)) %>%
          pivot_longer(!Date, names_to="HRU_ID", values_to = "Results")%>%
          dplyr::filter(HRU_ID %in% hru_active$HRU_ID)
        if(nrow(output) > 0){
          counter <- counter + nrow(output)
          data <- (integer((nrow(output)* 3))) 
          if(init_file == T){
            con <- file(file.path(db_path,model_name_long,output_name), "wb")
            header <- sprintf("%-10s", parameter)     # Pad to 10 chars
            model <- sprintf("%-10s", model_name_short)
            writeBin(charToRaw(header), con)
            writeBin(charToRaw(model),con)
            writeBin(as.integer(0), con, size = 8)
            
            
            data[seq(from = 1, to = length(data), by = 3)] <- as.integer(as.POSIXct(output$Date))
            data[seq(from = 2, to = length(data), by = 3)] <- as.integer(output$HRU_ID)
            data[seq(from = 3, to = length(data), by = 3)] <- as.integer(output$Results*1e4)
            writeBin(data, con, size = 4)
            
            init_file <<- F
          } else {
            data[seq(from = 1, to = length(data), by = 3)] <- as.integer(as.POSIXct(output$Date))
            data[seq(from = 2, to = length(data), by = 3)] <- as.integer(output$HRU_ID)
            data[seq(from = 3, to = length(data), by = 3)] <- as.integer(output$Results*1e4)
            writeBin(data, con, size = 4)
          }
        }
        # ------------------------------------------------------------------------------------------------
      }
      # ------------------------------------------------------------------------------------------------
      
      
      # ------------------------------------------------------------------------------------------------
      close(con)
      con <- file(file.path(db_path,model_name_long,output_name), "r+b")  # Open for read+write
      header <- sprintf("%-10s", parameter)     # Pad to 10 chars
      model <- sprintf("%-10s", model_name_short)
      writeBin(charToRaw(header), con)
      seek(con, where = 10, origin = 'start')
      writeBin(charToRaw(model),con)
      seek(con, where = 20, origin = "start")      
      writeBin(as.integer(counter), con, size = 8)
      close(con)
      # ------------------------------------------------------------------------------------------------
      
      
      # ------------------------------------------------------------------------------------------------
      # closeout
      setWinProgressBar(pb,
                        100,
                        label = paste('Model: ',model_name_short,
                                      '     parameter:', parameter,
                                      ' WRITE = DONE',
                                      sep = ''))
      Sys.sleep(5)
      close(pb)
      error <<- T
      # ------------------------------------------------------------------------------------------------
    })
    # ------------------------------------------------------------------------------------------------
  }
  # ------------------------------------------------------------------------------------------------
}
# ------------------------------------------------------------------------------------------------