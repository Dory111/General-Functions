#===============================================================================
# Linearlly interpolates missing values in a vector using RLE
#===============================================================================
LERP_Vector <- function(vector)
{
  #-------------------------------------------------------------------------------
  # Vector error
  if(is.vector(vector) == FALSE){
    stop('\nLERP_Vector: DATA NOT IN FORM OF VECTOR')
  } else {}
  #-------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------
  # Numeric error
  if(is.numeric(vector) == FALSE){
    stop('\nLERP_Vector: DATA NOT IN FORM OF NUMERIC')
  } else {}
  #-------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------
  # Setting up the values and lengths
  vector <- as.vector(unlist(vector)) # enforcing vector if data frame column
  vector[is.na(vector) == TRUE] <- -9999
  rle <- rle(vector)
  values <- rle$values
  lengths <- rle$lengths
  #-------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------
  # lerp between values by checking if value to right and left of each NA is a number
  position <- 0
  for(i in 1:length(values)){
  
    
    #-------------------------------------------------------------------------------
    # are lerp conditions true
    if(i > 1 &
       i <= (length(values) - 1)){
       #-------------------------------------------------------------------------------
       if(values[i] == -9999 &
          values[i-1] != -9999 &
          values[i+1] != -9999){
          replacement_sequence <- seq(from = values[i-1],
                                      to = values[i+1],
                                      length.out = lengths[i] + 2)
          vector[position:(position+lengths[i]+1)] <- replacement_sequence
       }
      #-------------------------------------------------------------------------------
    } 
    #-------------------------------------------------------------------------------
    position <- position + lengths[i] 
  }
  #-------------------------------------------------------------------------------
  vector[vector == -9999] <- NA
  return(vector)
}
#-------------------------------------------------------------------------------