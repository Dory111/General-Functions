#===============================================================================
# This function takes in a series of hexadecimal or base r color specifications and
# gives them alpha values
#===============================================================================
Hex_to_RGBA <- function(hex,
                        Alpha = 1)
{
  
  #-------------------------------------------------------------------------------
  # Changing to decimal colors
  hex <- col2rgb(hex)
  hex <- as.data.frame(hex)
  hex <- hex/255
  #-------------------------------------------------------------------------------
  
  
  #-------------------------------------------------------------------------------
  # if alpha is the same dimensions as the hex data frame
  # aka 2 alphas are specified for 2 colors then just rbind it
  # if the rbind cant work because of dimensionality repeat whatever alpha is specified
  # up to the n colors
  tryCatch(expr = {
    
    hex <- rbind(hex, Alpha)
    
  }, 
  error = function(e) {
    
    Alpha <- rep_len(Alpha, ncol(hex))
    hex <- rbind(hex, Alpha)
    
  },
  warning = function(w) {
    
    Alpha <- rep_len(Alpha, ncol(hex))
    hex <- rbind(hex, Alpha)
    
  })
  #-------------------------------------------------------------------------------
  
  
  
  #-------------------------------------------------------------------------------
  # Returning colors as hex
  color_list <- c()
  for(col in 1:ncol(hex))
  {
    
    color_list <- append(color_list,
                         rgb(hex[1,col],
                             hex[2,col],
                             hex[3,col],
                             hex[4,col]))
    
  }
  #-------------------------------------------------------------------------------
  return(color_list)
}
#-------------------------------------------------------------------------------
