#===============================================================================
# Returns data frame of values, rankings, and exceedance probabilities from
# a vector of numeric values
#===============================================================================
Exceedance_Probabilities <- function(values,
                                     na.rm = F,
                                     decreasing = TRUE)
{
  #-------------------------------------------------------------------------------
  # Vector error
  if(is.vector(values) == FALSE){
    stop('\nExceedance_Probabilities: DATA NOT IN FORM OF VECTOR')
  } else {}
  #-------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------
  # Numeric error
  if(is.numeric(values) == FALSE){
    stop('\nExceedance_Probabilities: DATA NOT IN FORM OF NUMERIC')
  } else {}
  #-------------------------------------------------------------------------------

  #-------------------------------------------------------------------------------
  values <- as.vector(unlist(values)) # coercing in case data frame column
  rank <- 1:length(values)
  order <- 1:length(values)
  DF <- data.frame(Values = values, Order = order) # assign values rank based on their position
  DF <- DF[order(DF$Values, decreasing = decreasing), ] # order values
  DF$Rank <- 1:length(values)
  DF$Prob <- 100* DF$Rank/(length(DF$Rank) + 1) # classical exceedance probability formula
  if(na.rm == T){
    DF <- DF[-c(which(is.na(DF$Values) == TRUE)), ] # remove NA
  }
  #-------------------------------------------------------------------------------
  return(DF)
}
#-------------------------------------------------------------------------------