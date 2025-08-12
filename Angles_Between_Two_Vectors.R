#===========================================================================================
# Gets angle between two vectors
# input vectors must be in form (x1,x2,y1,y2)
#===========================================================================================
angles_between_two_vectors <- function(V,
                                       U,
                                       signed = TRUE){
  
  # get x and y distance travelled by vectors
  V <- c(V[2] - V[1], V[4] - V[3])
  U <- c(U[2] - U[1], U[4] - U[3])
  
  # get dot product
  dot <- sum(V * U) 
  
  #-------------------------------------------------------------------------------
  if(signed == FALSE){
    # get magnitude of vectors
    mag_U <- sqrt(sum(U^2))
    mag_V <- sqrt(sum(V^2))
    
    # Angle in radians
    theta_rad <- acos(dot/(mag_U * mag_V))
    
    # Angle in degrees
    theta_deg <- (theta_rad * 180)/pi
  }
  #-------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------
  if(signed == TRUE){
    theta_deg <- atan2(U[1]*V[2] - U[2]*V[1], dot) * 180 / pi
  }
  #-------------------------------------------------------------------------------
  return(theta_deg)
}
#-------------------------------------------------------------------------------