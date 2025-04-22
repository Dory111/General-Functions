#===========================================================================================
# must be the geometry itself and not the entire sf object
#===========================================================================================
Extract_SF_Linestring_Vertices <- function(geometry)
{
  
  latitude <- geometry[1][[1]]
  latitude <- as.matrix(latitude)[,2]
  
  
  longitude <- geometry[1][[1]]
  longitude <- as.matrix(longitude)[,1]
  
  
  return(list(latitude,
              longitude))
  
  
}
#-------------------------------------------------------------------------------