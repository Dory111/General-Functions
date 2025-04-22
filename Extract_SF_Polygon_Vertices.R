#===========================================================================================
# must be the geometry itself and not the entire sf object
#===========================================================================================
Extract_SF_Polygon_Vertices <- function(geometry)
{

  latitude <- geometry[1][[1]][1]
  latitude <- latitude[[1]][,2]
  
  
  longitude <- geometry[1][[1]][1]
  longitude <- longitude[[1]][,1]
  
  
  return(list(latitude,
              longitude))
}
#-------------------------------------------------------------------------------