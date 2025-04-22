#===========================================================================================
# must be the geometry itself and not the entire sf object
#===========================================================================================
Extract_SF_MultiPolygon_Vertices <- function(geometry,lay)
{

  geometry <- geometry[1][[1]][1]

  longitude <- geometry[[1]][[lay]][,1]
  
  latitude <- geometry[[1]][[lay]][,2]
  
  
  return(list(latitude,
              longitude))
}
#-------------------------------------------------------------------------------





#===========================================================================================
# must be the geometry itself and not the entire sf object
#===========================================================================================
Extract_SF_MultiPolygon_Layers <- function(geometry)
{
  
  geometry <- b118$geometry
  geometry <- geometry[1][[1]][1]
  nlay <- length(geometry[[1]])
  
  
  return(list(nlay))
  
  
}
#-------------------------------------------------------------------------------