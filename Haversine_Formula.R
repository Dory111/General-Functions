# creates km distance from latlon coordinates
Haversine_Formula <- function(LatA,LonA,
                              LatB,LonB,
                              Re = 6371)
{
  LatA <- LatA * (3.14159/180)
  LonA <- LonA * (3.14159/180)
  LatB <- LatB * (3.14159/180)
  LonB <- LonB * (3.14159/180)
  
  dlat <- LatB - LatA
  dlon <- LonB - LonA
  
  a <- sin(dlat / 2)^2 + cos(LatA) * cos(LatB) * sin(dlon / 2)^2
  c <- 2 * atan2(sqrt(a),sqrt(1-a))
  
  d <- Re * c
  return(d)
}