#===========================================================================================
# Gets offset by a certain number of units to be used in map plotting
#===========================================================================================
Map_Text_Offset_By_Lwd <- function(lwd = 3,
                                   dev_size = dev.size("in"),
                                   usr = par("usr")) {
  
  x_units_per_in <- diff(usr[1:2]) / dev_size[1]
  y_units_per_in <- diff(usr[3:4]) / dev_size[2]
  
  inches <- lwd/72 # 72 units per lwd (as defined in R)
  
  # Average scaling for x and y
  units_per_in <- mean(c(x_units_per_in, y_units_per_in))
  
  # how many units should user offset graph
  offset <- inches * units_per_in
  return(offset)
}
# -------------------------------------------------------------------------------------