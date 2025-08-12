#===========================================================================================
# Takes vertices of list(y,x) and removes all points between
# dist_norm and dist_norm*1.5
# If points farther apart than dist_norm*1.5 it will add
# points between them artificially by linear interpolation
#===========================================================================================
Remove_Linestring_Points_By_Proximity <- function(user_supplied_crs = 4326,
                                                  dist_norm = 1000,
                                                  points_list)
{
  verts <- points_list
  
  new_x <- verts[[2]]
  new_y <- verts[[1]]
  counter <- 1
  # -------------------------------------------------------------------------------------
  # while length not exceeded
  while(counter <= (length(new_x)-1)){
    no_action_add <- 0

    # -------------------------------------------------------------------------------------
    # calculate distance between current and subsequent point
    obj <- st_point(c(new_x[counter],
                      new_y[counter]))  %>% st_sfc(crs = user_supplied_crs)
    obj2 <- st_point(c(new_x[counter+1],
                       new_y[counter+1])) %>% st_sfc(crs = user_supplied_crs)
    dist_orig <- st_distance(obj,obj2) %>%
      as.numeric()
    # -------------------------------------------------------------------------------------
    
    # -------------------------------------------------------------------------------------
    # if no points could be interpolated bewteen or removed
    if(dist_orig > dist_norm &
       dist_orig < (dist_norm*1.1) &
       is.na(dist_orig) == FALSE){
      
      no_action_add <- 1
    }
    # -------------------------------------------------------------------------------------
    
    if(is.na(dist_orig) == FALSE){
      # -------------------------------------------------------------------------------------
      # if the distance between the two points is too small
      if(dist_orig < dist_norm){
        #print('smaller')
        dist_new <- dist_orig
        
        # -------------------------------------------------------------------------------------
        # continue to remove points between them until it isnt
        while(dist_new < dist_norm){
          
          # -------------------------------------------------------------------------------------
          # remove a point
          new_x <- new_x[-c(counter+1)]
          new_y <- new_y[-c(counter+1)]
          # -------------------------------------------------------------------------------------
          
          # -------------------------------------------------------------------------------------
          # continue process until either end of coordinates reached or
          # distance criteria is satisfied
          if(counter <= (length(new_x) - 1)){
            obj <- st_point(c(new_x[counter],
                              new_y[counter]))  %>% st_sfc(crs = user_supplied_crs)
            obj2 <- st_point(c(new_x[counter+1],
                               new_y[counter+1])) %>% st_sfc(crs = user_supplied_crs)
            dist_new <- st_distance(obj,obj2) %>%
              as.numeric()
          } else {
            
            dist_new <- dist_norm + 1
            counter <- length(new_x)
            
          }
          # -------------------------------------------------------------------------------------
        }
        # -------------------------------------------------------------------------------------
      } 
      # -------------------------------------------------------------------------------------
      
      # -------------------------------------------------------------------------------------
      # double check length not being exceeded
      if(counter <= (length(new_x) - 1)){
        # -------------------------------------------------------------------------------------
        # calculate distance between current and subsequent point
        # if points have been removed this will be updated
        # if no points have been removed then this will remain the same
        obj <- st_point(c(new_x[counter],
                          new_y[counter]))  %>% st_sfc(crs = user_supplied_crs)
        obj2 <- st_point(c(new_x[counter+1],
                           new_y[counter+1])) %>% st_sfc(crs = user_supplied_crs)
        dist_orig <- st_distance(obj,obj2) %>%
          as.numeric()
        # -------------------------------------------------------------------------------------
        
        # -------------------------------------------------------------------------------------
        # if points are now too far apart after removal
        if(dist_orig >= dist_norm*1.1){
          
          n <- ceiling(dist_orig/dist_norm)
          
          if(n == 2){
            x_seq <- new_x[counter] + (diff(c(new_x[counter], new_x[counter+1])) * (dist_norm/dist_orig))
            y_seq <- new_y[counter] + (diff(c(new_y[counter], new_y[counter+1])) * (dist_norm/dist_orig))
            
            new_x <- c(new_x[1:counter],
                       x_seq,
                       new_x[(counter+1):length(new_x)])
            new_y <- c(new_y[1:counter],
                       y_seq,
                       new_y[(counter+1):length(new_y)])
            counter <- counter + length(x_seq)
            # x_seq <- (new_x[counter] + new_x[counter+1])/2
            # y_seq <- (new_y[counter] + new_y[counter+1])/2
          } else if (n >= 3){
            # -------------------------------------------------------------------------------------
            # sequences of greater than three points
            x_seq <- seq(from = new_x[counter],
                         to = new_x[counter+1],
                         length.out = n)
            y_seq <- seq(from = new_y[counter],
                         to = new_y[counter+1],
                         length.out = n)
            # -------------------------------------------------------------------------------------
            
            # -------------------------------------------------------------------------------------
            # only keep the in between points
            x_seq <- x_seq[-c(1,length(x_seq))]
            y_seq <- y_seq[-c(1,length(y_seq))]
            # -------------------------------------------------------------------------------------
            
            new_x <- c(new_x[1:counter],
                       x_seq,
                       new_x[(counter+1):length(new_x)])
            new_y <- c(new_y[1:counter],
                       y_seq,
                       new_y[(counter+1):length(new_y)])
            counter <- counter + length(x_seq) + 1
          }
          
        }
        # -------------------------------------------------------------------------------------
      }
      # -------------------------------------------------------------------------------------
      counter <- counter + no_action_add
    } else {
      counter <- length(new_x)
    }
  }
  # -------------------------------------------------------------------------------------
  return(list(new_y,
              new_x))
}
# -------------------------------------------------------------------------------------