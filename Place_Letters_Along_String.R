############################################################################################
#################################### MAIN FUNCTION #########################################
############################################################################################
#===========================================================================================
# places letters along irregular string
#===========================================================================================
Place_Letters_Along_String <- function(offset = 0.0001,
                                       verts,
                                       letters = 'Test',
                                       user_supplied_crs = 4326,
                                       dist_norm = 1000,
                                       initial_point_skip = 4,
                                       between_point_skip = 0,
                                       add_space_between_words = TRUE,
                                       repeat_name_n = 1,
                                       read_direction = 'left-right',
                                       text_position = 'above')
{
  # -------------------------------------------------------------------------------------
  # to be filled with data
  letter_vertices <- list(c(), # y
                          c(), # x
                          c(), # char
                          c()) # angle
  # -------------------------------------------------------------------------------------
  
  # -------------------------------------------------------------------------------------
  # adaquate space between words
  if(add_space_between_words == TRUE){
    # -------------------------------------------------------------------------------------
    split_letters <- strsplit(letters, '')[[1]]
    if(split_letters[length(split_letters)] == ''|
       split_letters[length(split_letters)] == ' '){
      split_letters <- split_letters[-c(length(split_letters))]
    }
    
    splits <- which(split_letters == ' ')
    new <- list()
    counter <- 1
    if(length(splits) > 0){
      for(i in 1:length(splits)){
        
        new[[i]] <- paste0(paste(split_letters[counter:splits[i]],collapse = ''),
                           '')
        counter <- counter + length(split_letters[counter:splits[i]])
        
        if(i == length(splits)){
          new[[i+1]] <- paste0(split_letters[(counter):length(split_letters)], collapse = '')
        }
      }
    }
    letters <- do.call(paste,new)
    # -------------------------------------------------------------------------------------
  }
  # -------------------------------------------------------------------------------------
  
  # -------------------------------------------------------------------------------------
  # error
  if((nchar(letters) + initial_point_skip + between_point_skip*nchar(letters)) > length(verts[[1]])){
    stop(paste('LABELS_ALONG_STRING:\n\n',
               'Number of points supplied is less than the number of characters\n\n',
               'and the supplied number of skipped points'))
  }
  # -------------------------------------------------------------------------------------

  # -------------------------------------------------------------------------------------
  # repeat along length of string
  repeat_counter <- 0
  letter_counter <- 0
  while(repeat_counter < repeat_name_n){
    # -------------------------------------------------------------------------------------
    # if theres enough room to accomodate letters
    if((nchar(letters) + between_point_skip) <= length(verts[[1]][letter_counter:length(verts[[1]])])){
      letter_counter <- letter_counter 
      # -------------------------------------------------------------------------------------
      for(i in 1:nchar(letters)){
        # -------------------------------------------------------------------------------------
        if(i == 1){
          letter_counter <- letter_counter + 1
        } else {
          letter_counter <- letter_counter + 1 + between_point_skip
        }
        char <- substring(letters,
                          i,
                          i)
        # -------------------------------------------------------------------------------------
  
        # -------------------------------------------------------------------------------------
        # coords of stream points
        x1 <- verts[[2]][initial_point_skip + letter_counter]
        x2 <- verts[[2]][initial_point_skip + letter_counter + 1]
        y1 <- verts[[1]][initial_point_skip + letter_counter]
        y2 <- verts[[1]][initial_point_skip + letter_counter + 1]
        # -------------------------------------------------------------------------------------
  
        # -------------------------------------------------------------------------------------
        # coords of horizontal comparison line
        x3 <- verts[[2]][initial_point_skip + letter_counter]
        x4 <- verts[[2]][initial_point_skip + letter_counter + 1]
        y3 <- verts[[1]][initial_point_skip + letter_counter]
        y4 <- verts[[1]][initial_point_skip + letter_counter]
        # -------------------------------------------------------------------------------------
  
        # -------------------------------------------------------------------------------------
        angle_unsigned <- angles_between_two_vectors(V = c(x1,x2,y1,y2),
                                                     U = c(x3,x4+1,y3,y4),
                                                     signed = FALSE)
  
        angle_signed <- angles_between_two_vectors(V = c(x1,x2,y1,y2),
                                                   U = c(x3,x4+1,y3,y4),
                                                   signed = TRUE)
        # -------------------------------------------------------------------------------------
  
  
        ########################################################################################
        ##################################### IS ANGLE VALID ###################################
        ########################################################################################
        # -------------------------------------------------------------------------------------
        # is angle valid? (point 1 != point 2 etc)
        if(is.nan(angle_unsigned) == FALSE){
  
          # -------------------------------------------------------------------------------------
          # append character and character rotation angle
          letter_vertices[[3]] <- append(letter_vertices[[3]],
                                         char)
          letter_vertices[[4]] <- append(letter_vertices[[4]],
                                         angle_signed)
          # -------------------------------------------------------------------------------------
  
          # -------------------------------------------------------------------------------------
          # USER WANTS TEXT ABOVE
          if(text_position == 'above'){
            output <- Place_Text_Above(x1 = x1,
                                       y1 = y1,
                                       x2 = x2,
                                       y2 = y2,
                                       offset = offset,
                                       angle_signed = angle_signed)
            y3 <- output[[1]]
            x3 <- output[[2]]
          }
          # -------------------------------------------------------------------------------------
  
  
          # -------------------------------------------------------------------------------------
          # USER WANTS TEXT BELOW
          if(text_position == 'below'){
            output <- Place_Text_Below(x1 = x1,
                                       y1 = y1,
                                       x2 = x2,
                                       y2 = y2,
                                       offset = offset,
                                       angle_signed = angle_signed)
            y3 <- output[[1]]
            x3 <- output[[2]]
          }
          # -------------------------------------------------------------------------------------
  
          # -------------------------------------------------------------------------------------
          # USER WANTS TEXT CENTERED
          if(text_position == 'center'){
            y3 <- ((y1+y2)/2)
            x3 <- ((x1+x2)/2)
          }
          # -------------------------------------------------------------------------------------
  
          # -------------------------------------------------------------------------------------
          # Append updated coordinates
          letter_vertices[[1]] <- append(letter_vertices[[1]],
                                         y3)
          letter_vertices[[2]] <- append(letter_vertices[[2]],
                                         x3)
          # -------------------------------------------------------------------------------------
        }
        # -------------------------------------------------------------------------------------
      }
      # -------------------------------------------------------------------------------------
    }
    # -------------------------------------------------------------------------------------
    repeat_counter <- repeat_counter + 1
  }
  # -------------------------------------------------------------------------------------

  # -------------------------------------------------------------------------------------
  # restructure points to attempt even spacing
  output <- Remove_Linestring_Points_By_Proximity(points_list = list(letter_vertices[[1]],
                                                                     letter_vertices[[2]]),
                                                  user_supplied_crs = user_supplied_crs,
                                                  dist_norm = dist_norm+1)
  # -------------------------------------------------------------------------------------
  
  # -------------------------------------------------------------------------------------
  # return to user
  output <- do.call(cbind, letter_vertices) %>%
    as.data.frame()
  colnames(output) <- c('Y','X','CHAR','SRT')
  output$X <- as.numeric(output$X)
  output$Y <- as.numeric(output$Y)
  return(output)
  # -------------------------------------------------------------------------------------
}
# -------------------------------------------------------------------------------------



############################################################################################
#################################### HELPER FUNCTIONS ######################################
############################################################################################
Place_Text_Above <- function(x1,
                             y1,
                             x2,
                             y2,
                             offset,
                             angle_signed)
{
  # -------------------------------------------------------------------------------------
  # Initial variables
  text_position_valid <- FALSE
  line_between_stream_points <- c(x1,x2,y1,y2)
  xmod <- 1
  ymod <- 1
  initial_try_x2 <- ((x1+x2)/2) + abs(offset)*xmod*abs(sin(Deg_To_Rad(angle_signed)))
  initial_try_y2 <- ((y1+y2)/2) + abs(offset)*ymod*abs(cos(Deg_To_Rad(angle_signed)))
  # -------------------------------------------------------------------------------------
  
  # -------------------------------------------------------------------------------------
  # if angle is positive then text will be placed above
  line_to_offset <- c(x1,
                      initial_try_x2,
                      y1,
                      initial_try_y2)
  text_angle <- angles_between_two_vectors(V = line_to_offset,
                                           U = line_between_stream_points,
                                           signed = TRUE)
  if(text_angle > 0){
    x3 <- initial_try_x2
    y3 <- initial_try_y2
    text_position_valid <- TRUE
  }
  # -------------------------------------------------------------------------------------
  
  while(text_position_valid == FALSE){
    
    # -------------------------------------------------------------------------------------
    # try flipping quadrant in x direction (+, -)
    xmod <- xmod * -1
    first_try_x2 <- ((x1+x2)/2) + abs(offset)*xmod*abs(sin(Deg_To_Rad(angle_signed)))
    first_try_y2 <- ((y1+y2)/2) + abs(offset)*ymod*abs(cos(Deg_To_Rad(angle_signed)))
    # -------------------------------------------------------------------------------------
    
    
    # -------------------------------------------------------------------------------------
    # if angle is positive then text will be placed above and loop breaks
    line_to_offset <- c(x1,
                        first_try_x2,
                        y1,
                        first_try_y2)
    text_angle <- angles_between_two_vectors(V = line_to_offset,
                                             U = line_between_stream_points,
                                             signed = TRUE)
    if(text_angle > 0){
      x3 <- first_try_x2
      y3 <- first_try_y2
      text_position_valid <- TRUE
    }
    # -------------------------------------------------------------------------------------
    
    
    
    
    
    
    
    # -------------------------------------------------------------------------------------
    # try flipping quadrant in y direction (-, +)
    xmod <- xmod * -1 # reset to previous value (-1*-1 = 1)
    ymod <- ymod * -1
    second_try_x2 <- ((x1+x2)/2) + abs(offset)*xmod*abs(sin(Deg_To_Rad(angle_signed)))
    second_try_y2 <- ((y1+y2)/2) + abs(offset)*ymod*abs(cos(Deg_To_Rad(angle_signed)))
    # -------------------------------------------------------------------------------------
    
    
    # -------------------------------------------------------------------------------------
    # if angle is positive then text will be placed above and loop breaks
    line_to_offset <- c(x1,
                        second_try_x2,
                        y1,
                        second_try_y2)
    text_angle <- angles_between_two_vectors(V = line_to_offset,
                                             U = line_between_stream_points,
                                             signed = TRUE)
    if(text_angle > 0){
      x3 <- second_try_x2
      y3 <- second_try_y2
      text_position_valid <- TRUE
    }
    # -------------------------------------------------------------------------------------
    
    
    
    
    
    
    # -------------------------------------------------------------------------------------
    # try flipping quadrant in y direction (-, -)
    xmod <- xmod * -1 # reset to -1
    third_try_x2 <- ((x1+x2)/2) + abs(offset)*xmod*abs(sin(Deg_To_Rad(angle_signed)))
    third_try_y2 <- ((y1+y2)/2) + abs(offset)*ymod*abs(cos(Deg_To_Rad(angle_signed)))
    # -------------------------------------------------------------------------------------
    
    
    # -------------------------------------------------------------------------------------
    # if angle is positive then text will be placed above and loop breaks
    line_to_offset <- c(x1,
                        second_try_x2,
                        y1,
                        second_try_y2)
    text_angle <- angles_between_two_vectors(V = line_to_offset,
                                             U = line_between_stream_points,
                                             signed = TRUE)
    if(text_angle > 0){
      x3 <- third_try_x2
      y3 <- third_try_y2
      text_position_valid <- TRUE
    }
    # -------------------------------------------------------------------------------------
  }
  # -------------------------------------------------------------------------------------
  return(list(y3,x3))
}
# -------------------------------------------------------------------------------------







############################################################################################
#################################### HELPER FUNCTIONS ######################################
############################################################################################
Place_Text_Below <- function(x1,
                             y1,
                             x2,
                             y2,
                             offset,
                             angle_signed)
{
  # -------------------------------------------------------------------------------------
  # Initial variables
  text_position_valid <- FALSE
  line_between_stream_points <- c(x1,x2,y1,y2)
  xmod <- 1
  ymod <- 1
  initial_try_x2 <- ((x1+x2)/2) + abs(offset)*xmod*abs(sin(Deg_To_Rad(angle_signed)))
  initial_try_y2 <- ((y1+y2)/2) + abs(offset)*ymod*abs(cos(Deg_To_Rad(angle_signed)))
  # -------------------------------------------------------------------------------------
  
  # -------------------------------------------------------------------------------------
  # if angle is positive then text will be placed above
  line_to_offset <- c(x1,
                      initial_try_x2,
                      y1,
                      initial_try_y2)
  text_angle <- angles_between_two_vectors(V = line_to_offset,
                                           U = line_between_stream_points,
                                           signed = TRUE)
  if(text_angle < 0){
    x3 <- initial_try_x2
    y3 <- initial_try_y2
    text_position_valid <- TRUE
  }
  # -------------------------------------------------------------------------------------
  
  while(text_position_valid == FALSE){
    
    # -------------------------------------------------------------------------------------
    # try flipping quadrant in x direction (+, -)
    xmod <- xmod * -1
    first_try_x2 <- ((x1+x2)/2) + abs(offset)*xmod*abs(sin(Deg_To_Rad(angle_signed)))
    first_try_y2 <- ((y1+y2)/2) + abs(offset)*ymod*abs(cos(Deg_To_Rad(angle_signed)))
    # -------------------------------------------------------------------------------------
    
    
    # -------------------------------------------------------------------------------------
    # if angle is positive then text will be placed above and loop breaks
    line_to_offset <- c(x1,
                        first_try_x2,
                        y1,
                        first_try_y2)
    text_angle <- angles_between_two_vectors(V = line_to_offset,
                                             U = line_between_stream_points,
                                             signed = TRUE)
    if(text_angle < 0){
      x3 <- first_try_x2
      y3 <- first_try_y2
      text_position_valid <- TRUE
    }
    # -------------------------------------------------------------------------------------
    
    
    
    
    
    
    
    # -------------------------------------------------------------------------------------
    # try flipping quadrant in y direction (-, +)
    xmod <- xmod * -1 # reset to previous value (-1*-1 = 1)
    ymod <- ymod * -1
    second_try_x2 <- ((x1+x2)/2) + abs(offset)*xmod*abs(sin(Deg_To_Rad(angle_signed)))
    second_try_y2 <- ((y1+y2)/2) + abs(offset)*ymod*abs(cos(Deg_To_Rad(angle_signed)))
    # -------------------------------------------------------------------------------------
    
    
    # -------------------------------------------------------------------------------------
    # if angle is positive then text will be placed above and loop breaks
    line_to_offset <- c(x1,
                        second_try_x2,
                        y1,
                        second_try_y2)
    text_angle <- angles_between_two_vectors(V = line_to_offset,
                                             U = line_between_stream_points,
                                             signed = TRUE)
    if(text_angle < 0){
      x3 <- second_try_x2
      y3 <- second_try_y2
      text_position_valid <- TRUE
    }
    # -------------------------------------------------------------------------------------
    
    
    
    
    
    
    # -------------------------------------------------------------------------------------
    # try flipping quadrant in y direction (-, -)
    xmod <- xmod * -1 # reset to -1
    third_try_x2 <- ((x1+x2)/2) + abs(offset)*xmod*abs(sin(Deg_To_Rad(angle_signed)))
    third_try_y2 <- ((y1+y2)/2) + abs(offset)*ymod*abs(cos(Deg_To_Rad(angle_signed)))
    # -------------------------------------------------------------------------------------
    
    
    # -------------------------------------------------------------------------------------
    # if angle is positive then text will be placed above and loop breaks
    line_to_offset <- c(x1,
                        second_try_x2,
                        y1,
                        second_try_y2)
    text_angle <- angles_between_two_vectors(V = line_to_offset,
                                             U = line_between_stream_points,
                                             signed = TRUE)
    if(text_angle < 0){
      x3 <- third_try_x2
      y3 <- third_try_y2
      text_position_valid <- TRUE
    }
    # -------------------------------------------------------------------------------------
  }
  # -------------------------------------------------------------------------------------
  return(list(y3,x3))
}
# -------------------------------------------------------------------------------------