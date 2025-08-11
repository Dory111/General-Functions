#===========================================================================================
# places letters along irregular string
#===========================================================================================
Place_Letters_Along_String <- function(offset = 1,
                                       verts,
                                       letters = 'Test',
                                       initial_point_skip = 4,
                                       between_point_skip = 1,
                                       repeat_name_n = 1)
{
  # -------------------------------------------------------------------------------------
  # to be filled with data
  letter_vertices <- list(c(), # y
                          c(), # x
                          c(), # char
                          c()) # angle
  # -------------------------------------------------------------------------------------
  
  # -------------------------------------------------------------------------------------
  # error
  if(nchar(letters) > (length(verts[[1]]) + initial_point_skip)){
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
      letter_counter <- letter_counter + between_point_skip
      # -------------------------------------------------------------------------------------
      for(i in 1:nchar(letters)){
        # -------------------------------------------------------------------------------------
        letter_counter <- letter_counter + 1
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
        
        # -------------------------------------------------------------------------------------
        # is angle valid? (point 1 != point 2 etc)
        if(is.nan(angle) == FALSE){
          
          letter_vertices[[3]] <- append(letter_vertices[[3]],
                                         char)
          letter_vertices[[4]] <- append(letter_vertices[[4]],
                                         angle_signed)
          
          # -------------------------------------------------------------------------------------
          # quadrant 1
          if(x2 > x1 &
             y2 > y1){
            letter_vertices[[1]] <- append(letter_vertices[[1]],
                                           ((y1+y2)/2) + abs(offset)*abs(cos(Deg_To_Rad(angle_signed))))
            letter_vertices[[2]] <- append(letter_vertices[[2]],
                                           ((x1+x2)/2) + abs(offset)*abs(sin(Deg_To_Rad(angle_signed))))
          }
          # -------------------------------------------------------------------------------------
          
          # -------------------------------------------------------------------------------------
          # quadrant 2
          if(x2 < x1 &
             y2 > y1){
            letter_vertices[[1]] <- append(letter_vertices[[1]],
                                           ((y1+y2)/2) + abs(offset)*abs(cos(Deg_To_Rad(angle_signed))))
            letter_vertices[[2]] <- append(letter_vertices[[2]],
                                           ((x1+x2)/2) + abs(offset)*abs(sin(Deg_To_Rad(angle_signed))))
          }
          # -------------------------------------------------------------------------------------
          
          # -------------------------------------------------------------------------------------
          # quadrant 3
          if(x2 < x1 &
             y2 < y1){
            letter_vertices[[1]] <- append(letter_vertices[[1]],
                                           ((y1+y2)/2) - abs(offset)*abs(cos(Deg_To_Rad(angle_signed))))
            letter_vertices[[2]] <- append(letter_vertices[[2]],
                                           ((x1+x2)/2) + abs(offset)*abs(sin(Deg_To_Rad(angle_signed))))
          }
          # -------------------------------------------------------------------------------------
          
          # -------------------------------------------------------------------------------------
          # quadrant 4
          if(x2 > x1 &
             y2 < y1){
            letter_vertices[[1]] <- append(letter_vertices[[1]],
                                           ((y1+y2)/2) + abs(offset)*abs(cos(Deg_To_Rad(angle_signed))))
            letter_vertices[[2]] <- append(letter_vertices[[2]],
                                           ((x1+x2)/2) + abs(offset)*abs(sin(Deg_To_Rad(angle_signed))))
          }
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