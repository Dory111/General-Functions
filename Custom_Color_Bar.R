Custom_Color_Bar <- function(colors,
                             xleft = par('usr')[1],
                             xright = par('usr')[1] * 0.2,
                             ybot = par('usr')[3],
                             ytop = par('usr')[4] * 0.8,
                             line_length = (par('usr')[4] - par('usr')[3]) * 0.1,
                             labels_TF = F,
                             labels_at = NULL,
                             labels_text = NULL)
{
  #-------------------------------------------------------------------------------
  # create matrix of the coordinates for the rectangles that constitute the color bar
  color_mat <- matrix(nrow = length(colors),
                      ncol = 6)
  color_mat <- as.data.frame(color_mat)
  colnames(color_mat) <- c('xleft','ybottom','xright','ytop','value','color')
  color_mat$xleft <- rep(xleft,nrow(color_mat))
  color_mat$xright <- rep(xright,nrow(color_mat))
  #-------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------
  # as the bars are horizontal the ybottom and ytop need to be incremented
  # while the xleft and xright are constant
  y_seq <- seq(from = ybot, to = ytop, length.out = length(colors))
  color_mat$ybottom <- seq(from = y_seq[1], to = tail(y_seq,2)[1], length.out = length(colors))
  color_mat$ytop <- seq(from = y_seq[2], to = tail(y_seq,1), length.out = length(colors))
  #-------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------
  color_mat$value <- seq(from = -1, to = 1, length.out = length(colors))
  color_mat$color <- colors
  y_range <- ytop-ybot
  #-------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------
  # draw the rectangles of the color bar
  for(j in 1:nrow(color_mat)){
    #-------------------------------------------------------------------------------
    # compensates that the first label in the bar might be drawn below where the
    # inner portion of the rectangle is by setting the first border of the color bar
    # to the first color while the other rectangles that make up the bar have their border
    # color set to NA
    if(j == 1){
      rect(color_mat$xleft[j],
           color_mat$ybottom[j],
           color_mat$xright[j],
           color_mat$ytop[j],
           col = color_mat$color[j],
           border = NA)
      lines(x = c(xright, xright + line_length),
            y = c(color_mat$ybottom[j],color_mat$ybottom[j]))
    } else {
      rect(color_mat$xleft[j],
           color_mat$ybottom[j],
           color_mat$xright[j],
           color_mat$ytop[j],
           col = color_mat$color[j],
           border = NA)
    }
    #-------------------------------------------------------------------------------
  }
  #-------------------------------------------------------------------------------
  
  #-------------------------------------------------------------------------------
  # create extra row for the last label to be drawn with
  # compensates for the fact that in any color bar there will be a different
  # number of colors and breaks necessarily
  new_row <- c(xleft,
               tail(color_mat$ytop,1),
               xright,
               tail(color_mat$ytop,1),
               NA,
               NA)
  color_mat <- rbind(color_mat,new_row)
  #-------------------------------------------------------------------------------
  

  #-------------------------------------------------------------------------------
  # should labels be plotted on the color bar
  if(labels_TF == TRUE){
    #-------------------------------------------------------------------------------
    # if user did not enter labels then set them to the break values
    if(is.null(labels_text) == TRUE){
      labels_text <- labels_at
    }
    #-------------------------------------------------------------------------------
    
    #-------------------------------------------------------------------------------
    # draw labels
    for(j in 1:length(labels_at)){
        lines(x = c(xright, xright + line_length),
              y = c(color_mat$ybottom[j],color_mat$ybottom[j]))
        text(x = xright + line_length,
             y = color_mat$ybottom[j], 
             pos = 4,
             labels = labels_text[j],
             cex = 1.4)
    }
    #-------------------------------------------------------------------------------
  }
  #-------------------------------------------------------------------------------
}
#-------------------------------------------------------------------------------