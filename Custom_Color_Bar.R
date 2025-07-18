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
  m <- matrix(nrow = length(colors),
              ncol = 6)
  m <- as.data.frame(m)
  colnames(m) <- c('xleft','ybottom','xright','ytop','value','color')
  m$xleft <- rep(xleft,nrow(m))
  m$xright <- rep(xright,nrow(m))
  
  
  y_seq <- seq(from = ybot, to = ytop, length.out = length(colors))
  m$ybottom <- seq(from = y_seq[1], to = tail(y_seq,2)[1], length.out = length(colors))
  m$ytop <- seq(from = y_seq[2], to = tail(y_seq,1), length.out = length(colors))
  new_row <- c(xleft,
               tail(m$ytop,1),
               xright,
               tail(m$ytop,1),
               NA,
               NA)
  
  m$value <- seq(from = -1, to = 1, length.out = length(colors))
  m$color <- colors
  y_range <- ytop-ybot
  #-------------------------------------------------------------------------------


  #-------------------------------------------------------------------------------
  for(j in 1:nrow(m)){
    rect(m$xleft[j],
         m$ybottom[j],
         m$xright[j],
         m$ytop[j],
         col = m$color[j],
         border = NA)
  }
  #-------------------------------------------------------------------------------
  
  m <- rbind(m,new_row)
  #-------------------------------------------------------------------------------
  if(labels_TF == T){
    if(is.null(labels_text) == T){
      labels_text <- labels_at
    }
    #-------------------------------------------------------------------------------
    for(j in 1:length(labels_at)){
      if(j == 1){
        lines(x = c(xright, xright + line_length),
              y = c(m$ybottom[j] + y_range*0.001,m$ybottom[j]+ y_range*0.001))
        text(x = xright + line_length,
             y = m$ybottom[j], 
             pos = 4,
             labels = labels_text[j],
             cex = 1.4)
      } else {
        lines(x = c(xright, xright + line_length),
              y = c(m$ybottom[j],m$ybottom[j]))
        text(x = xright + line_length,
             y = m$ybottom[j], 
             pos = 4,
             labels = labels_text[j],
             cex = 1.4)
      }
      #-------------------------------------------------------------------------------
    }
    #-------------------------------------------------------------------------------
  }
  #-------------------------------------------------------------------------------
}
#-------------------------------------------------------------------------------