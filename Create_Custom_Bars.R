#===============================================================================
# Creates both stacked and non-stacked barcharts of various styles
# barstyle 1 = solid
# barstyle 2 = striped bars
# barstyle 3 = striped with lines
#===============================================================================
Create_Custom_Bars <- function(x = c(),
                               y = c(),
                               barwidth = 1,
                               corners = par('usr'),
                               barstyle = 1,
                               barcolor = 'black',
                               stripecolor = 'red',
                               stripespacing = 0.2,
                               stripewidth = 0.2,
                               stripedir = 'forward',
                               stripeborder = 'red',
                               stripestart = 'odd',
                               linecolor = 'red',
                               linestyle = 1,
                               linewidth = 1,
                               linespacing = 0.1,
                               linedir = 'forward',
                               demo = FALSE)
{
  
  
  # ------------------------------------------------------------------------------
  # Making sure the width is the same dimension
  barwidth <- rep_len(barwidth,length(x))
  # ------------------------------------------------------------------------------
  
  
  
  # ------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------
  # DRAWING BARS; DRAWING BARS; DRAWING BARS; DRAWING BARS; DRAWING BARS
  # ------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------
  
  # ------------------------------------------------------------------------------
  # drawing the bars
  for(i in 1:length(x))
  {
    rect(x[i] - barwidth[i]/2,
         corners[1],
         x[i] + barwidth[i]/2,
         y[i],
         col = barcolor)
  }
  # ------------------------------------------------------------------------------
  
  
  
  
  
  
  
  
  
  
  
  # ------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------
  # DRAWING STRIPES; DRAWING STRIPES; DRAWING STRIPES; DRAWING STRIPES; DRAWING STRIPES
  # ------------------------------------------------------------------------------
  # ------------------------------------------------------------------------------
  
  # ------------------------------------------------------------------------------
  # Drawing stripes
  if(barstyle == 2)
  {
    
    # ------------------------------------------------------------------------------
    # for each bar
    for(i in 1:length(x))
    {
      # ------------------------------------------------------------------------------
      # where to put the stripes y coordinates
      yref <- seq(from = corners[1],
                      to = y[i],
                      by = stripespacing)
      yref <- append(yref,y[i])
      # ------------------------------------------------------------------------------
      
      
      # ------------------------------------------------------------------------------
      # if stripes should start above the bottom
      if(stripestart == 'even')
      {
        
        eliminate <- seq(from = 1,
                         to = length(yref),
                         by = 1)
        yref <- yref[-c(which(eliminate %% 2 == 0))]
        
      }
      # ------------------------------------------------------------------------------
      
      
      
      # ------------------------------------------------------------------------------
      # if stripes should start at the bottom
      if(stripestart == 'odd')
      {
        
        eliminate <- seq(from = 1,
                         to = length(yref),
                         by = 1)
        yref <- yref[-c(which(eliminate %% 2 != 0))]
        
      }
      # ------------------------------------------------------------------------------
      
      
      
      # ------------------------------------------------------------------------------
      # for
      for(j in 1:length(yref))
      {
        # ------------------------------------------------------------------------------
        # if stripes are to be angled upwards
        if(stripedir == 'forward')
        {
          # ------------------------------------------------------------------------------
          # if the stripe is going to overrun the bar
          if((yref[j] + stripewidth) > y[i] &
             (yref[j] + stripewidth/2) <= y[i])
          {
            
            known_triangle_len <- abs(y[i] - yref[j])
            x_overrun <- (x[i] - barwidth[i]/2) + known_triangle_len # advantage of 45 45 90 triangle
            
            polygon(x = c(x[i] - barwidth[i]/2,
                          x[i] - barwidth[i]/2,
                          x_overrun,
                          x[i] + barwidth[i]/2,
                          x[i] + barwidth[i]/2),
                    y = c(yref[j] - stripewidth/2,
                          yref[j] + stripewidth/2,
                          y[i],
                          y[i],
                          yref[j]),
                    col = stripecolor,
                    border = stripeborder)
            
            
          } 
          
          if((yref[j] + stripewidth/2) > y[i]) {
            
            
            known_triangle_len <- abs(y[i] - (yref[j] - stripewidth/2))
            x_overrun <- (x[i] - barwidth[i]/2) + known_triangle_len # advantage of 45 45 90 triangle
            
            if(x_overrun > (x[i] + barwidth[i]/2)){
              x_overrun <- (x[i] + barwidth[i]/2)
            }
            
            polygon(x = c(x[i] - barwidth[i]/2,
                          x[i] - barwidth[i]/2,
                          x_overrun,
                          x[i] + barwidth[i]/2,
                          x[i] + barwidth[i]/2),
                    y = c(yref[j] - stripewidth/2,
                          y[i],
                          y[i],
                          y[i],
                          y[i]),
                    col = stripecolor,
                    border = stripeborder)
            
            
          } 
          
          if((yref[j] + stripewidth) <= y[i] & 
             (yref[j] + stripewidth/2) <= y[i])
          {
            polygon(x = c(x[i] - barwidth[i]/2,
                          x[i] - barwidth[i]/2,
                          x[i] + barwidth[i]/2,
                          x[i] + barwidth[i]/2),
                    y = c(yref[j] - stripewidth/2,
                          yref[j] + stripewidth/2,
                          yref[j] + stripewidth,
                          yref[j]),
                    col = stripecolor,
                    border = stripeborder)
            
          }
          # ------------------------------------------------------------------------------
        }
        # ------------------------------------------------------------------------------
        
        
        
        
        
        
        
        
        # ------------------------------------------------------------------------------
        # if stripes are to be angled downwards
        if(stripedir == 'backward')
        {
          

          # ------------------------------------------------------------------------------
          # if the stripe is going to overrun the bar
          if((yref[j]) > y[i] &
             (yref[j] + stripewidth/2) <= y[i])
          {

            known_triangle_len <- abs(y[i] - yref[j])
            x_overrun <- (x[i] - barwidth[i]/2) + known_triangle_len # advantage of 45 45 90 triangle

            polygon(x = c(x[i] - barwidth[i]/2,
                          x[i] - barwidth[i]/2,
                          x_overrun,
                          x[i] + barwidth[i]/2,
                          x[i] + barwidth[i]/2),
                    y = c(yref[j] - stripewidth/2,
                          y[i],
                          y[i],
                          y[i],
                          yref[j] - stripewidth),
                    col = stripecolor,
                    border = stripeborder)


          }

          if((yref[j] + stripewidth/2) > y[i]) {


            known_triangle_len <- abs(y[i] - (yref[j] - stripewidth/2))
            x_overrun <- (x[i] - barwidth[i]/2) + known_triangle_len # advantage of 45 45 90 triangle

            if(x_overrun > (x[i] + barwidth[i]/2)){
              x_overrun <- (x[i] + barwidth[i]/2)
            }

            polygon(x = c(x[i] - barwidth[i]/2,
                          x[i] - barwidth[i]/2,
                          x_overrun,
                          x[i] + barwidth[i]/2,
                          x[i] + barwidth[i]/2),
                    y = c(yref[j] - stripewidth/2,
                          y[i],
                          y[i],
                          y[i],
                          yref[j] - stripewidth),
                    col = stripecolor,
                    border = stripeborder)


          }

          if((yref[j]) <= y[i] &
             (yref[j] + stripewidth/2) <= y[i])
          {
            polygon(x = c(x[i] - barwidth[i]/2,
                          x[i] - barwidth[i]/2,
                          x[i] + barwidth[i]/2,
                          x[i] + barwidth[i]/2),
                    y = c(yref[j] - stripewidth/2,
                          yref[j] + stripewidth/2,
                          yref[j],
                          yref[j] - stripewidth),
                    col = stripecolor,
                    border = stripeborder)
            
          }
          # ------------------------------------------------------------------------------
        }
        # ------------------------------------------------------------------------------
      }
      # ------------------------------------------------------------------------------
    }
    # ------------------------------------------------------------------------------
  }
  # ------------------------------------------------------------------------------
  
  
  
  
  
  
  
  
  
  # ------------------------------------------------------------------------------
  # Drawing stripes
  if(barstyle == 3)
  {

    # ------------------------------------------------------------------------------
    # for each bar
    for(i in 1:length(x))
    {
      # ------------------------------------------------------------------------------
      # where to put the stripes y coordinates
      yref <- seq(from = corners[1],
                  to = y[i],
                  by = stripespacing)
      yref <- append(yref,y[i])
      # ------------------------------------------------------------------------------
      
      # ------------------------------------------------------------------------------
      for(j in 1:length(yref))
      {
        # ------------------------------------------------------------------------------
        if(linedir == 'forward')
        {
          # ------------------------------------------------------------------------------
          # preventing line overrun
          if(yref[j] != y[i])
          {
          
            
            y2 <- yref[j] + barwidth[i]/2
            x2 <- x[i] + barwidth[i]/2
            # ------------------------------------------------------------------------------
            # preventing line overrun
            if(y2 > y[i])
            {

              known_triangle_len <- abs(y[i] - (yref[j] + barwidth[i]/2))
              x_overrun <- (x[i] - barwidth[i]/2) + known_triangle_len # advantage of 45 45 90 triangle
              print(x[i])
              y2 <- y[i]

            }
            # ------------------------------------------------------------------------------
            lines(x = c(x[i] - barwidth[i]/2,
                        x2),
                  y = c(yref[j],
                        y2),
                  lty = linestyle,
                  col = linecolor,
                  lwd = linewidth)
          }
          # ------------------------------------------------------------------------------
          
        }
        # ------------------------------------------------------------------------------
        
        
        
        # ------------------------------------------------------------------------------
        if(linedir == 'backward')
        {
          
          
          y2 <- yref[j] - barwidth[i]/2
          x2 <- x[i] + barwidth[i]/2

          lines(x = c(x[i] - barwidth[i]/2,
                      x2),
                y = c(yref[j],
                      y2),
                lty = linestyle,
                col = linecolor,
                lwd = linewidth)

        }
        # ------------------------------------------------------------------------------
      }
      # ------------------------------------------------------------------------------
    }
    # ------------------------------------------------------------------------------
  }
  # ------------------------------------------------------------------------------
}
# ------------------------------------------------------------------------------
