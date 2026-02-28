custom_boxplots <- function(bstats,
                            out = NULL,
                            box_width = 0.35,
                            whisker_lty = 3,
                            whisker_col = 'black',
                            whisker_lwd = 2,
                            whisker_width = 0.35,
                            box_col = 'gray',
                            box_lwd = 2,
                            box_lty = 1,
                            box_labels_tf = FALSE,
                            box_labels = 'boxn',
                            label_srt = 25,
                            label_pos = 1,
                            label_col = 'black',
                            label_cex = 1,
                            label_y_offset = 0.015,
                            box_border = 'black',
                            ylab = '',
                            xlab = '',
                            xaxt = 'n',
                            yaxt = NULL,
                            cex.lab = 1,
                            cex.axis = 1,
                            main = '',
                            title = NULL,
                            title_x_offset = 0.01,
                            title_y_offset = 0.07,
                            title_pos = 4,
                            title_cex = 1.6,
                            title_col = 'black',
                            subtitle = NULL,
                            subtitle_x_offset = 0.01,
                            subtitle_y_offset = 0.03,
                            subtitle_pos = 4,
                            subtitle_cex = 1.1,
                            subtitle_col = 'gray40',
                            mar = c(5.1,4.1,4.1,2.1),
                            grid = TRUE,
                            grid_col = 'gray40',
                            grid_lty = 3,
                            grid_lwd = 1,
                            plotbg = 'gray95',
                            rectbg = 'gray97')
{
  if(!'matrix' %in% class(bstats)){
    stop(paste0('\nerror encountered when running custom_boxplots:\n',
                'bstats is not an array'))
  }
  # =================================================================================================
  # ensure that the number of arguments matches the number of boxes
  # =================================================================================================
  ensure_argument_lengths <- function(args){
    # ------------------------------------------------------------------------------------------------
    # for each argument ensure user has entered correct number of arguments
    for(i in 1:length(args)){
      # ------------------------------------------------------------------------------------------------
      # if length of argument is incorrect
      if(length(args[[i]]) %% n_boxes != 0){
        # ------------------------------------------------------------------------------------------------
        # case where user has entered too many arguments
        if(length(args[[i]]) > n_boxes){
          end_missing_position <- min(c(length(args[[i]]),
                                        n_boxes))
          args[[i]] <- args[[i]][1:end_missing_position]
        }
        # ------------------------------------------------------------------------------------------------
        
        # ------------------------------------------------------------------------------------------------
        # case where user has entered not enough arguments
        if(length(args[[i]]) < n_boxes){
          while(length(args[[i]]) < n_boxes){
            
            n_missing <- n_boxes - length(args[[i]])
            
            # ------------------------------------------------------------------------------------------------
            # if all is needed is a simple append statement
            if(n_missing < length(args[[i]])){
              args[[i]] <- append(args[[i]],
                                  args[[i]][1:n_missing])
            }
            # ------------------------------------------------------------------------------------------------
            
            # ------------------------------------------------------------------------------------------------
            # if a rep and an append are needed
            if(n_missing > length(args[[i]])){
              rep_n <- floor(n_boxes/length(args[[i]]))
              args[[i]] <- rep(args[[i]],rep_n)
            }
            # ------------------------------------------------------------------------------------------------
          }
        }
        # ------------------------------------------------------------------------------------------------
      }
      # ------------------------------------------------------------------------------------------------
    }
    # ------------------------------------------------------------------------------------------------
    return(args)
  }
  # ------------------------------------------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------
  # ensure argument lengths match n_boxes
  n_boxes <- ncol(bstats)
  args <- ensure_argument_lengths(list(whisker_lty,
                                       whisker_col,
                                       whisker_lwd,
                                       whisker_width,
                                       box_col,
                                       box_lwd,
                                       box_lty,
                                       box_border,
                                       box_width,
                                       box_labels,
                                       label_srt,
                                       label_col,
                                       label_cex,
                                       label_y_offset))
  whisker_lty <- args[[1]]
  whisker_col <- args[[2]]
  whisker_lwd <- args[[3]]
  whisker_width <- args[[4]]
  box_col <- args[[5]]
  box_lwd <- args[[6]]
  box_lty <- args[[7]]
  box_border <- args[[8]]
  box_width <- args[[9]]
  box_labels <- args[[10]]
  label_srt <- args[[11]]
  label_col <- args[[12]]
  label_cex <- args[[13]]
  label_y_offset <- args[[14]]
  # ------------------------------------------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------
  # plot outliars or not
  if(is.null(out) == FALSE){
    # ------------------------------------------------------------------------------------------------
    # basic plotting positions
    n_boxes <- ncol(bstats)
    box_positions <- list()
    for(i in 1:n_boxes){
      box_positions[[i]] <- c(rep(i,nrow(bstats)),
                              rep(i,length(out[[i]])))
    }
    # box_positions_wide <- do.call(cbind, box_positions)
    box_positions_long <- as.vector(unlist(box_positions))
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # long format for plotting
    bstats_long <- matrix(c(bstats[,1],out[[1]]),
                          ncol = 1)
    bstats_wide <- bstats
    if(ncol(bstats) > 1){
      for(i in 2:ncol(bstats)){
        bstats_long <- rbind(bstats_long,
                             matrix(c(bstats[,i],out[[i]]),
                                    ncol = 1))
      }
    }
    # ------------------------------------------------------------------------------------------------
  } else {
    # ------------------------------------------------------------------------------------------------
    # basic plotting positions
    n_boxes <- ncol(bstats)
    box_positions <- list()
    for(i in 1:n_boxes){
      box_positions[[i]] <- c(rep(i,nrow(bstats)))
    }
    # box_positions_wide <- do.call(cbind, box_positions)
    box_positions_long <- as.vector(unlist(box_positions))
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # long format for plotting
    bstats_long <- matrix(c(bstats[,1]),
                          ncol = 1)
    bstats_wide <- bstats
    if(ncol(bstats) > 1){
      for(i in 2:ncol(bstats)){
        bstats_long <- rbind(bstats_long,
                             matrix(bstats[,i],
                                    ncol = 1))
      }
    }
    # ------------------------------------------------------------------------------------------------
  }
  # ------------------------------------------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------
  par(mar = c(mar))
  par(bg = plotbg)
  plot(x = box_positions_long,
       y = bstats_long,
       xlim = c(1 - 0.5,
                n_boxes + 0.5),
       xaxt = xaxt,
       yaxt = yaxt,
       xlab = xlab,
       ylab = ylab,
       main = main,
       cex.lab = cex.lab,
       cex.axis = cex.axis,
       col = 'white')
  usr <- par('usr')
  rect(usr[1],usr[3],usr[2],usr[4],
       col = rectbg)
  
  if(grid == TRUE){
    for(i in 1:length(axTicks(2))){
      abline(h = axTicks(2)[i],
             lwd = grid_lwd,
             lty = grid_lty,
             col = grid_col)
    }
  }
  x_range <- par('usr')[2] - par('usr')[1]
  y_range <- par('usr')[4] - par('usr')[3]
  # ------------------------------------------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------
  for(i in 1:n_boxes){
    # ------------------------------------------------------------------------------------------------
    # draw main body of box
    rect(xleft = i - box_width[i],
         ybottom = bstats[2,i],
         xright = i + box_width[i],
         ytop = bstats[4,i],
         border = box_border[i],
         lwd = box_lwd[i],
         lty = box_lty[i],
         col = box_col[i])
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    if(is.null(out) == FALSE){
      points(x = rep(i,length(out[[i]])),
             y = out[[i]],
             bg = box_col[i],
             col = box_border[i],
             pch = 21)
    }
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # draw median
    lines(x = c(i - box_width[i],
                i + box_width[i]),
          y = c(bstats[3,i],
                bstats[3,i]),
          col = box_border[i],
          lwd = box_lwd[i],
          lty = box_lty[i])
    # ------------------------------------------------------------------------------------------------
    
    # ------------------------------------------------------------------------------------------------
    # draw bottom whisker
    lines(x = c(i,i),
          y = c(bstats[2,i],
                bstats[1,i]),
          col = whisker_col[i],
          lwd = whisker_lwd[i],
          lty = whisker_lty[i])
    lines(x = c(i - whisker_width[i],
                i + whisker_width[i]),
          y = c(bstats[1,i],
                bstats[1,i]),
          col = whisker_col[i],
          lwd = whisker_lwd[i],
          lty = whisker_lty[i])
    # ------------------------------------------------------------------------------------------------
    
    
    # ------------------------------------------------------------------------------------------------
    # draw top whisker
    lines(x = c(i,i),
          y = c(bstats[4,i],
                bstats[5,i]),
          col = whisker_col[i],
          lwd = whisker_lwd[i],
          lty = whisker_lty[i])
    lines(x = c(i - whisker_width[i],
                i + whisker_width[i]),
          y = c(bstats[5,i],
                bstats[5,i]),
          col = whisker_col[i],
          lwd = whisker_lwd[i],
          lty = whisker_lty[i])
    # ------------------------------------------------------------------------------------------------
  }
  # ------------------------------------------------------------------------------------------------
  
  # ------------------------------------------------------------------------------------------------
  # labels or no labels
  if(box_labels_tf == TRUE){
    if('boxn' %in% box_labels){
      axis(1,
           at = c(1:n_boxes),
           labels = c(1:n_boxes))
    }
    
    if(!'boxn' %in% box_labels){
      axis(1,
           at = c(1:n_boxes),
           labels = rep('',n_boxes))
      for(i in 1:length(box_labels)){
        text(x = i,
             y = par('usr')[3] - y_range*label_y_offset[i],
             adj = label_pos,
             srt = label_srt[i],
             labels = box_labels[i],
             cex = label_cex[i],
             col = label_col[i],
             xpd = NA)
      }
    }
  }
  # ------------------------------------------------------------------------------------------------
  
  
  # ------------------------------------------------------------------------------------------------
  if(is.null(title) == FALSE){
    text(x = par('usr')[1] - x_range*title_x_offset,
         y = par('usr')[4] + y_range*title_y_offset,
         pos = title_pos,
         labels = title, 
         cex = title_cex,
         col = title_col,
         xpd = NA)
  }
  
  if(is.null(subtitle) == FALSE){
    text(x = par('usr')[1] - x_range*subtitle_x_offset,
         y = par('usr')[4] + y_range*subtitle_y_offset,
         pos = subtitle_pos,
         labels = subtitle, 
         cex = subtitle_cex,
         col = subtitle_col,
         xpd = NA)
  }
  # ------------------------------------------------------------------------------------------------
  box()
}
# ------------------------------------------------------------------------------------------------