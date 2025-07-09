#===============================================================================
# Estimates an axis sequence for the percentage change between base values
# and new values
#===============================================================================
Decide_Percentage_Marks <- function(base_values,
                                    new_values,
                                    plot_axis = axTicks(2),
                                    round = 3,
                                    invert = FALSE,
                                    scale = 'difference')
{
  # -------------------------------------------------------------------------------------
  # are full values being plotted or the difference between them?
  if(scale == 'difference'){
    # -------------------------------------------------------------------------------------
    # Plotting difference or real values?
    if(invert == TRUE){
      avoid_nans <- ((base_values - new_values)/new_values)*100
    } else {
      avoid_nans <- ((new_values - base_values)/base_values)*100
    }
    # -------------------------------------------------------------------------------------
    
    # -------------------------------------------------------------------------------------
    # Get ticks
    range <- range(avoid_nans[is.nan(avoid_nans) == FALSE &
                                is.na(avoid_nans) == FALSE &
                                is.infinite(avoid_nans) == FALSE])
    percentages_sequence <- seq(from = range[1],
                                to = range[2],
                                length.out = length(plot_axis))
    percentages_sequence <- round(percentages_sequence,round)
    # -------------------------------------------------------------------------------------
    
  } else if (scale == 'values'){
    # -------------------------------------------------------------------------------------
    # Get original ticks by plotting to null pdf device
    pdf(NULL)
    invisible(plot(base_values))
    comparison_values <- axTicks(2)
    dev.off()
    # -------------------------------------------------------------------------------------
    
    # -------------------------------------------------------------------------------------
    # in case divide by 0 by one axis having 0 in range while the other doesnt
    range <- (range(plot_axis)/range(comparison_values)) * 100
    range <- range[is.nan(range) == FALSE &
                   is.na(range) == FALSE &
                   is.infinite(range) == FALSE]
    # -------------------------------------------------------------------------------------
    
    # -------------------------------------------------------------------------------------
    if(length(range) == 1){
      # -------------------------------------------------------------------------------------
      # deciding order of sequence
      if(range < 0){
        percentages_sequence <- seq(from = range,
                                    to = 0,
                                    length.out = length(plot_axis))
      } else {
        percentages_sequence <- seq(from = 0,
                                    to = range,
                                    length.out = length(plot_axis))
      }
      # -------------------------------------------------------------------------------------
    } else { # if 0 not in range it self organizes and dont need to decide the order for it
      percentages_sequence <- seq(from = range[1],
                                  to = range[2],
                                  length.out = length(plot_axis))
    }
    # -------------------------------------------------------------------------------------
    percentages_sequence <- round(percentages_sequence,round)
  }
  # -------------------------------------------------------------------------------------
  return(percentages_sequence)
}
# -------------------------------------------------------------------------------------