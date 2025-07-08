#===============================================================================
# Estimates an axis sequence for the percentage change between base values
# and new values
#===============================================================================
Decide_Percentage_Marks <- function(base_values,
                                    new_values,
                                    plot_axis = axTicks(2),
                                    round = 3)
{
  # -------------------------------------------------------------------------------------
  range <- range(((new_values - base_values)/base_values)*100)
  percentages_sequence <- seq(from = range[1],
                              to = range[2],
                              length.out = length(plot_axis))
  percentages_sequence <- round(percentages_sequence,round)
  # -------------------------------------------------------------------------------------
  return(percentages_sequence)
}
# -------------------------------------------------------------------------------------