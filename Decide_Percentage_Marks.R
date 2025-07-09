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
  avoid_nans <- ((new_values - base_values)/base_values)*100
  range <- range(avoid_nans[is.nan(avoid_nans) == FALSE &
                              is.na(avoid_nans) == FALSE])
  percentages_sequence <- seq(from = range[1],
                              to = range[2],
                              length.out = length(plot_axis))
  percentages_sequence <- round(percentages_sequence,round)
  # -------------------------------------------------------------------------------------
  return(percentages_sequence)
}
# -------------------------------------------------------------------------------------