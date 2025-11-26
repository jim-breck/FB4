########################################################################
### Data Interpolation Utilities
########################################################################
#
# This module provides wrapper functions for interpolating time-series data.
#
# Two interpolation methods are used in FB4:
#   - Linear: For most environmental and biological variables (temperature, diet, etc.)
#   - Constant: For mortality and reproduction (values persist until changed)
#
########################################################################

interpolate_linear <- function(days, values, n_days) {
  ### Linearly interpolate values over n days
  ###
  ### Parameters:
  ###   days: Vector of day numbers with data
  ###   values: Vector of values corresponding to days
  ###   n_days: Number of days to interpolate to
  ###
  ### Returns: Vector of interpolated values

  interpolated <- approx(days, values, n = n_days, method = "linear")$y
  return(interpolated)
}

interpolate_constant <- function(days, values, n_days) {
  ### Constant interpolation (values persist until changed)
  ### Used for mortality and reproduction where rates/events remain constant
  ### until explicitly changed
  ###
  ### Parameters:
  ###   days: Vector of day numbers with data
  ###   values: Vector of values corresponding to days
  ###   n_days: Number of days to interpolate to
  ###
  ### Returns: Vector of interpolated values

  interpolated <- approx(days, values, n = n_days, method = "constant")$y
  return(interpolated)
}

interpolate_days <- function(days, values, n_days, method = "linear") {
  ### Interpolate day numbers (x-axis)
  ### Returns the x-coordinates of the interpolated points
  ###
  ### Parameters:
  ###   days: Vector of day numbers with data
  ###   values: Vector of values corresponding to days
  ###   n_days: Number of days to interpolate to
  ###   method: Interpolation method ("linear" or "constant")
  ###
  ### Returns: Vector of interpolated day numbers

  interpolated_days <- approx(days, values, n = n_days, method = method)$x
  return(interpolated_days)
}

subset_to_range <- function(data, first_day, last_day) {
  ### Subset interpolated data to a specific day range
  ###
  ### Parameters:
  ###   data: Vector of interpolated data
  ###   first_day: First day of desired range
  ###   last_day: Last day of desired range
  ###
  ### Returns: Subset of data from first_day to last_day

  return(data[first_day:last_day])
}
