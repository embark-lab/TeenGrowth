# Shared helper functions for unit conversion

#' Convert height between units
#' @param height Height value.
#' @param from_unit Source unit ('cm' or 'in').
#' @param to_unit Target unit ('cm' or 'in').
#' @return Converted height value.
#' @keywords internal
convert_height <- function(height, from_unit, to_unit) {
  if (is.null(height)) return(NA)
  if (from_unit == to_unit) return(height)
  if (from_unit == 'cm' && to_unit == 'in') return(height * 0.393701)
  if (from_unit == 'in' && to_unit == 'cm') return(height * 2.54)
  stop("Invalid height units")
}

#' Convert weight between units
#' @param weight Weight value.
#' @param from_unit Source unit ('kg' or 'lb').
#' @param to_unit Target unit ('kg' or 'lb').
#' @return Converted weight value.
#' @keywords internal
convert_weight <- function(weight, from_unit, to_unit) {
  if (is.null(weight)) return(NA)
  if (from_unit == to_unit) return(weight)
  if (from_unit == 'kg' && to_unit == 'lb') return(weight * 2.20462)
  if (from_unit == 'lb' && to_unit == 'kg') return(weight / 2.20462)
  stop("Invalid weight units")
}
