
#'Function that calculates BMI (Body Mass Index) based on weight and height inputs.
#' @param weight Weight value in either kilograms (kg) or pounds (lbs).
#' @param weight_unit Unit of weight measurement ('kg' or 'lbs').
#' @param height Height value in either centimeters (cm), meters (m), inches (in), or feet (ft).
#' @param height_unit Unit of height measurement ('cm', 'm', 'in', or 'ft').
#' @return BMI calculated based on the provided weight and height.
#' @export

calculate_bmi <- function(weight,
                          weight_unit = 'kg',
                          height,
                          height_unit = 'cm') {
  if (weight_unit == 'kg' & height_unit == 'cm'){
    bmi <- weight / ((height / 100)^2)
  } else if (weight_unit == 'kg' & height_unit == 'm'){
    bmi <- weight / (height^2)
  } else if (weight_unit == 'lb' & height_unit == 'in'){
    bmi <- (weight / (height^2)) * 703
  }

  return(bmi)
}

#' Function that converts weight from pounds (lb) to kilograms (kg).
#' @param weight Weight value in pounds (lb) or kilograms (kg) (default).
#' @return Weight value converted to kilograms (kg).
#' @export
weight_in_kg <- function(weight,
                         weight_units = 'kg') {
  if (weight_units == 'lbs'| weight_units == 'lb'| weight_units == 'pounds') {
    weight <- weight * 0.45359237  # Convert pounds to kilograms
  }
  else if (weight_units == 'kg'| weight_units == 'kgs'|weight_units == 'kilograms'|weight_units == 'kilos') {
    weight <- weight
  }
  return(weight)
}


#' Function that converts height from various units (inches, meters, feet) to centimeters (cm).
#' @param height Height value in either inches (in), meters (m), or feet (ft).
#' @param height_unit Unit of height measurement ('in', 'm', or 'ft').
#' @return Height value converted to centimeters (cm).
#' @export
height_in_cm <- function(height, height_unit = 'cm') {
  if (height_unit == 'in' | height_unit == 'inches' | height_unit == 'inch' ) {
    height <- height * 2.54  # Convert inches to centimeters
  } else if (height_unit == 'm'| height_unit == 'meter' | height_unit == 'meters') {
    height <- height * 100   # Convert meters to centimeters
  } else if (height_unit == 'ft' | height_unit == 'feet') {
    height <- height * 30.48 # Convert feet to centimeters
  }
    else if (height_unit == 'cm'|height_unit == 'centimeter'|height_unit == 'centimeters') {
    height <- height
  }
  return(height)
}
