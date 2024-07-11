#' Determine the age unit (days, weeks, months, or years) based on the input age.
#'
#' This function assumes that the input age is between 5 and 21 years.
#'
#' @param age Numeric value representing the age.
#'
#' @return A character string indicating the age unit (days, weeks, months, or years).
#'
#' @examples
#' age_unit(365)  # Returns "days"
#' age_unit(10*365)  # Returns "years"
#'
#' @export
age_unit <- function(age) {
  if (is.na(age)) {
    return(NA)
  } else if (age > 365 * 5) {
    return('days')
  } else if (age > 52 * 5) {
    return('weeks')
  } else if (age > 12 * 3) {
    return('months')
  } else {
    return('years')
  }
}


#' Convert age from various units to days.
#'
#' This function takes an age input that can be in years, months, days, or as a combination of birthdate and date of assessment.
#'
#' @param age Numeric value representing the age.
#' @param age_unit (Optional) A character string specifying the unit of the input age ('years', 'months', 'days').
#' @param dob (Optional) The date of birth as a character string in the format 'YYYY-MM-DD'.
#' @param date_assessed (Optional) The date of assessment as a character string in the format 'YYYY-MM-DD'.
#'
#' @return Numeric value representing the age in days.
#'
#' @examples
#' age_in_days(10, "years")  # Returns the age in days
#' age_in_days(120, "months")  # Returns the age in days
#'
#' @export

age_in_days <- function(age, age_unit = NULL, dob = NULL, date_assessed = NULL) {
  if (!is.null(dob) & !is.null(date_assessed)) {
    age_unit <- 'dates'
  } else if (is.null(age_unit)) {
    age_unit <- age_unit(age)
  }

  if (is.na(age) || is.na(age_unit)) {
    return(NA)
  } else if (age_unit == 'years') {
    return(age * 365.25)
  } else if (age_unit == 'months') {
    return(age * 30.4375)
  } else if (age_unit == 'weeks') {
    return(age * 7)
  } else if (age_unit == 'days') {
    return(age)
  } else if (age_unit == 'dates') {
    return(as.numeric(difftime(date_assessed, dob, units = 'days')))
  } else {
    return(NA)
  }
}


#' Convert age to age in months (rounded).
#' @param age age
#' @param age_unit (Optional) A character string specifying the unit of the input age ('years', 'months', 'days').
#' @param dob (Optional) The date of birth as a character string in the format 'YYYY-MM-DD'.
#' @param date_assessed (Optional) The date of assessment as a character string in the format 'YYYY-MM-DD'.
#' @return Numeric value representing the age in months (rounded to the nearest month).
#'
#' @examples
#' age_in_months(365)  # Returns 12
#' age_in_months(730)  # Returns 24
#'
#' @export
age_in_months <- function(age, age_unit = NULL, dob = NULL, date_assessed = NULL) {
  age_in_months <- round(age_in_days(age = age, age_unit = age_unit, dob = dob, date_assessed = date_assessed) / 30.4375, 0)
  return(age_in_months)
}
