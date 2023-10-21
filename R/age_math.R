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
  if (age > 365 * 5) {
    age_unit <- 'days'
  } else if (age > 52 * 5) {
    age_unit <- 'weeks'
  } else if (age > 12 * 5) {
    age_unit <- 'months'
  } else if (age > 2) {
    age_unit <- 'years'
  }
  return(age_unit)
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
  if (is.null(age_unit)) {
    age_unit <- age_unit(age)
  }
  if (!is.null(dob) & !is.null(date_assessed)) {
    age_unit <- 'dates'
  }
  if (age_unit == 'years') {
    age_in_days <- age * 365.25
  } else if (age_unit == 'months') {
    age_in_days <- age * 30.4375
  } else if (age_unit == 'days') {
    age_in_days <- age
  } else if (age_unit == 'weeks') {
    age_in_days <- age * 7
  } else if (age_unit == 'dates') {
    age_in_days <- as.numeric(difftime(date_assessed, dob, units = 'days'))
  }
  return(age_in_days)
}

#' Convert age in days to age in months (rounded).
#'
#' @param age_days Numeric value representing the age in days.
#'
#' @return Numeric value representing the age in months (rounded to the nearest month).
#'
#' @examples
#' age_in_months(365)  # Returns 12
#' age_in_months(730)  # Returns 24
#'
#' @export
age_in_months <- function(age_days) {
  age_in_months <- round(age_in_days(age_days) / 30.4375, 0)
  return(age_in_months)
}
