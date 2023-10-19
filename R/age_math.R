
# Function that determines whether age is in days, weeks, months, or years based on the input
# assumes that age is between 5 and 21 years
age_unit <- function(age){
  if (age > 365*5){
    age_unit <- 'days'
  } else if (age > 52*5){
    age_unit <- 'weeks'
  } else if (age > 12*5){
    age_unit <- 'months'
  } else if (age > 2) {
    age_unit <- 'years'
  }
  return(age_unit)
}


# Function that takes an age input that is either years, months, days, or birthdate + date of assessment and outputs age in days
age_in_days <- function(age, age_unit = NULL, dob = NULL, date_assessed = NULL){
  if(is.null(age_unit)){
    age_unit <- age_unit(age)
  }
  if (!is.null(dob) & !is.null(date_assessed)){
    age_unit <- 'dates'
  }
  if (age_unit == 'years'){
    age_in_days <- age*365.25
  } else if (age_unit == 'months'){
    age_in_days <- age*30.4375
  } else if (age_unit == 'days'){
    age_in_days <- age
  } else if (age_unit == 'weeks'){
    age_in_days <- age*7
  } else if (age_unit == 'dates'){
    age_in_days <- as.numeric(difftime(date_assessed, dob, units = 'days'))
  }
  return(age_in_days)
}

# Function that converts age in days to age in months
age_in_months <- function(age_days){
  age_in_months <- round(age_in_days(age_days)/30.4375,0)
  return(age_in_months)
}

