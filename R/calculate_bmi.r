
# function that takes weight (in kg or lbs), and height (in cm or m or in) as input and outputs bmi

# set up parameters for function below for r package
#' @param weight weight in kg or lbs
#' @param weight_unit kg or lbs
#' @param height height in cm or m or in
#' @param height_unit cm or m or in
#' @return bmi
#' @export
#'

calculate_bmi <- function(weight, weight_unit = 'kg', height, height_unit = 'cm') {
  if (weight_unit == 'kg' & height_unit == 'cm'){
    bmi <- weight/((height/100)^2)
  } else if (weight_unit == 'kg' & height_unit == 'm'){
    bmi <- weight/(height^2)
  } else if (weight_unit == 'lb' & height_unit == 'in'){
    bmi <- (weight/(height^2))*703
  } else if (weight_unit == 'lbs' & height_unit == 'cm'){
    bmi <- (weight/((height/100)^2))*703
  } else if (weight_unit == 'lbs' & height_unit == 'm'){
    bmi <- (weight/(height^2))*703
  } else if (weight_unit == 'kg' & height_unit == 'in'){
    bmi <- (weight/(height^2))*703
  } else if (weight_unit == 'lbs' & height_unit == 'm'){
    bmi <- (weight/(height^2))*703
  } else if (weight_unit == 'kg' & height_unit == 'in'){
    bmi <- (weight/(height^2))*703
  }
  return(bmi)
}

# Function that detects how sex variable is coded ('Male/Female; M/F, or 0(Male)/1(Female)') and converts them to 1/2.

align_sex_coding <- function(sex) {
  if (all(sex %in% c("Male", "Female"))) {
    coding <- "Male/Female"
    sex[sex == "Male"] <- 1
    sex[sex == "Female"] <- 2
  } else if (all(sex %in% c("M", "F"))) {
    coding <- "M/F"
    sex[sex == "M"] <- 1
    sex[sex == "F"] <- 2
  } else if (all(sex %in% c(0, 1))) {
    coding <- "0(Male)/1(Female)"
    sex[sex == 0] <- 1
    sex[sex == 1] <- 2
  } else if (all(sex %in% c(1, 2))) {
    coding <- "1(Male)/2(Female)"
  } else {
    stop("Invalid coding. Please use 'Male/Female', 'M/F', '0/1', or '1/2'.")
  }
  return(list(coding = coding, sex = sex))
}

# Function that has input of weight in kg or lbs and returns weight in kg
weight_in_kg <- function(weight, weight_unit) {
  if (weight_unit == 'kg') {
    weight <- weight
  } else if (weight_unit == 'lb') {
    weight <- weight*.4535
  }
  return(weight)
}

# Function that has input of height in cm, in, meters, or feet and returns height in cm
height_in_cm <- function(height, height_unit) {
  if (height_unit == 'cm') {
    height <- height
  } else if (height_unit == 'in') {
    height <- height*2.54
  } else if (height_unit == 'm') {
    height <- height*100
  } else if (height_unit == 'ft') {
    height <- height*30.48
  }
  return(height)
}

