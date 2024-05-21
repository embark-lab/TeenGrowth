

#' @title solve for weight
#' @description Using BMI and height, solve for weight
#' @param bmi 'bmi'
#' @param height 'height in inches'
#' @import dplyr
#' @return weight
#' @export

solve_for_weight <- function(bmi, height){
  w = (bmi*(height^2))/703
  return(w)
}


#' @title Cutoff BMI
#' @description Makes Cutoff BMIs
#' @param data 'data'
#' @param sex '1 = Male, 2 = Female'
#' @param age 'age in months'
#' @param adult_height 'adult height in inches'
#' @import dplyr
#' @return bmi scores based on bmiz and age
#' @export

cutoff_bmi_by_age_data <- function (age = agemos, data_source = 'cdc', sex = 2, adult_height = NA) {
  vectorized_bmi_lookup <- Vectorize(bmi_lookup, vectorize.args = c("age"))
  agemos <- c(36:240)
  tibble(agemos = age) %>%
    mutate(
      adult_height = adult_height,
      median_bmi = vectorized_bmi_lookup(data_point = 0, sex = sex, age = agemos, type = 'bmiz', data_source = data_source),
      UW_cutoff_bmi = vectorized_bmi_lookup(data_point = -1, sex = sex, age = agemos, type = 'bmiz', data_source = data_source),
      median_wt = if_else(age > 14*12 & !is.na(adult_height), solve_for_weight(median_bmi, adult_height), NaN),
      AN_cutoff_wt = if_else(age > 14*12 & !is.na(adult_height), median_wt * 0.85, NaN),
      sex = sex
    )
}

