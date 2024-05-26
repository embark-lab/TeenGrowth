#' Clean Forecast Data
#'
#' This function cleans and processes forecast data by filtering based on
#' patient ID, model type, and confidence interval. It formats the data to
#' include only ages divisible by 6 and rounds all numerical values to one
#' decimal place. It also formats the expected BMI and expected weight.
#'
#' @param data A data frame containing forecast data.
#' @param px The patient ID to filter the data.
#' @param model The model type to filter the data. Default is 'ARIMA_mean'.
#' @param ci The confidence interval type to filter the data. Default is '95%'.
#' @return A cleaned and processed data frame with the age in years, expected
#' BMI, and expected weight, formatted appropriately.
#' @examples
#' # Assuming 'forecast_data' is a data frame containing the relevant columns
#' clean_data <- clean_forecast_data(forecast_data, px = 12345)
#' @export
clean_forecast_data <- function(data,
                                px,
                                model) {
  data <-  data %>%
    filter(id == px & .model == model) %>%
    # only use ages that are divisible by 6
    filter(agemos %% 6 == 0) %>%
    # make an age column that is the age in years
    mutate(Age = agemos / 12) %>%
    select(Age, eBMI, lower_eBMI, upper_eBMI, eWeight, lower_eWeight, upper_eWeight) %>%
    # make columns for eWeight, lower_eWeight, and upper_eWeight in kgs
    mutate(eWeight_kgs = eWeight * 0.453592,
           lower_eWeight_kgs = lower_eWeight * 0.453592,
           upper_eWeight_kgs = upper_eWeight * 0.453592) %>%
    # round all of the values to 1 decimal place
    mutate_all(round, 1) %>%
    # make eBMI (lower_eBMI, upper_eBMI) as the format of eBMI (lower_eBMI, upper_eBMI)
    mutate(`Expected BMI` = paste0(eBMI, ' (', lower_eBMI, ', ', upper_eBMI, ')')) %>%
    # make eWeight (lower_eWeight, upper_eWeight) as the format of eWeight (lower_eWeight, upper_eWeight)
    mutate(`Expected Weight (lbs)` = paste0(eWeight, ' (', lower_eWeight, ', ', upper_eWeight, ')')) %>%
    # make same for eWeight_kgs (lower_eWeight_kgs, upper_eWeight_kgs)
    mutate(`Expected Weight (kgs)` = paste0(eWeight_kgs, ' (', lower_eWeight_kgs, ', ', upper_eWeight_kgs, ')')) %>%
    # remove the lower and upper eBMI and eWeight columns
    select(Age, `Expected BMI`, `Expected Weight (lbs)`, `Expected Weight (kgs)`)

  # return the data
  return(data)
}
