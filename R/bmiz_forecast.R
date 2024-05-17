# Function to fit models and generate forecasts for BMIz data
# Load necessary libraries
library(dplyr)
library(tsibble)
library(fable)
library(fabletools)
library(purrr)
library(tidyr)

vectorized_age_in_months <- Vectorize(age_in_months, vectorize.args = c("age"))
vectorized_age_in_days <- Vectorize(age_in_days, vectorize.args = c("age"))

make_bmiz_tsibble <- function(data, age = age, bmiz = bmiz, px = NULL, dob = NULL, date_assessed = NULL, age_unit = NULL) {
  if (!missing(px) && px %in% names(data)) {
    # If px column is provided and exists in the data, use it
    data <- data |> mutate(px = .data[[px]])
  } else {
    # If px column is not provided or does not exist, create a px column with 1s
    data <- data |> mutate(px = 1)
  }

  ts_data <- data |>
    select(age, bmiz, px) |>
    mutate(agemos = vectorized_age_in_months(age = as.numeric(age), age_unit = age_unit, dob = dob, date_assessed = date_assessed)) |>
    select(-age) |>
    as_tsibble(index = agemos, key = px) |>
    # rename key to participant
    rename(participant = px) |>
    tsibble::fill_gaps()

  return(ts_data)
}


# Function to fit models and generate forecasts for BMIz data
fit_and_forecast_bmiz <- function(ts_data) {

  # Fit the models for each participant, excluding rows with NA bmiz
  bmiz_fit <- ts_data %>%
    model(
      Mean = MEAN(bmiz), # Mean of all measurements
      ARIMA = ARIMA(bmiz ~ pdq(0,1,0)) # 'Naive' Model which uses the last observation and includes a random walk from the last obs
    )


  # Generate forecasts for each participant
  forecast_fit <- bmiz_fit %>%
    mutate(ARIMA_mean = (ARIMA + Mean) / 2) %>%
    forecast(h = 60)   # forecasts for 5 years

  return(forecast_fit)
}

# Assuming tsibble_data is already created and structured correctly
# Fit the models and generate forecasts using the defined function
forecast_results <- fit_and_forecast_bmiz(tsibble_data)


# puts make tsibble and fit data together
make_bmiz_forecast <- function(data, age = age, bmiz = bmiz, participant = NULL,
                               dob = NULL, date_assessed = NULL, age_unit = NULL) {
  if (!missing(participant) && participant %in% names(data)) {
    # If px column is provided and exists in the data, use it
    data <- data |> mutate(participant = .data[[participant]])
  } else {
    # If participant column is not provided or does not exist, create a participant column with 1s
    data <- data |> mutate(participant = 1)
  }
  ts_data <- make_bmiz_tsibble(data, age = age, bmiz = bmiz, px = 'participant', dob = dob, date_assessed = date_assessed, age_unit = age_unit)
  bmiz_fit <- fit_and_forecast_bmiz(ts_data)
  return(bmiz_fit)
}


