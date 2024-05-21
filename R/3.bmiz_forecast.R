
#' Create tsibble for BMIz Data
#'
#' @param cleaned_data A data frame containing the input data - cleaned per clean_data function
#' @param dob Column name for date of birth (optional).
#' @import dplyr
#' @import tsibble
#' @param date_assessed Column name for date of assessment (optional).
#' @param age_unit Unit of age (optional).
#' @return A tsibble with the BMIz data.
#' @export
#'

make_bmiz_tsibble <- function(cleaned_data) {
  ts_data <- cleaned_data |>
    dplyr::select(agemos, bmiz, id)
  ts_data <- ts_data |>
    tsibble::as_tsibble(index = agemos, key = id) |>
    rename(id = id) |>
    tsibble::fill_gaps()

  return(ts_data)
}


#' Fit Models and Generate Forecasts for BMIz Data
#'
#' @param data A tsibble containing the BMIz data.
#' @param lower_margin Lower margin for the forecast.
#' @param upper_margin Upper margin for the forecast.
#' @param central_value Central value for the forecast.
#' @import tibble
#' @import dplyr
#' @return A forecast object for each id.
#' @export

user_defined_forecast <- function(data,  lower_margin = lower_margin,
                                  upper_margin = upper_margin,
                                  central_value = central_value) {
  User_Model <- paste0('(-', lower_margin,', ',central_value, ', ',upper_margin,')')
  if (central_value == "mean") {
    central_bmiz <- mean(data$bmiz, na.rm = TRUE)
  } else if (central_value == "max") {
    central_bmiz <- max(data$bmiz, na.rm = TRUE)
  } else if (central_value == "most_recent") {
    central_bmiz <- data$bmiz[which.max(data$agemos)]
  } else {
    stop("Invalid central_value. Choose 'mean', 'max', or 'most_recent'.")
  }

  forecast <- tibble(
    agemos = seq(max(data$agemos, na.rm = TRUE) + 1, max(data$agemos, na.rm = TRUE) + 60),
    .mean = central_bmiz,
    .lower = central_bmiz - lower_margin,
    .upper = central_bmiz + upper_margin,
    id = unique(data$id)[1] # Adjust length to match number of forecast periods
  )
  forecast <- forecast |>
    mutate(!!User_Model := paste0("[", .lower, ", ", .upper, "]"))
  # make a hilo object
  return(forecast)
}


#' Fit Models and Generate Forecasts for BMIz Data
#'
#' @param ts_data A tsibble containing the BMIz data.
#' @return A forecast object for each id.
#' @import fable
#' @import fabletools
#' @import dplyr
#' @import tsibble
#' @export

fit_and_forecast_bmiz <- function(ts_data,  lower_margin = lower_margin,
                                  upper_margin = upper_margin,
                                  central_value = central_value) {
  User_Model <- paste0('(-', lower_margin,', ',central_value, ', ',upper_margin,')')
  bmiz_fit <- ts_data %>%
    model(
      Mean = MEAN(bmiz),
      ARIMA = ARIMA(bmiz ~ pdq(0,1,0))
    )

  forecast_fit <- bmiz_fit %>%
    mutate(ARIMA_mean = (ARIMA + Mean) / 2) %>%
    forecast(h = 60) %>%
    hilo(level = c(95,99)) |>
    as_tibble() |>
    select(id, .model, agemos, .mean, `95%`, `99%`) |>
    mutate(agemos = as.integer(agemos))

  home_cooked_fit <- ts_data %>%
    group_by(id) %>%
    do(user_defined_forecast(., lower_margin = lower_margin, upper_margin = upper_margin, central_value = central_value)) %>%
    ungroup() %>%
    mutate(.model = "User Defined") %>%
    select(id, .model, agemos, .mean, !!User_Model)


  # Combine all forecasts
  combined_forecast <- bind_rows(forecast_fit, home_cooked_fit)

  return(combined_forecast)
}

#' Create and Forecast BMIz Data
#'
#' @param data A data frame containing the input data.
#' @param age Column name for age in the data frame.
#' @param bmiz Column name for BMIz in the data frame.
#' @param id Column name for id identifier.
#' @param dob Column name for date of birth (optional).
#' @param date_assessed Column name for date of assessment (optional).
#' @param age_unit Unit of age (optional).
#' @return A forecast object for each id.
#' @export

make_bmiz_forecast <- function(data,
                               age = age,
                               bmiz = bmiz,
                               id = id,
                               dob = NULL,
                               date_assessed = NULL,
                               age_unit = NULL,
                               lower_margin = lower_margin,
                               upper_margin = upper_margin,
                               central_value = central_value)
  {
  if (!missing(id) && id %in% names(data)) {
    data <- data |> mutate(id = .data[[id]])
  } else {
    data <- data |> mutate(id = 1)
  }
  ts_data <- make_bmiz_tsibble(data, age = age, bmiz = bmiz, id = id, dob = dob, date_assessed = date_assessed, age_unit = age_unit)
  bmiz_fit <- fit_and_forecast_bmiz(ts_data, lower_margin = lower_margin, upper_margin = upper_margin, central_value = central_value)
  return(bmiz_fit)
}
