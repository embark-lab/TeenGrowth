
#' Create tsibble for BMIz Data
#'
#' @param data A data frame containing the input data - cleaned per clean_data function
#' @import dplyr
#' @importFrom tsibble as_tsibble
#' @importFrom tsibble fill_gaps
#' @return A tsibble with the BMIz data.
#' @export
#'

make_bmiz_tsibble <- function(data) {
  ts_data <- data |>
    dplyr::select(agemos, bmiz, id)
  ts_data <- ts_data |>
    tsibble::as_tsibble(index = agemos, key = id) |>
    tsibble::fill_gaps()

  return(ts_data)
}


#' Fit User Defined Forecast for BMIz Data
#'
#' @param data A tsibble containing the BMIz data.
#' @param lower_margin Lower margin for the forecast.
#' @param upper_margin Upper margin for the forecast.
#' @param central_value Central value for the forecast. Options are "mean", "max", "most_recent", "mean+most_recent".
#' @importFrom tidyr tibble
#' @import dplyr
#' @return A forecast object for each id.
#' @export
user_defined_forecast <- function(data,
                                  lower_margin = NULL,
                                  upper_margin = NULL,
                                  central_value = c("mean", "max", "most_recent", "mean+most_recent")) {
  central_value <- match.arg(central_value)

  if (is.null(lower_margin) || is.null(upper_margin)) {
    stop("Please provide lower_margin and upper_margin.")
  }

  if (central_value == "mean") {
    central_bmiz <- mean(data$bmiz, na.rm = TRUE)
  } else if (central_value == "max") {
    central_bmiz <- max(data$bmiz, na.rm = TRUE)
  } else if (central_value == "most_recent") {
    central_bmiz <- data$bmiz[which.max(data$agemos)]
  } else if (central_value == "mean+most_recent") {
    central_bmiz <- (mean(data$bmiz, na.rm = TRUE) + max(data$bmiz, na.rm = TRUE)) / 2
  } else {
    stop("Invalid central_value. Choose 'mean', 'max', 'most_recent', or 'mean+most_recent'.")
  }

  forecast <- tibble(
    agemos = seq(max(data$agemos, na.rm = TRUE) + 1, by = 1, length.out = 120),
    .mean = central_bmiz,
    .lower = central_bmiz - lower_margin,
    .upper = central_bmiz + upper_margin,
    id = unique(data$id)[1]
  )
  User_Model <- paste0('(-', lower_margin, ', ', central_value, ', ', upper_margin, ')')
  forecast <- forecast |>
    mutate(!!User_Model := paste0("[", .lower, ", ", .upper, "]"))
  return(forecast)
}

#' Fit Models and Generate Forecasts for BMIz Data
#'
#' @param ts_data A tsibble containing the BMIz data.
#' @param central_value Central value for the forecast. Options are "mean", "max", "most_recent", "mean+most_recent".
#' @param ci Confidence interval for the forecast.
#' @param lower_margin Lower margin for the forecast.
#' @param upper_margin Upper margin for the forecast.
#' @return A forecast object for each id.
#' @import fable
#' @import fabletools
#' @import dplyr
#' @importFrom tsibble as_tsibble
#' @importFrom purrr map_dbl
#' @export
fit_and_forecast_bmiz <- function(ts_data,
                                  central_value = c("mean", "max", "most_recent", "mean+most_recent"),
                                  ci = 95,
                                  lower_margin = NULL,
                                  upper_margin = NULL) {
  central_value <- match.arg(central_value)

  fit_forecast <- function(data, model_name, model_formula, ci) {
    ci_col <- paste0(ci, "%")
    data %>%
      model(!!model_name := model_formula) %>%
      forecast(h = 120) %>%
      hilo(level = ci) %>%
      as_tibble() %>%
      mutate(agemos = as.integer(agemos)) %>%
      mutate(lower_eBMIz = map_dbl(.data[[ci_col]], ~ .x$lower),
             upper_eBMIz = map_dbl(.data[[ci_col]], ~ .x$upper)) %>%
      rename(eBMIz = .mean) %>%
      select(id, .model, agemos, eBMIz, lower_eBMIz, upper_eBMIz)
  }

  if (central_value == "mean" && (ci == 95 || ci == 99)) {
    bmiz_fit <- fit_forecast(ts_data, "mean", MEAN(bmiz), ci)

  # Defunct most_recent which uses the ARIMA model -- noted in simulations to produce low specificity -- replaced with below
  ##else if (central_value == "most_recent" && (ci == 95 || ci == 99)) {
 ## bmiz_fit <- fit_forecast(ts_data, "most_recent", ARIMA(bmiz ~ pdq(0,1,0)), ci)

  }  else if (central_value == "most_recent" && (ci == 95 || ci == 99)) {
      bmiz_fit <- fit_forecast(ts_data, "mean", MEAN(bmiz), ci)


      most_recent_bmiz <- as.data.frame(ts_data)%>%
        group_by(id) %>%
        arrange(desc(agemos)) %>%
        slice(1) %>%
        select(id, most_recent_bmiz = bmiz)

      bmiz_fit <- bmiz_fit %>%
        left_join(most_recent_bmiz, by = "id") %>%
        mutate(.most_recent = most_recent_bmiz,
               .diff = .most_recent - eBMIz,
               lower_eBMIz = lower_eBMIz + .diff,
               upper_eBMIz = upper_eBMIz + .diff,
               eBMIz = eBMIz + .diff) %>%
       select(id, .model, agemos, eBMIz, lower_eBMIz, upper_eBMIz)

 ## Also defunct for same reason -- replaced by below
# else if (central_value == "mean+most_recent" && (ci == 95 || ci == 99)) {
#     bmiz_fit <- ts_data %>%
#       model(
#         Mean = MEAN(bmiz),
#         ARIMA = ARIMA(bmiz ~ pdq(0,1,0))
#       ) %>%
#       mutate(ARIMA_mean = (ARIMA + Mean) / 2) %>%
#       forecast(h = 120) %>%
#       hilo(level = ci) %>%
#       as_tibble() %>%
#       mutate(agemos = as.integer(agemos),
#              lower_eBMIz = map_dbl(.data[[paste0(ci, "%")]], ~ .x$lower),
#              upper_eBMIz = map_dbl(.data[[paste0(ci, "%")]], ~ .x$upper)) %>%
#       rename(eBMIz = .mean) %>%
#       select(id, .model, agemos, eBMIz, lower_eBMIz, upper_eBMIz) %>%
#       filter(.model == "ARIMA_mean")
#
  }  else if (central_value == "mean+most_recent" && (ci == 95 || ci == 99)) {
      bmiz_fit <- fit_forecast(ts_data, "mean", MEAN(bmiz), ci)

      most_recent_bmiz <- as.data.frame(ts_data) %>%
        group_by(id) %>%
        arrange(desc(agemos)) %>%
        slice(1) %>%
        select(id, most_recent_bmiz = bmiz)

      mean_bmiz <- ts_data %>%
        group_by(id) %>%
        summarize(mean_bmiz = mean(bmiz, na.rm = TRUE))

      mean_most_recent_bmiz <- most_recent_bmiz %>%
        left_join(mean_bmiz, by = "id") %>%
        mutate(mean_most_recent_bmiz = (most_recent_bmiz + mean_bmiz) / 2)

      bmiz_fit <- bmiz_fit %>%
        left_join(mean_most_recent_bmiz, by = "id") %>%
        mutate(.mean_most_recent = mean_most_recent_bmiz,
               .diff = .mean_most_recent - eBMIz,
               lower_eBMIz = lower_eBMIz + .diff,
               upper_eBMIz = upper_eBMIz + .diff,
               eBMIz = eBMIz + .diff) %>%
        select(id, .model, agemos, eBMIz, lower_eBMIz, upper_eBMIz)
    } else if (central_value == "max" && (ci == 95 || ci == 99)) {
    bmiz_fit <- fit_forecast(ts_data, "max", MEAN(bmiz), ci)

    max_bmiz <- as.data.frame(ts_data) %>%
      group_by(id) %>%
      mutate(max_bmiz = max(bmiz, na.rm = TRUE)) %>%
      select(id, max_bmiz) %>%
      distinct()

    bmiz_fit <- bmiz_fit %>%
      left_join(max_bmiz, by = "id") %>%
      mutate(.max = max_bmiz,
             .diff = .max - eBMIz,
             lower_eBMIz = lower_eBMIz + .diff,
             upper_eBMIz = upper_eBMIz + .diff,
             eBMIz = eBMIz + .diff) %>%
      select(id, .model, agemos, eBMIz, lower_eBMIz, upper_eBMIz)
  } else if (ci == "User-Defined") {
    user_forecast <- ts_data %>%
      group_by(id) %>%
      do(user_defined_forecast(., lower_margin = lower_margin, upper_margin = upper_margin, central_value = central_value)) %>%
      ungroup() %>%
      mutate(.model = central_value)

    User_Model <- paste0('(-', lower_margin, ', ', central_value, ', ', upper_margin, ')')

    user_forecast <- user_forecast %>%
      separate(!!sym(User_Model), into = c("lower", "upper"), sep = ", ") %>%
      mutate(
        lower_eBMIz = as.numeric(sub("\\[", "", lower)),
        upper_eBMIz = as.numeric(sub("\\]", "", upper))
      ) %>%
      rename(eBMIz = .mean) %>%
      select(id, .model, agemos, eBMIz, lower_eBMIz, upper_eBMIz)

    bmiz_fit <- user_forecast
  } else {
    stop("Invalid model selection")
  }

  # Return the filtered data
  return(bmiz_fit)
}


#' Create and Forecast BMIz Data
#'
#' @param data A data frame containing the input data.
#' @param ci Confidence interval for the forecast.
#' @param lower_margin Lower margin for the forecast.
#' @param upper_margin Upper margin for the forecast.
#' @param central_value Central value for the forecast. Options are "mean", "max", "most_recent", "mean+most_recent".
#' @return A forecast object for each id.
#' @export
make_bmiz_forecast <- function(data,
                               ci = 95,
                               lower_margin = NULL,
                               upper_margin = NULL,
                               central_value = c("mean", "max", "most_recent", "mean+most_recent")) {
  central_value <- match.arg(central_value)

  ts_data <- make_bmiz_tsibble(data)
  bmiz_fit <- fit_and_forecast_bmiz(ts_data,
                                    ci = ci,
                                    lower_margin = lower_margin,
                                    upper_margin = upper_margin,
                                    central_value = central_value)
  return(bmiz_fit)
}
