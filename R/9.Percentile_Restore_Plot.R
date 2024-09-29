

#' BMI Percentile Restoration Planning Plot
#' @param clean_data a cleaned dataframe - cleaned by the tx_plot_clean function
#' @param forecast_data a forecasted dataframe for 1 participant
#' @param slope_per_week the slope of weight gain expected in lbs per week
#' @return a treatment plan plot
#' @import dplyr
#' @import lubridate
#' @import rlang
#' @import ggplot2
#' @importFrom stats approx coef lm qnorm
#' @export

Pct_Restore_Plot <- function(clean_data, forecast_data) {
  # Check if required data is present
  stopifnot(!is.null(clean_data), !is.null(forecast_data))

  # Extract static data
  start_date <- clean_data$static_data$tx_start_date
  dob <- clean_data$static_data$dob
  prediction_ht_in <- clean_data$static_data$prediction_ht_in

  # Check if dates are in Date format
  if (!inherits(start_date, "Date") || !inherits(dob, "Date")) {
    stop("start_date and dob must be Date objects")
  }

  # Define plotting range
  x_start <- start_date - lubridate::days(30)
  x_end <- Sys.Date() + lubridate::days(60)

  # Ensure forecast_data has necessary columns
  required_cols <- c("eBMIz", "lower_eBMIz", "upper_eBMIz", "agemos")
  missing_cols <- setdiff(required_cols, colnames(forecast_data))
  if (length(missing_cols) > 0) {
    stop("forecast_data is missing columns: ", paste(missing_cols, collapse = ", "))
  }

  # Prepare forecast_data
  forecast_data <- forecast_data %>%
    mutate(
      ePct = pnorm(eBMIz) * 100,
      lower_ePCT = pnorm(lower_eBMIz) * 100,
      upper_ePCT = pnorm(upper_eBMIz) * 100,
      date_assessed = dob + lubridate::days(round(agemos * 30.4375))
    ) %>%
    mutate(age_days = as.numeric(date_assessed - dob))

  # Debug: Check forecast_data after mutation
  cat("Forecast data after processing:\n")
  print(head(forecast_data))

  # Calculate mean percentiles
  ePct <- mean(forecast_data$ePct, na.rm = TRUE)
  lower_ePct <- mean(forecast_data$lower_ePCT, na.rm = TRUE)
  upper_ePct <- mean(forecast_data$upper_ePCT, na.rm = TRUE)

  # Check for NA values in percentiles
  if (is.na(ePct) || is.na(lower_ePct) || is.na(upper_ePct)) {
    stop("Percentile calculations resulted in NA values. Check forecast_data for missing or invalid values.")
  }

  # Calculate starting BMI
  start_bmi <- calculate_bmi(
    weight = clean_data$static_data$intake_wt_kgs,
    height = clean_data$static_data$adult_ht_in * 2.54
  )

  # Debug: Output start_bmi
  cat("Start BMI:", start_bmi, "\n")

  # Calculate intake age in days
  intake_age <- as.numeric(clean_data$static_data$tx_start_date - clean_data$static_data$dob)

  # Check if intake_age is positive
  if (intake_age <= 0) {
    stop("intake_age is not positive. Check tx_start_date and dob.")
  }

  # Calculate intake percentile
  intake_pct <- pnorm(bmiz_lookup(
    bmi = start_bmi,
    sex = clean_data$static_data$sex,
    age_unit = 'days',
    age = intake_age
  )) * 100

  # Debug: Output intake_pct
  cat("Intake BMI percentile:", intake_pct, "\n")

  # Calculate percentile data for all dynamic data
  vectorized_bmiz_lookup <- function(bmi, sex, age, data_source = 'cdc', age_unit = NULL, dob = NULL, date_assessed = NULL) {
    mapply(bmiz_lookup, bmi = bmi, sex = sex, age = age, MoreArgs = list(data_source = data_source, age_unit = age_unit, dob = dob, date_assessed = date_assessed))
  }
  # Create a vector of sex values
  sex_vector <- rep(1, nrow(clean_data$dynamic_data))

  dynamic_data <- clean_data$dynamic_data %>%
    mutate(
      bmi = calculate_bmi(weight = weight_kgs, height = height_cm),
      pct = pnorm(vectorized_bmiz_lookup(
        bmi = bmi,
        sex = sex_vector,
        age_unit = 'days',
        age = age_days
      )) * 100
    )

  # Debug: Check dynamic_data after mutation
  cat("Dynamic data after adding bmi and pct:\n")
  print(tail(dynamic_data))

  # Filter data for plotting
  dynamic_data <- dynamic_data %>%
    filter(age_days >= clean_data$static_data$tx_start_age_days)

  # Check if dynamic_data is not empty after filtering
  if (nrow(dynamic_data) == 0) {
    stop("No data available after filtering. Check tx_start_age_days and dynamic_data.")
  }

  # Get start and current percentiles and dates
  start_pct <- dynamic_data$pct[1]
  current_pct <- dynamic_data$pct[nrow(dynamic_data)]
  current_date <- dynamic_data$date_assessed[nrow(dynamic_data)]

  # Debug: Output start and current percentiles
  cat("Start percentile:", start_pct, "\n")
  cat("Current percentile:", current_pct, "\n")

  # Create ribbon data for plotting
  ribbon_data <- data.frame(
    date_assessed = c(x_start, x_end),
    lower_ePct = c(lower_ePct, lower_ePct),
    ePct = c(ePct, ePct),
    upper_ePct = c(upper_ePct, upper_ePct)
  )

  # Function to get ordinal suffix
  ordinal_suffix <- function(n) {
    if (n %% 100 %in% 11:13) {
      return('th')
    } else if (n %% 10 == 1) {
      return('st')
    } else if (n %% 10 == 2) {
      return('nd')
    } else if (n %% 10 == 3) {
      return('rd')
    } else {
      return('th')
    }
  }

  # Round percentiles and get suffixes
  ePct_rounded <- round(ePct, 0)
  lower_ePct_rounded <- round(lower_ePct, 0)
  upper_ePct_rounded <- round(upper_ePct, 0)
  start_pct_rounded <- round(start_pct, 0)
  current_pct_rounded <- round(current_pct, 0)

  ePct_suffix <- ordinal_suffix(ePct_rounded)
  lower_ePct_suffix <- ordinal_suffix(lower_ePct_rounded)
  upper_ePct_suffix <- ordinal_suffix(upper_ePct_rounded)
  start_pct_suffix <- ordinal_suffix(start_pct_rounded)
  current_pct_suffix <- ordinal_suffix(current_pct_rounded)

  # Debug: Output rounded percentiles with suffixes
  cat("Predicted BMI Percentile:", ePct_rounded, ePct_suffix, "\n")
  cat("Intake Percentile:", start_pct_rounded, start_pct_suffix, "\n")
  cat("Current Percentile:", current_pct_rounded, current_pct_suffix, "\n")

  ggplot(dynamic_data, mapping = aes(x = date_assessed)) +
    geom_point(
      data = dynamic_data,
      mapping = aes(x = date_assessed, y = pct),
      fill = embarktools::embark_colors[3],
      col = embarktools::embark_colors[1],
      size = 4,
      shape = 21
    ) +
    geom_smooth(
      data = dynamic_data,
      mapping = aes(x = date_assessed, y = pct),
      col = embarktools::embark_colors[1],
      size = 1,
      linetype = 'dotted',
      se = FALSE
    ) +
    # Ribbons representing percentile ranges
    geom_ribbon(
      data = ribbon_data,
      aes(x = date_assessed, ymin = -Inf, ymax = lower_ePct),
      fill = embarktools::embark_colors[2],
      alpha = 0.2
    ) +
    geom_ribbon(
      data = ribbon_data,
      aes(x = date_assessed, ymin = lower_ePct, ymax = ePct),
      fill = embarktools::embark_colors[6],
      alpha = 0.2
    ) +
    geom_ribbon(
      data = ribbon_data,
      aes(x = date_assessed, ymin = ePct, ymax = upper_ePct),
      fill = embarktools::embark_colors[4],
      alpha = 0.2
    ) +

    # Smoothed lines for expected percentiles
    geom_smooth(
      data = ribbon_data,
      aes(x = date_assessed, y = ePct),
      col = embarktools::embark_colors[4],
      se = FALSE
    ) +
    geom_smooth(
      data = ribbon_data,
      aes(x = date_assessed, y = upper_ePct),
      col = embarktools::embark_colors[4],
      linetype = 'dotted',
      se = FALSE
    ) +
    geom_smooth(
      data = ribbon_data,
      aes(x = date_assessed, y = lower_ePct),
      col = embarktools::embark_colors[2],
      linetype = 'dotted',
      se = FALSE
    ) +

    # Vertical line for the start date
    geom_vline(
      xintercept = start_date,
      linetype = 'dashed',
      col = embarktools::embark_colors[1]
    ) +

    # Custom x-axis scaling
    scale_x_date(limits = c(x_start, x_end), date_labels = "%b %Y") +
    annotate(
      "text",
      x = start_date,
      y = upper_ePct,
      label = paste("Tx Start Date\n", format(start_date, "%b %d, %Y")),
      vjust = 0.5,
      angle = 90,
      color = embarktools::embark_colors[1],
      size = 4,
      hjust = 1
    ) +
    annotate(
      "text",
      x = x_end,
      y = upper_ePct,
      label = paste0(
        "Predicted BMI Percentile: ",
        ePct_rounded, ePct_suffix, " \n",
        "Range: ",
        lower_ePct_rounded, lower_ePct_suffix, " - ",
        upper_ePct_rounded, upper_ePct_suffix
      ),
      vjust = 1,
      angle = 0,
      color = embarktools::embark_colors[1],
      size = 4,
      hjust = 1
    ) +
    annotate(
      "text",
      x = start_date,
      y = start_pct,
      label = paste0("Intake:\n", start_pct_rounded, start_pct_suffix),
      vjust = -1,
      hjust = 0.5,
      color = embarktools::embark_colors[1],
      size = 3
    ) +
    labs(title = "BMI Percentile Restoration", x = "", y = "BMI Percentile") +
    annotate(
      "text",
      x = current_date,
      y = current_pct,
      label = paste0("Current:\n", current_pct_rounded, current_pct_suffix),
      vjust = -1,
      hjust = 0.5,
      color = embarktools::embark_colors[1],
      size = 3,
      hjust = 1
    ) +
    geom_hline(
      mapping = aes(x = date_assessed, yintercept = ePct),
      col = embarktools::embark_colors[1],
      size = 0.5,
      linetype = 'dotted'
    ) +
    geom_hline(
      mapping = aes(x = date_assessed, yintercept = lower_ePct),
      col = embarktools::embark_colors[1],
      size = 0.5,
      linetype = 'dotted'
    ) +
    embarktools::embark_theme_a +
    theme(axis.text.x = element_text(angle = 35, size = 14, hjust = 1))
}
