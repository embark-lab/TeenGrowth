
#' Weight Restoration Planning Plot
#' @param clean_data a cleaned dataframe - cleaned by the tx_plot_clean function
#' @param forecast_data a forecasted dataframe for 1 participant
#' @param slope_per_week the slope of weight gain expected in lbs per week
#' @return a treatment plan plot
#' @import dplyr
#' @import lubridate
#' @import rlang
#' @export

Wt_Restore_Plot <- function(clean_data, forecast_data, slope_per_week = 0.5) {

  start_date <- clean_data$static_data$tx_start_date
  dob <- clean_data$static_data$dob
  prediction_ht_in <- clean_data$static_data$prediction_ht_in

  forecast_data <- forecast_data %>%
    mutate(
      eWeight = eBMI * (prediction_ht_in^2) / 703,
      upper_eWeight = upper_eBMI * (prediction_ht_in^2) / 703,
      lower_eWeight = lower_eBMI * (prediction_ht_in^2) / 703,
      AN_cutoff_wt = UW_cutoff_bmi * (prediction_ht_in^2) / 703,
      date_assessed = dob + lubridate::days(round(agemos * 30.4375))
    ) %>%
    mutate(age_days = as.numeric(date_assessed - dob))

  # Calculate the slope line data
  start_weight <- clean_data$static_data$intake_wt_lbs
  slope <- slope_per_week / 7  # Convert lbs per week to lbs per day

  # Find the crossing point
  forecast_data <- forecast_data %>%
    arrange(date_assessed) %>%
    mutate(predicted_abline = start_weight + slope * as.numeric(difftime(date_assessed, start_date, units = "days")),
           crossing_point = predicted_abline >= eWeight)

  crossing_row <- forecast_data %>%
    filter(crossing_point) %>%
    slice(1)

  # Determine end date
  if (nrow(crossing_row) > 0) {
    end_date <- crossing_row$date_assessed + months(3)
    crossing_date <- crossing_row$date_assessed
    crossing_weight <- crossing_row$eWeight
  } else {
    end_date <- start_date + lubridate::days(round(365 * 1.5))
    crossing_date <- NA
    crossing_weight <- NA
  }

  # Filter data for the line
  line_data <- forecast_data %>%
    filter(date_assessed >= start_date & date_assessed <= crossing_date)

  # Calculate x_start and x_end
  x_start <- start_date - lubridate::days(90)
  x_end <- end_date

  # Filter forecast_data and clean_data$dynamic_data within date range
  forecast_data_filtered <- forecast_data %>%
    filter(date_assessed >= x_start & date_assessed <= x_end)
  dynamic_data_filtered <- clean_data$dynamic_data %>%
    filter(date_assessed >= start_date & date_assessed <= x_end)

  # Set y-axis limits
  y_min <- min(forecast_data_filtered$lower_eWeight, dynamic_data_filtered$weight_lbs, na.rm = TRUE) - 10
  y_max <- max(forecast_data_filtered$upper_eWeight, dynamic_data_filtered$weight_lbs, na.rm = TRUE) + 10

  # Calculate midpoint for label placement
  annotation_row <- forecast_data_filtered %>%
    arrange(abs(difftime(date_assessed, start_date))) %>%
    slice(1)
  annotation_date <- annotation_row$date_assessed
  annotation_weight <- annotation_row$eWeight

  # make a mini dataframe with the start weight and EBW goal weight and their dates
  line_data_1 <- data.frame(date_assessed = c(start_date, crossing_date),
                            weight_lbs = c(start_weight, crossing_weight))

  p <- ggplot(forecast_data_filtered, mapping = aes(x = date_assessed)) +
    geom_ribbon(aes(ymin = -Inf, ymax = lower_eWeight), fill = embarktools::embark_colors[2], alpha = 0.2) +
    geom_ribbon(aes(ymin = lower_eWeight, ymax = eWeight), fill = embarktools::embark_colors[6], alpha = 0.2) +
    geom_ribbon(aes(ymin = eWeight, ymax = upper_eWeight), fill = embarktools::embark_colors[4], alpha = 0.2) +
    geom_smooth(mapping = aes(y = eWeight), col = embarktools::embark_colors[4], se = FALSE) +
    geom_smooth(mapping = aes(y = upper_eWeight), col = embarktools::embark_colors[4], linetype = 'dotted', se = FALSE) +
    geom_smooth(mapping = aes(y = lower_eWeight), col = embarktools::embark_colors[2], linetype = 'dotted', se = FALSE) +
    geom_vline(xintercept = start_date, linetype = 'dashed', col = embarktools::embark_colors[1]) +
    scale_x_date(limits = c(x_start, x_end), date_labels = "%b %Y") +
    scale_y_continuous(limits = c(y_min, y_max)) +
    geom_point(data = dynamic_data_filtered, mapping = aes(x = date_assessed, y = weight_lbs),
               fill = embarktools::embark_colors[3],
               col = embarktools::embark_colors[1],
               size = 4,
               shape = 21) +
    geom_line(data = line_data_1, mapping = aes(x = date_assessed, y = weight_lbs),
              col = embarktools::embark_colors[1], linetype = "dotted", size = 2) +
    geom_point(data = crossing_row, mapping = aes(x = date_assessed, y = eWeight),
               col = embarktools::embark_colors[1], size = 5,
               fill = embarktools::embark_colors[3],
               shape = 21) +
    geom_point(aes(x = start_date, y = start_weight),
               col = embarktools::embark_colors[1],
               fill = embarktools::embark_colors[2],
               size = 5, shape = 21) +
    geom_text(data = crossing_row, mapping = aes(x = date_assessed + 60, y = eWeight,
                                                 label = paste("EBW Goal:",
                                                               format(date_assessed, "%b %d, %Y"),
                                                               "\nWeight:", round(eWeight, 1))),
              vjust = -1, hjust = 1, color = embarktools::embark_colors[1], size = 5) +
    geom_text(aes(x = annotation_date, y = annotation_weight, label = "Expected Weight"),
              vjust = -1, hjust = 0.5, color = embarktools::embark_colors[4], size = 5) +
    geom_text(aes(x = start_date, y = start_weight,
                  label = paste("Start Weight:\n", round(start_weight, 1))),
              vjust = 2, hjust = 0.5, color = embarktools::embark_colors[1], size = 4) +
    annotate("text", x = start_date, y = y_max, label = paste("Tx Start Date\n", format(start_date, "%b %d, %Y")),
             vjust = 0.5, angle = 90, color = embarktools::embark_colors[1], size = 4, hjust = 1) +
    labs(title = "Weight Restoration Plan", x = "", y = "Weight (lbs)") +
    embarktools::embark_theme_a +
    theme(axis.text.x = element_text(angle = 35, size = 14, hjust = 1))

  # Add a line of best fit through the dynamic data points if there are more than 3
  if (nrow(dynamic_data_filtered) > 3) {
    best_fit <- lm(weight_lbs ~ date_assessed, data = dynamic_data_filtered)
    slope_best_fit <- coef(best_fit)["date_assessed"] * 7  # Convert slope to lbs per week
    last_point <- dynamic_data_filtered %>% arrange(desc(date_assessed)) %>% slice(1)

    p <- p +
      geom_smooth(data = dynamic_data_filtered, aes(x = date_assessed, y = weight_lbs),
                  method = "lm", se = FALSE, col = embarktools::embark_colors[3],
                  linetype = "dotted") +
      geom_text(aes(x = last_point$date_assessed + 30, y = last_point$weight_lbs,
                    label = paste("Current Trend: \n", round(slope_best_fit, 2), "lbs/week")),
                vjust = 0.5, hjust = 0, color = embarktools::embark_colors[1], size = 5)
  }

  # Label the expected trend line with slope specification
  midpoint_abline <- line_data %>%
    arrange(abs(difftime(date_assessed, start_date+90))) %>%
    slice(1)
  p <- p +
    geom_text(aes(x = midpoint_abline$date_assessed , y = midpoint_abline$predicted_abline,
                  label = paste("Expected Trend: \n", round(slope_per_week, 2), "lbs/week")),
              vjust = -1, hjust = 0.5, color = embarktools::embark_colors[1], size = 4)

  print(p)
}

