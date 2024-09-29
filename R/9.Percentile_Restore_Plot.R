

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

  start_date <- clean_data$static_data$tx_start_date
  dob <- clean_data$static_data$dob
  prediction_ht_in <- clean_data$static_data$prediction_ht_in
  x_start <- start_date - lubridate::days(30)
  x_end <- Sys.Date() + lubridate::days(30)

  forecast_data <- forecast_data %>%
    mutate(ePct = pnorm(eBMIz)*100,
      lower_ePCT = pnorm(lower_eBMIz)*100,
      upper_ePCT = pnorm(upper_eBMIz)*100,
      date_assessed = dob + lubridate::days(round(agemos * 30.4375))) %>%
    mutate(age_days = as.numeric(date_assessed - dob))

  ePct <- mean(forecast_data$ePct, na.rm = TRUE)
  lower_ePct <- mean(forecast_data$lower_ePCT, na.rm = TRUE)
  upper_ePct <- mean(forecast_data$upper_ePCT, na.rm = TRUE)

  start_bmi <- calculate_bmi(weight = clean_data$static_data$intake_wt_kgs, height = clean_data$static_data$adult_ht_in * 2.54)

    intake_age <- as.numeric(clean_data$static_data$tx_start_date - clean_data$static_data$dob)

    intake_pct <- pnorm(bmiz_lookup(bmi = start_bmi,
                sex = align_sex_coding(clean_data$static_data$sex)$sex,
                age_unit = 'days',
                age = intake_age))*100
# calculate pct data for all of the dynamic data

    vectorized_bmiz_lookup <- Vectorize(bmiz_lookup, vectorize.args = c("bmi", "age"))

    dynamic_data <- clean_data$dynamic_data %>%
      mutate(bmi = calculate_bmi(weight = weight_kgs, height = height_cm)) |>
      mutate(pct = pnorm(vectorized_bmiz_lookup(
          bmi = bmi,
          sex = align_sex_coding(clean_data$static_data$sex)$sex,
          age_unit = 'days',
          age = age_days))) |>
      mutate(pct = pct*100)

    dynamic_data <- dynamic_data |>
      filter(age_days >= clean_data$static_data$tx_start_age_days)

    start_pct <- dynamic_data$pct[1]
    current_pct <- dynamic_data$pct[nrow(dynamic_data)]
    current_date <- dynamic_data$date_assessed[nrow(dynamic_data)]

    # Create a dataframe with just x_start and x_end
    ribbon_data <- data.frame(
      date_assessed = c(x_start, x_end),
      lower_ePct = c(lower_ePct, lower_ePct),
      ePct = c(ePct, ePct),
      upper_ePct = c(upper_ePct, upper_ePct)
    )


    ggplot(dynamic_data, mapping = aes(x = date_assessed)) +
      geom_point(data = dynamic_data, mapping = aes(x = date_assessed, y = pct),
                 fill = embarktools::embark_colors[3],
                 col = embarktools::embark_colors[1],
                 size = 4,
                 shape = 21) +

      # Ribbon from -Inf to lower_ePct
      geom_ribbon(data = ribbon_data, aes(x = date_assessed, ymin = -Inf, ymax = lower_ePct),
                  fill = embarktools::embark_colors[2], alpha = 0.2) +

      # Ribbon between lower_ePct and ePct
      geom_ribbon(data = ribbon_data, aes(x = date_assessed, ymin = lower_ePct, ymax = ePct),
                  fill = embarktools::embark_colors[6], alpha = 0.2) +

      # Ribbon between ePct and upper_ePct
      geom_ribbon(data = ribbon_data, aes(x = date_assessed, ymin = ePct, ymax = upper_ePct),
                  fill = embarktools::embark_colors[4], alpha = 0.2) +

      # Smoothed line for ePct
      geom_smooth(data = ribbon_data, aes(x = date_assessed, y = ePct), col = embarktools::embark_colors[4], se = FALSE) +

      # Smoothed dotted line for upper_ePct
      geom_smooth(data = ribbon_data, aes(x = date_assessed, y = upper_ePct), col = embarktools::embark_colors[4], linetype = 'dotted', se = FALSE) +

      # Smoothed dotted line for lower_ePct
      geom_smooth(data = ribbon_data, aes(x = date_assessed, y = lower_ePct), col = embarktools::embark_colors[2], linetype = 'dotted', se = FALSE) +

      # Vertical line for the start date
      geom_vline(xintercept = start_date, linetype = 'dashed', col = embarktools::embark_colors[1]) +

      # Custom x-axis scaling
      scale_x_date(limits = c(x_start, x_end), date_labels = "%b %Y") +
      annotate("text", x = start_date, y = upper_ePct, label = paste("Tx Start Date\n", format(start_date, "%b %d, %Y")),
      vjust = 0.5, angle = 90, color = embarktools::embark_colors[1], size = 4, hjust = 1) +
      annotate("text", x = x_end, y = upper_ePct, label = paste0("Predicted \n BMI Percentile: ", round(ePct, 0),"th \n", "Range: ", round(lower_ePct, 0), "th - ", round(upper_ePct, 0), "th"),
               vjust = 1, angle = 0, color = embarktools::embark_colors[1], size = 4, hjust = 1) +
     annotate ("text", x = start_date, y = start_pct, label = paste0("Intake:\n", round(start_pct, 0),"th"),
                vjust = -1, hjust = 0.5, color = embarktools::embark_colors[1], size = 3) +
   labs(title = "BMI Percentile Restoration", x = "", y = "BMI Percentile") +
      annotate("text", x = current_date, y = current_pct, label = paste0("Current:\n ", round(current_pct, 0),"th"),
               vjust = -1, hjust = 0.5,  color = embarktools::embark_colors[1], size = 3, hjust = 1) +
    geom_hline(mapping = aes(x = date_assessed, yintercept = ePct),
    col = embarktools::embark_colors[1], size = 0.5, linetype = 'dotted') +
    geom_hline(mapping = aes(x = date_assessed, yintercept = lower_ePct),
    col = embarktools::embark_colors[1], size = 0.5, linetype = 'dotted') +
   embarktools::embark_theme_a +
  theme(axis.text.x = element_text(angle = 35, size = 14, hjust = 1))

}

