#' @title m2y
#' @description Month to Year Conversion fuction for GGplot
#' @param age_m 'age in months'
#' @return year

m2y <- function(age_m){
  year <- (age_m/12)
  return(year)
}


#' @title plot_eBMI
#' @description plot eBMI
#' @param data 'data'
#' @param agemos 'age column'
#' @param sex '1 = Male, 2 = Female'
#' @param age 'age in months'
#' @param adult_height 'adult height in cm'
#' @import dplyr
#' @import ggplot2
#' @import embarktools
#' @return plot
#' @export


plot_eBMI <- function(clean_data,
                      forecast_data,
                      px) {
  data_1 <- clean_data %>%
    filter(id == px)
  data_2 <- forecast_data %>%
    filter(id == px)
  data_3 <- forecast_data %>%
    filter(id == px & !is.na(.model))
  data_2$age <- data_2$agemos/12
  # identify the starting and ending age of the prediction interval
  start_age <- min(data_3$agemos)
  end_age <- max(data_3$agemos)
  # identify the midpoint of the prediction interval
  mid_age <- round((start_age + end_age) / 2,0)
  # identify the bmi associated with the midpoint of the prediction interval
  mid_bmi <- data_2 %>%
    # get the age closest to the midpoint of the prediction interval -- may not be exact
    filter(agemos == mid_age) %>%
    # get the bmi associated with the age closest to the midpoint of the prediction interval
    pull(upper_eBMI)

  ggplot2::ggplot(data = data_2, mapping = aes(x = age, y = eBMI)) +
  geom_point(mapping = aes(agemos, bmi), data = data_1,
             stat = 'identity', position = 'identity',
             col = embarktools::embark_colors[1],
             fill = embarktools::embark_colors[3],
             size = 3,
             shape = 21) +
    stat_smooth(mapping = aes(x = agemos, y = bmi), method = lm, formula = y~x + poly(x,2) + poly(x,3),
                linetype = 'dotted', col = embarktools::embark_colors[3],
                data = data_1, se = FALSE, size = 1.5) +
    stat_smooth(mapping = aes(x = agemos, y = upper_eBMI), col = embarktools::embark_colors[4],
                linetype = 'dashed', data = data_2,  position = 'identity', size = 2 ) +
    stat_smooth(mapping = aes(x = agemos, y = lower_eBMI), col = embarktools::embark_colors[4],
                linetype = 'dashed', data = data_2,  position = 'identity', size = 2 ) +
    # shade the inside of the confidence interval
    geom_ribbon(mapping = aes(x = agemos, ymin = lower_eBMI, ymax = upper_eBMI), data = data_2,
                fill = embarktools::embark_colors[4], alpha = 0.3) +
     stat_smooth(mapping = aes (x = agemos, y =eBMI), data = data_2, col = embarktools::embark_colors[3],
                 size = 2) +
    scale_x_continuous(breaks = 12*0:240, label = m2y(12*0:240)) +
stat_smooth(mapping = aes(x = agemos, y = median_bmi), data = forecast_data, col = embarktools::embark_colors[1], linetype = 'dotted', position = 'identity')+
 stat_smooth(mapping = aes(x = agemos, y = UW_cutoff_bmi), data = forecast_data, col = embarktools::embark_colors[2], linetype = 'dotted', position = 'identity' )+
 xlab('Age') +
ylab('Body Mass Index') +
    coord_cartesian(ylim = c(12, 30)) +
    embarktools::embark_theme_a +
    # label the stat_smooth orange line
    annotate("text", x = 48, y = 22, label = "Median BMI", color = embarktools::embark_colors[1],
             # make the text slanted
             angle = -45) +
    # label the stat_smooth red line
    annotate("text", x = 54, y = 16.5, label = "Underweight BMI", color = embarktools::embark_colors[2],
             angle = -40) +
    # Add graph title
    ggtitle("Expected BMI across Age") +
    # add in some grid lines
    theme(panel.grid.major = element_line(colour = "grey", linetype = "dotted")) +
    # label in the middle of the prediction interval line
    annotate("text", x = mid_age, y = mid_bmi + 1, label = "Prediction Window",
             color = embarktools::embark_colors[4],
             size = 5,
             angle = 20)
}
# create a weight plot

#' Plot Weight Forecast
#'
#' This function plots the weight forecast data for a given participant, model, and confidence interval.
#'
#' @param forecast_data A data frame containing the forecast data.
#' @param model The model type used for forecasting.
#' @param ci The confidence interval level.
#' @param px The participant ID to filter the data.
#' @param a_height Optional parameter to specify the adult height if it is not available in the data.
#' @return A ggplot object displaying the weight forecast.
#' @import ggplot2
#' @import dplyr
#' @export
#' @examples
#' # Assuming forecast_data is your dataframe and embarktools is loaded
#' # plot_weight(forecast_data, "ARIMA", "95%", 1, a_height = 65)
plot_weight <- function(clean_data,
                        forecast_data,
                        px,
                        a_height = NULL) {
  # Filter the data based on input parameters
  data <- forecast_data %>%
    filter(id == px & agemos >= 14 * 12)
  # Assign adult height if it is NA
  if (!is.null(a_height)) {
    data <- data %>%
      mutate(adult_height = ifelse(is.na(adult_height), a_height, adult_height))
  }
  # Calculate any weights after age 14 for girls and 16 for boys using the BMI and adult height from the clean data
  clean_data <- clean_data %>%
    filter(id == px & agemos >= 14 * 12)
    if (!is.null(a_height)) {
      clean_data <- clean_data %>%
        mutate(adult_height = ifelse(is.na(adult_height), a_height, adult_height))
    }

  # Calculate weights using BMI and adult height
  data <- data %>%
    mutate(
      eWeight = eBMI * (adult_height^2) / 703,
      upper_eWeight = upper_eBMI * (adult_height^2) / 703,
      lower_eWeight = lower_eBMI * (adult_height^2) / 703,
      AN_cutoff_wt = UW_cutoff_bmi * (adult_height^2) / 703
    )
  data$age = data$agemos / 12

  # same for clean data
  clean_data <- clean_data |>
    mutate(weight = bmi * (adult_height^2) / 703)
  clean_data$age = clean_data$agemos / 12

  # Plot the data
  ggplot2::ggplot(data = data, mapping = aes(x = age, y = eWeight)) +
    stat_smooth(mapping = aes(y = upper_eWeight), col = embarktools::embark_colors[4], linetype = 'dashed') +
    stat_smooth(mapping = aes(y = lower_eWeight), col = embarktools::embark_colors[4], linetype = 'dashed') +
    stat_smooth(mapping = aes(y = eWeight), col = embarktools::embark_colors[3]) +
    # add points but only every 6 months
    geom_point(data = data %>% filter(agemos %% 6 == 0),
               aes(x = age, y = eWeight),
               size = 3,
               col = embarktools::embark_colors[1],
               fill = embarktools::embark_colors[3],
               shape = 21) +
    # add points for the clean data
    geom_point(data = clean_data,
               aes(x = age, y = weight),
               size = 3,
               col = embarktools::embark_colors[2],
               fill = embarktools::embark_colors[2],
               shape = 21) +
    scale_x_continuous(breaks = seq(14, max(data$age), by = 1),
                       labels = seq(14, max(data$age), by = 1)) +
    stat_smooth(mapping = aes(y = AN_cutoff_wt), col = embarktools::embark_colors[2], linetype = 'dotted') +
    # add geom_ribbon to shade the confidence interval
    geom_ribbon(mapping = aes(ymin = lower_eWeight, ymax = upper_eWeight), fill = embarktools::embark_colors[4], alpha = 0.3) +
    # title
    ggtitle(paste("Weight Prediction")) +
    xlab('Age (years)') +
    ylab('Weight (lbs)') +
 #   coord_cartesian(ylim = c(min(data$lower_eWeight, na.rm = TRUE), max(data$upper_eWeight, na.rm = TRUE))) +
    embarktools::embark_theme_a +
    # add in some grid lines
    theme(panel.grid.major = element_line(colour = "grey", linetype = "dotted")) +
    scale_color_manual(
      values = c(
        "Upper eWeight" = embarktools::embark_colors[4],
        "Lower eWeight" = embarktools::embark_colors[4],
        "eWeight" = embarktools::embark_colors[3],
        "Forecasted Data" = embarktools::embark_colors[1],
        "Actual Data" = embarktools::embark_colors[2],
        "AN Cutoff Weight" = embarktools::embark_colors[2]
      ),
      name = "Legend"
    ) +
    scale_fill_manual(
      values = c("Confidence Interval" = embarktools::embark_colors[4]),
      name = "Legend"
    ) +
    theme(legend.position = "bottom")
}

# Example Usage:
# Assuming forecast_data is your dataframe and embarktools is loaded
# plot_weight(forecast_data, "ARIMA", "95%", 1, a_height = 65)
