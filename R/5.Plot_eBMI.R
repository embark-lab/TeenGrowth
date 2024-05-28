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
#' @param adult_height_in 'adult height in in'
#' @import dplyr
#' @import ggplot2
#' @import embarktools
#' @import ggpubr
#' @return plot
#' @export


plot_eBMI <- function(clean_data,
                      forecast_data,
                      px,
                      ed_aoo = NA) {

  # make ed aao a column in the data if it is provided
  if (!is.na(ed_aoo) & !is.null(ed_aoo)) {
    clean_data$ed_aoo <- ed_aoo*12
  }
  # if there is no column named ed_aoo, then make it == 10000
  else if (!"ed_aoo" %in% colnames(clean_data)) {
    clean_data$ed_aoo <- 10000
  }
  else {
    clean_data$ed_aoo <- 10000 }

    post_ed <- clean_data |>
      filter(id == px & agemos >= ed_aoo)

    pre_ed <- clean_data |>
      filter(id == px & agemos < ed_aoo)

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

  p <- ggplot2::ggplot(data = data_2, mapping = aes(x = age, y = eBMI)) +
    geom_point(mapping = aes(agemos, bmi), data = pre_ed,
               stat = 'identity', position = 'identity',
               col = embarktools::embark_colors[1],
               fill = embarktools::embark_colors[3],
               size = 3,
               shape = 21) +
   # if the post_ed data is not empty, plot the post_ed data
    stat_smooth(mapping = aes(x = agemos, y = bmi), method = lm, formula = y~x + poly(x,2),
                linetype = 'dotted', col = embarktools::embark_colors[3],
                data = pre_ed, se = FALSE, size = 1.5) +
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
    stat_smooth(mapping = aes(x = agemos, y = UW_cutoff_bmi), data = forecast_data, col = embarktools::embark_colors[6], linetype = 'dotted', position = 'identity' )+
    xlab('Age') +
    ylab('Body Mass Index') +
    coord_cartesian(ylim = c(12, 30)) +
    embarktools::embark_theme_a +
    # label the stat_smooth orange line
    annotate("text", x = 48, y = 22, label = "Median BMI", color = embarktools::embark_colors[1],
             # make the text slanted
             angle = -45) +
    # label the stat_smooth red line
    annotate("text", x = 54, y = 16.5, label = "Underweight BMI", color = embarktools::embark_colors[6],
             angle = -40) +
    # Add graph title
    ggtitle("Expected BMI across Age") +
    # add in some grid lines
    theme(panel.grid.major = element_line(colour = "grey", linetype = "dotted")) +
    theme(axis.title.y = element_text(margin = margin(r = 10)),
          axis.title.x = element_text(margin = margin(t = 10)))
  # label in the middle of the prediction interval line
  # annotate("text", x = mid_age, y = mid_bmi + 1, label = "Prediction Window",
  #          color = embarktools::embark_colors[4],
  #          size = 5,
  #          angle = 20)


  if (nrow(post_ed) > 0) {
   p <- p+ geom_point(mapping = aes(agemos, bmi), data = post_ed,
               stat = 'identity', position = 'identity',
               col = embarktools::embark_colors[1],
               fill = embarktools::embark_colors[2],
               size = 3,
               shape = 21) }

  # Create the legend plot
  legend_data <- data.frame(
    label = factor(c("Prediction Window", "Pre-ED BMI / Expected BMI", "BMI post ED onset"),
                   levels = c("Prediction Window", "Pre-ED BMI / Expected BMI", "BMI post ED onset"))
  )

  # Define the colors for the legend
  colors <- c(
    "Prediction Window" = "#A481C7",  # Lavender
    "Pre-ED BMI / Expected BMI" = "#C2B824",  # Chartreuse
    "BMI post ED onset" = "#EF6C45"  # Coral
  )

  # Create legend components
  prediction_window_legend <- ggplot(data.frame(x = c(1, 2), y = c(1, 1)), aes(x = x, y = y)) +
    geom_ribbon(aes(ymin = 0.5, ymax = 1.5), fill = colors["Prediction Window"], alpha = 0.3) +
    theme_void() +
    ggtitle("Prediction Window") +
    theme(plot.title = element_text(hjust = 0.5, size = 12,
                                    color = embarktools::embark_colors[1],
                                    family = 'Avenir'))

  pre_ed_legend <- ggplot(data.frame(x = 1, y = 1), aes(x = x, y = y)) +
    geom_point(size = 5, shape = 21, color = colors["Pre-ED BMI / Expected BMI"], fill = colors["Pre-ED BMI / Expected BMI"]) +
    theme_void() +
    ggtitle("Pre-ED BMI / Expected BMI") +
    theme(plot.title = element_text(hjust = 0.5, size = 12,
                                    color = embarktools::embark_colors[1],
                                    family = 'Avenir'))

  post_ed_legend <- ggplot(data.frame(x = 1, y = 1), aes(x = x, y = y)) +
    geom_point(size = 5, shape = 21, color = colors["BMI post ED onset"], fill = colors["BMI post ED onset"]) +
    theme_void() +
    ggtitle("BMI post ED onset") +
    theme(plot.title = element_text(hjust = 0.5, size = 12,
                                    color = embarktools::embark_colors[1],
                                    family = 'Avenir'))

  # Combine the custom legend components
  custom_legend <- ggpubr::ggarrange(
    prediction_window_legend, pre_ed_legend, post_ed_legend,
    ncol = 3, nrow = 1,
    common.legend = TRUE, legend = "none"
  )

  # Combine the main plot and the custom legend
  final_plot <- ggpubr::ggarrange(p, custom_legend, ncol = 1, heights = c(10, 1))
  return(final_plot)
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
#' @import ggpubr
#' @export
#' @examples
#' # Assuming forecast_data is your dataframe and embarktools is loaded
#' # plot_weight(forecast_data, "ARIMA", "95%", 1, a_height = 65)

plot_weight <- function(clean_data, forecast_data, px, ed_aoo = NA, a_height = NULL) {
  # if sex is 1, plot since age 16, if sex is 2, plot since age 14


  # Ensure data does not contain non-finite values
  forecast_data <- forecast_data %>%
    filter(is.finite(eWeight) & is.finite(upper_eWeight) & is.finite(lower_eWeight))

  # Assign adult height if it is NA
  if (!is.null(a_height)) {
    clean_data <- clean_data %>%
      mutate(adult_height_in = ifelse(is.na(adult_height_in), a_height, adult_height_in))
  }

  # Make ed_aoo a column in the data if it is provided
  if (!is.na(ed_aoo) & !is.null(ed_aoo)) {
    clean_data$ed_aoo <- ed_aoo * 12
  } else if (!"ed_aoo" %in% colnames(clean_data)) {
    clean_data$ed_aoo <- 10000
  } else {
    clean_data$ed_aoo <- 10000
  }

  # Same for clean data
  clean_data <- clean_data %>%
    mutate(weight = bmi * (adult_height_in^2) / 703,
           age = agemos / 12) %>%
  filter(is.finite(weight) & id == px)

  if (clean_data$sex[1] == 1) {
    age_start <- 16
  } else {
    age_start <- 14
  }
  clean_data <- clean_data |>
    filter(agemos >= (age_start * 12))

  # Filter the data based on input parameters
  data <- forecast_data %>%
    filter(id == px & agemos >= (age_start * 12))

  # Calculate weights using BMI and adult height
  data <- data %>%
    mutate(
      eWeight = eBMI * (adult_height_in^2) / 703,
      upper_eWeight = upper_eBMI * (adult_height_in^2) / 703,
      lower_eWeight = lower_eBMI * (adult_height_in^2) / 703,
      AN_cutoff_wt = UW_cutoff_bmi * (adult_height_in^2) / 703
    )
  data$age <- data$agemos / 12

  post_ed <- clean_data %>%
    filter(agemos >= ed_aoo)
  pre_ed <- clean_data %>%
    filter(agemos < ed_aoo)

  # Plot the data
  p <- ggplot2::ggplot(data = data, mapping = aes(x = age, y = eWeight)) +
    stat_smooth(mapping = aes(y = upper_eWeight), col = embarktools::embark_colors[4], linetype = 'dashed') +
    stat_smooth(mapping = aes(y = lower_eWeight), col = embarktools::embark_colors[4], linetype = 'dashed') +
    stat_smooth(mapping = aes(y = eWeight), col = embarktools::embark_colors[3]) +
    geom_point(data = data %>% filter(agemos %% 6 == 0),
               aes(x = age, y = eWeight),
               size = 3,
               col = embarktools::embark_colors[1],
               fill = embarktools::embark_colors[3],
               shape = 21) +
    geom_point(data = pre_ed,
               aes(x = age, y = weight),
               size = 3,
               col = embarktools::embark_colors[1],
               fill = embarktools::embark_colors[3],
               shape = 21) +
    geom_point(data = post_ed,
               aes(x = age, y = weight),
               size = 3,
               col = embarktools::embark_colors[1],
               fill = embarktools::embark_colors[2],
               shape = 21) +
    scale_x_continuous(breaks = seq(age_start, max(data$age), by = 1),
                       labels = seq(age_start, max(data$age), by = 1)) +
    geom_ribbon(mapping = aes(ymin = lower_eWeight, ymax = upper_eWeight), fill = embarktools::embark_colors[4], alpha = 0.3) +
    ggtitle("Weight Prediction") +
    xlab('Age (years)') +
    ylab('Weight (lbs)') +
    embarktools::embark_theme_a +
    theme(panel.grid.major = element_line(colour = "grey", linetype = "dotted"),
          legend.position = "none") +
    # make some padding between y-axis and y-axis label
    theme(axis.title.y = element_text(margin = margin(r = 10)),
          axis.title.x = element_text(margin = margin(t = 10)))
    # Remove the default legend+


  # Create the legend plot
  legend_data <- data.frame(
    label = factor(c("Prediction Window", "Pre-ED Wt / Expected Wt", "Wt post ED onset"),
                   levels = c("Prediction Window", "Pre-ED Wt / Expected Wt", "Wt post ED onset"))
  )

  # Define the colors for the legend
  colors <- c(
    "Prediction Window" = "#A481C7",  # Lavender
    "Pre-ED Wt / Expected Wt" = "#C2B824",  # Chartreuse
    "Wt post ED onset" = "#EF6C45"  # Coral
  )

  # Create legend components
  prediction_window_legend <- ggplot(data.frame(x = c(1, 2), y = c(1, 1)), aes(x = x, y = y)) +
    geom_ribbon(aes(ymin = 0.5, ymax = 1.5), fill = colors["Prediction Window"], alpha = 0.3) +
    theme_void() +
    ggtitle("Prediction Window") +
    theme(plot.title = element_text(hjust = 0.5, size = 12,
                                    color = embarktools::embark_colors[1],
                                    family = 'Avenir'))

  pre_ed_legend <- ggplot(data.frame(x = 1, y = 1), aes(x = x, y = y)) +
    geom_point(size = 5, shape = 21, color = colors["Pre-ED Wt / Expected Wt"], fill = colors["Pre-ED Wt / Expected Wt"]) +
    theme_void() +
    ggtitle("Pre-ED Wt / Expected Wt") +
    theme(plot.title = element_text(hjust = 0.5, size = 12,
                                    color = embarktools::embark_colors[1],
                                    family = 'Avenir'))

  post_ed_legend <- ggplot(data.frame(x = 1, y = 1), aes(x = x, y = y)) +
    geom_point(size = 5, shape = 21, color = colors["Wt post ED onset"], fill = colors["Wt post ED onset"]) +
    theme_void() +
    ggtitle("Wt post ED onset") +
    theme(plot.title = element_text(hjust = 0.5, size = 12,
                                    color = embarktools::embark_colors[1],
                                    family = 'Avenir'))

  # Combine the custom legend components
  custom_legend <- ggpubr::ggarrange(
    prediction_window_legend, pre_ed_legend, post_ed_legend,
    ncol = 3, nrow = 1,
    common.legend = TRUE, legend = "none"
  )

  # Combine the main plot and the custom legend
  final_plot <- ggpubr::ggarrange(p, custom_legend, ncol = 1, heights = c(10, 1))
  return(final_plot)
}



