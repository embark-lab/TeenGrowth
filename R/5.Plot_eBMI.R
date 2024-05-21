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


plot_eBMI <- function(raw_data, forcast_data, model, id) {
  raw_data <- raw_data %>%
    filter(participant == id)
  forcast_data <- forecast_data %>%
    filter(participant == id & .model == model)
  data$age <- data$agemos/12
  ggplot2::ggplot(data = data, mapping = aes(x = age, y = eBMI)) +
  #  geom_point(mapping = aes(agemos, bmi_real), data = bmi_data_bypx[[i]], stat = 'identity', position = 'identity') +
# #    stat_smooth(mapping = aes(x = agemos, y = bmi_real), method = lm, formula = y~x + poly(x,2) + poly(x,3), linetype = 'dotted', col = 'blue', data = bmi_data_bypx[[i]], se = FALSE) +
#     stat_smooth(mapping = aes(x = agemos, y = bmi_upper), col = 'purple', linetype = 'dashed', data = fcast_bypx[[i]],  position = 'identity' ) +
#     stat_smooth(mapping = aes(x = agemos, y = bmi_lower), col = 'purple', linetype = 'dashed', data = fcast_bypx[[i]],  position = 'identity' ) +
#     stat_smooth(mapping = aes (x = agemos, y = bmi), data = fcast_bypx[[i]]) +
    scale_x_continuous(breaks = 12*0:240, label = m2y(12*0:240)) +
    stat_smooth(mapping = aes(x = agemos, y = median_bmi), col = 'orange', linetype = 'dotted', stat = 'identity', position = 'identity')+
    stat_smooth(mapping = aes(x = agemos, y = UW_cutoff_bmi), col = 'red', linetype = 'dotted', stat = 'identity', position = 'identity' )+
    ylim(c(14, 27))+
    xlab('Age') +
    ylab('BMI') +
    embarktools::embark_theme_a
}
