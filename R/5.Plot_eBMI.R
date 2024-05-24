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
                      model,
                      ci,
                      px) {
  data_1 <- clean_data %>%
    filter(id == px)
  data_2 <- forecast_data %>%
    filter(id == px & .model == model & interval_type == ci)
  data_2$age <- data_2$agemos/12
  ggplot2::ggplot(data = data_2, mapping = aes(x = age, y = eBMI)) +
  geom_point(mapping = aes(agemos, bmi), data = data_1, stat = 'identity', position = 'identity') +
    stat_smooth(mapping = aes(x = agemos, y = bmi), method = lm, formula = y~x + poly(x,2) + poly(x,3), linetype = 'dotted', col = 'blue', data = data_1, se = FALSE) +
    stat_smooth(mapping = aes(x = agemos, y = upper_eBMI), col = 'purple', linetype = 'dashed', data = data_2,  position = 'identity' ) + stat_smooth(mapping = aes(x = agemos, y = lower_eBMI), col = 'purple', linetype = 'dashed', data = data_2,  position = 'identity' ) +
     stat_smooth(mapping = aes (x = agemos, y =eBMI), data = data_2) +
    scale_x_continuous(breaks = 12*0:240, label = m2y(12*0:240)) +
stat_smooth(mapping = aes(x = agemos, y = median_bmi), data = data_2, col = 'orange', linetype = 'dotted', position = 'identity')+
 stat_smooth(mapping = aes(x = agemos, y = UW_cutoff_bmi), data = data_2, col = 'red', linetype = 'dotted', position = 'identity' )+
 xlab('Age') +
ylab('BMI') +
    coord_cartesian(ylim = c(12, 30)) +
    embarktools::embark_theme_a
}

