#' @title Plot BMI
#' @description Plot BMI
#' @param data 'data'
#' @param age_days 'age in days column'
#' @param sex '1 = Male, 2 = Female'
#' @param age 'age in months'
#' @param adult_height 'adult height in cm'
#' @import dplyr
#' @return bmi scores based on bmiz and age
#' @export


Plot_eBMI <- function(data, age, bmi, sex) {
  ggplot(data = data, mapping = aes(x = age, y = bmi)) +
    geom_point(mapping = aes(agemos, bmi_real), data = bmi_data_bypx[[i]], stat = 'identity', position = 'identity') +
    stat_smooth(mapping = aes(x = agemos, y = bmi_real), method = lm, formula = y~x + poly(x,2) + poly(x,3), linetype = 'dotted', col = 'blue', data = bmi_data_bypx[[i]], se = FALSE) +
    stat_smooth(mapping = aes(x = agemos, y = bmi_upper), col = 'purple', linetype = 'dashed', data = fcast_bypx[[i]],  position = 'identity' ) +
    stat_smooth(mapping = aes(x = agemos, y = bmi_lower), col = 'purple', linetype = 'dashed', data = fcast_bypx[[i]],  position = 'identity' ) +
    stat_smooth(mapping = aes (x = agemos, y = bmi), data = fcast_bypx[[i]]) +
    scale_x_continuous(breaks = 12*0:240, label = m2y(12*0:240)) +
    stat_smooth(mapping = aes(x = agemos, y = bmi), col = 'orange', linetype = 'dotted', data = median_bmi_by_age, stat = 'identity', position = 'identity')+
    stat_smooth(mapping = aes(x = agemos, y = UW_cutoff_bmi), col = 'red', linetype = 'dotted', data = median_bmi_by_age, stat = 'identity', position = 'identity' )+
    ylim(c(14, 27))+
    xlab('Age') +
    ylab('BMI')
}
