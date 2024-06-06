library(dplyr)

clean_data_1 <- clean_data(demo,
                         id_col_name = "participant",
                         age_col_name = 'age',
                         ht_col_name = 'height',
                         wt_col_name = 'weight',
                         sex_col_name = 'sex',
                         wt_unit = 'lb',
                         ht_unit = 'in',
                         adult_ht_col_name= 'adult_height_in',
                         ed_aoo_col_name = 'ed_aoo',
                         age_unit = 'years')


px <- 2

forecast_input <- clean_data_1 |>
                  filter(id == px) |>
                  filter(agemos < agemos_ed_onset[1])

forecast_data <- make_full_bmi_df(forecast_input,
                                  central_value = 'most_recent',
                                  ci = 95,
                                  lower_margin = 0.5,
                                  upper_margin = 0.5)



plot_eBMI(clean_data_1,
        forecast_data,
        px = px,
        agemos_onset_ed = forecast_input$agemos_ed_onset[1])

plot_weight(clean_data_1,
            forecast_data,
            px = px,
            agemos_onset_ed = forecast_input$agemos_ed_onset[1])

# Example usage
clean_data <- tx_plot_clean(demo %>% filter(participant == 2),
                            dob = '2008-11-01',
                            age_col_name = 'age',
                            age_unit = 'years',
                            tx_start_date = '2024-01-01',
                            ht_col_name = 'height',
                            ht_unit = 'in',
                            sex = 2,
                            wt_col_name = 'weight',
                            wt_unit = 'lb')

# Ensure forecast_data is provided for testing

Wt_Restore_Plot(clean_data, forecast_data, slope_per_week = 1)



clean_data_1 <- clean_data(test_data_3,
                           id_col_name = "participant",
                           age_col_name = 'age',
                           sex_col_name = 'sex',
                           bmiz_col_name = 'bmiz',
                           ht_unit = 'in',
                           adult_ht_col_name= 'adult_ht_in',
                           age_unit = 'years')

px <- 1

forecast_input <- clean_data_1 |>
  filter(id == px) |>
  filter(agemos < 15*12)

clean_data <- tx_plot_clean(test_data_3 %>% filter(participant == px),
                            dob = '2009-11-01',
                            age_col_name = 'age',
                            age_unit = 'years',
                            tx_start_date = '2024-01-01',
                            adult_ht = 66,
                            ht_unit = 'in',
                            age_adult_ht = 14,
                            sex = 2,
                            bmiz_col_name = 'bmiz')
