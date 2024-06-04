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


px <- 1

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
clean_data <- tx_plot_clean(demo |> filter(participant == 1),
                            dob = '2007-12-30',
                            age_col_name = 'age',
                            tx_start_date = '2024-01-01',
                            ht_col_name = 'height',
                            ht_unit  = 'in',
                            sex = 2,
                            wt_col_name = 'weight',
                            wt_unit = 'lbs')

# Ensure forecast_data is provided for testing

plot_weight_2(clean_data, forecast_data, slope_per_week = 1)


# Example usage
clean_data <- tx_plot_clean(demo %>% filter(participant == 1),
                            dob = '2008-01-01',
                            age_col_name = 'age',
                            age_unit = 'years',
                            tx_start_date = '2024-01-01',
                            ht_col_name = 'height',
                            ht_unit = 'in',
                            sex = 2,
                            wt_col_name = 'weight',
                            wt_unit = 'lbs')

