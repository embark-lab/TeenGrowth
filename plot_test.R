library(dplyr)

clean_data <- clean_data(demo,
                         id_col_name = "participant",
                         age_col_name = 'age',
                         ht_col_name = 'height',
                         wt_col_name = 'weight',
                         sex_col_name = 'sex',
                         wt_unit = 'lb',
                         ht_unit = 'in',
                         ed_aoo_col_name = 'ed_aoo'
)


px <- 3

forecast_input <- clean_data |>
                  filter(id == px) |>
                  filter(agemos < agemos_ed_onset[1])

forecast_data <- make_full_bmi_df(forecast_input,
                                  central_value = 'mean',
                                  ci = 'User-Defined',
                                  lower_margin = 0.5,
                                  upper_margin = 0.5)



plot_eBMI(clean_data,
        forecast_data,
        px = px,
        agemos_onset_ed = forecast_input$agemos_ed_onset[1])


