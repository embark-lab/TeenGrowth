
demo  <- clean_data(test_data_3,
                     adult_height_col_name = 'adult_ht_in',
                     ht_col_name = 'height',
                     ht_unit = 'in',
                     bmiz_col_name = 'bmiz',
                     age_col_name = 'age',
                     sex_col_name = 'sex',
                     id_col_name = 'participant')

calculate_weight <- function(bmi, height_in) {
  weight_lbs <- bmi * (height_in^2) / 703
  return(weight_lbs)
}

demo <-  demo |>
  mutate(wt = calculate_weight(bmi, height_in)) |>
  # recode sex back to F and M from 1 and 2
  mutate(sex = recode(sex, '1' = 'M', '2' = 'F')) |>
  select(id, age_years, height_in, wt,
                sex, adult_height_in) |>
  rename(participant = id,
                         age = age_years,
                         height = height_in,
                         weight = wt) |>
  mutate(age = round(age, 1),
        weight = round(weight, 1))

# save the demo data
save(demo,file='data/demo_data.RData')


