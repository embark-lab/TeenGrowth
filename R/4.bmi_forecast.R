#' Cull Demographics Data
#'
#' Align participant, sex coding, and add adult height and sex columns, if available.
#'
#' @param data A cleaned data frame containing demographic information.
#' @return A data frame with aligned demographics data.
#' @export
#'
cull_demos <- function(data) {
  df1_demos <- data |>
    select(id, sex, adult_height_in, agemos_adult_ht) |>
    unique()
  # make sex a double
  return(df1_demos)
}

#' Generate BMI Cutoffs by Participant
#'
#' Generate BMI cutoff data for each participant in the data frame.
#'
#' @param data A data frame containing participant information.
#' @return A data frame with BMI cutoff data for each participant.
#' @export
cutoffs_by_participant <- function(data) {
  results_list <- list()

  for (i in 1:nrow(data)) {
    id <- data$id[i]
    sex <- data$sex[i]
    adult_height_in <- data$adult_height_in[i]

    cutoff_data <- cutoff_bmi_by_age_data(sex = sex,
                                          adult_height = adult_height_in)
    cutoff_data <- cutoff_data %>%
      mutate(id = id)

    results_list[[i]] <- cutoff_data
  }

  result <- bind_rows(results_list)
  return(result)
}




#' Apply BMI Lookup
#'
#' Apply the BMI lookup function to a data frame.
#'
#' @param data A data frame containing BMI data.
#' @param data_point_col Column name for data points.
#' @param age_col Column name for age.
#' @param data_source Data source for BMI lookup (default: 'cdc').
#' @param type Type of BMI data (default: 'bmiz').
#' @return A vector of BMI values.
#' @export
apply_bmi_lookup <- function(data,
                             data_point_col,
                             age_col = agemos,
                             data_source =
                             'cdc',
                             type = 'bmiz') {
  data_point_col <- enquo(data_point_col)
  age_col <- enquo(age_col)

  data %>%
    rowwise() %>%
    mutate(
      bmi_value = bmi_lookup(
        data_point = !!data_point_col,
        type = type,
        data_source = data_source,
        sex = sex,
        age = !!age_col,
        age_unit = "months"
      )
    ) %>%
    pull(bmi_value)
}


# lookup eBMI values

#' Add eBMI to Data Frame
#'
#' Add estimated BMI (eBMI) and corresponding weights to the data frame.
#'
#' @param data A data frame containing BMI data.
#' @param adult_height Column name for adult height.
#' @return A data frame with added eBMI and weight columns.
#' @export
add_eBMI_to_df <- function(data,
                           adult_height = adult_height_in) {
  data <- data |>
    mutate(eBMI = apply_bmi_lookup(data, data_point_col = eBMIz),
           lower_eBMI = apply_bmi_lookup(data, data_point_col = lower_eBMIz),
           upper_eBMI = apply_bmi_lookup(data, data_point_col = upper_eBMIz)) |>
    mutate(eWeight = solve_for_weight(bmi = eBMI,
                                      height = adult_height_in),
           lower_eWeight = solve_for_weight(bmi = lower_eBMI,
                                            height = adult_height_in),
           upper_eWeight = solve_for_weight(bmi = upper_eBMI,
                                            height = adult_height_in)) |>
    # make eWeight, lower_eWeight, and upper_eWeight NA for agemos < 14*12
    mutate(eWeight = ifelse(agemos < agemos_adult_ht, NA, eWeight),
           lower_eWeight = ifelse(agemos < agemos_adult_ht, NA, lower_eWeight),
           upper_eWeight = ifelse(agemos < agemos_adult_ht, NA, upper_eWeight))

  return(data)
}



#' Make Full BMI Data Frame
#'
#' Create a full BMI data frame by processing forecasts, generating cutoffs, and adding eBMI values.
#'
#' @param data A cleaned data frame containing BMI data.
#' @param ci The confidence interval level.
#' @param id Column name for IDs (default: 'id').
#' @param adult_height Column name for adult height (default: 'adult_height').
#' @param lower_margin The lower margin for the user-defined forecast.
#' @param upper_margin The upper margin for the user-defined forecast.
#' @param central_value The central value for the user-defined forecast.
#' @import tidyr
#' @import dplyr
#' @return A full BMI data frame with processed forecasts and cutoffs.
#' @export
forecast_bmi <- function(data,
                             ci,
                             adult_height = 'adult_height_in',
                             id = 'id',
                             lower_margin = 0.5,
                             upper_margin = 0.5,
                             central_value = 'mean') {
  # filter out data with ed_aoo < agemos if ed_aoo is not all NA
  if (!all(is.na(data$agemos_ed_onset))) {
    data <- data |>
      filter(agemos <= agemos_ed_onset)
  }
  df_long <- make_bmiz_forecast(data = data,
                                ci = ci,
                                lower_margin = lower_margin,
                                upper_margin = upper_margin,
                                central_value = central_value)
  demos_data <- cull_demos(data)
  full_df_long <- full_join(demos_data, df_long, by = 'id')
  cutoff_data <- cutoffs_by_participant(cull_demos(data))
  full_df <- full_join(cutoff_data, full_df_long, by = c('id',
                                                         'agemos',
                                                         'sex',
                                                         'adult_height_in'))
  full_df <- add_eBMI_to_df(data = full_df,
                            adult_height = adult_height)

  return(full_df)
}


#' Clean and Process Data
#'
#' @param data A data frame containing BMI data.
#' @param central_value The central_value type used for forecasting.
#' @param ci The confidence interval level.
#' @param id_col_name Column name for IDs (default: NULL).
#' @param age_col_name Column name for age (default: NULL).
#' @param dob_col_name Column name for date of birth (default: NULL).
#' @param date_assessed_col_name Column name for date assessed (default: NULL).
#' @param age_unit Unit for age (default: NULL).
#' @param sex_col_name Column name for sex (default: NULL).
#' @param ht_col_name Column name for height (default: NULL).
#' @param ht_unit Unit for height (default: 'cm').
#' @param wt_col_name Column name for weight (default: NULL).
#' @param wt_unit Unit for weight (default: 'kg').
#' @param bmi_col_name Column name for BMI (default: NULL).
#' @param bmiz_col_name Column name for BMI z-score (default: NULL).
#' @param pct_col_name Column name for BMI percentile (default: NULL).
#' @param data_source Data source for BMI lookup (default: 'cdc').
#' @param lower_margin The lower margin for the user-defined forecast.
#' @param upper_margin The upper margin for the user-defined forecast.
#' @param central_value The central value for the user-defined forecast.
#' @param adult_ht_col_name Column name for adult height (default: NULL).
#' @param age_adult_ht_col_name Column name for age at adult height (default: NULL).
#' @param ed_aoo_col_name Column name for age at onset of eating disorder (default: NULL).
#' @return A full BMI data frame with processed forecasts and cutoffs.
#' @export
#' @import dplyr
#' @import tidyr
clean_and_process <- function(data,
                              central_value = 'mean',
                              ci = 95,
                              id_col_name = NULL,
                              age_col_name = NULL,
                              dob_col_name = NULL,
                              date_assessed_col_name = NULL,
                              age_unit = NULL,
                              sex_col_name = NULL,
                              ht_col_name = NULL,
                              ht_unit = 'cm',
                              wt_col_name = NULL,
                              wt_unit = 'kg',
                              bmi_col_name = NULL,
                              bmiz_col_name = NULL,
                              pct_col_name = NULL,
                              data_source = 'cdc',
                              adult_ht_col_name = NULL,
                              age_adult_ht_col_name = NULL,
                              lower_margin = 0.5,
                              upper_margin = 0.5,
                              ed_aoo_col_name = NULL) {
  clean_data <- clean_data(data,
                           id_col_name = id_col_name,
                           age_col_name = age_col_name,
                           dob_col_name = dob_col_name,
                           date_assessed_col_name = date_assessed_col_name,
                           age_unit = age_unit,
                           sex_col_name = sex_col_name,
                           ht_col_name = ht_col_name,
                           ht_unit = ht_unit,
                           wt_col_name = wt_col_name,
                           wt_unit = wt_unit,
                           bmi_col_name = bmi_col_name,
                           bmiz_col_name = bmiz_col_name,
                           pct_col_name = pct_col_name,
                           data_source = data_source,
                           adult_ht_col_name = adult_ht_col_name,
                           age_adult_ht_col_name = age_adult_ht_col_name,
                           ed_aoo_col_name = ed_aoo_col_name)
  full_df <- forecast_bmi(clean_data,
                              central_value = central_value,
                              ci = ci,
                              lower_margin = lower_margin,
                              upper_margin = upper_margin)
  return(full_df)
}
