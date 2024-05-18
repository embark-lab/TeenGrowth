#' Cull Demographics Data
#'
#' Align participant, sex coding, and add adult height and sex columns, if available.
#'
#' @param data A data frame containing demographic information.
#' @param participant (Optional) Column name for participant IDs.
#' @param sex (Optional) Column name for sex.
#' @param adult_height (Optional) Column name for adult height.
#' @return A data frame with aligned demographics data.
#' @export
#'
cull_demos <- function(data, participant = NULL, sex = NULL, adult_height = NULL) {
  if (!missing(participant) && participant %in% names(data)) {
    data <- data |> mutate(participant = .data[[participant]])
  } else if ('participant' %in% names(data)) {
    data <- data
  } else {
    data <- data |> mutate(participant = 1)
  }

  if (!missing(sex) && sex %in% names(data)) {
    data <- data |> mutate(sex = .data[[sex]])
  } else if ('sex' %in% names(data)) {
    data <- data
  } else {
    data <- data |> mutate(sex = 2)
  }

  if (!missing(adult_height) && adult_height %in% names(data)) {
    data <- data |> mutate(adult_height = .data[[adult_height]])
  } else {
    data <- data |> mutate(adult_height = NA)
  }

  df1_demos <- align_sex_coding_in_df(data = data, sex_column = 'sex') |>
    select(participant, sex, adult_height) |>
    unique()

  return(df1_demos)
}

cutoffs_by_participant <- function(data) {
  results_list <- list()
  # Loop through each participant and apply the cutoff_bmi_by_age_data function
  for (i in 1:nrow(data)) {
    participant <- data$participant[i]
    sex <- data$sex[i]
    adult_height <- data$adult_height[i]

    # Generate the cutoff data for the current participant
    cutoff_data <- cutoff_bmi_by_age_data(sex = sex, adult_height = adult_height)

    # Add the participant identifier
    cutoff_data <- cutoff_data %>%
      mutate(participant = participant)

    # Store the result in the list
    results_list[[i]] <- cutoff_data
  }

  # Combine the results into a single tibble
  result <- bind_rows(results_list)
  return(result)
}


# make a function that will take a bmiz forecast dataframe and return formatted intervals
# Function to convert hilo objects to character strings
hilo_to_char <- function(hilo_obj) {
  lower <- hilo_obj$lower
  upper <- hilo_obj$upper
  paste0("[", lower, ", ", upper, "]")
}

# Main function to process the bmiz forecast dataframe
process_bmiz_forecast <- function(df) {
  # Convert `95%` and `99%` columns to character strings
  df <- df %>%
    mutate(
      `95%` = sapply(`95%`, hilo_to_char),
      `99%` = sapply(`99%`, hilo_to_char)
    )

  # Pivot longer and remove rows where range is NA
  df_long <- df %>%
    pivot_longer(
      cols = c(`95%`, `99%`, user_defined_hilo),
      names_to = "interval_type",
      values_to = "range"
    ) %>%
    # filter out when range is [NA, NA]
    filter (range != "[NA, NA]")
# extract the lower and upper bounds of the interval to columns ('lower', and 'upper') and make them numeric - get rid of the opening and closing brackets as well
  df_long <- df_long %>%
    separate(
      col = range,
      into = c("lower", "upper"),
      sep = ", ",
      remove = TRUE
    ) %>%
    mutate(
      lower_eBMIz = as.numeric(sub("\\[", "", lower)),
      upper_eBMIz = as.numeric(sub("\\]", "", upper))
    ) |>
    # rename .mean to eBMIz
    rename(eBMIz = .mean)  |>
    # remove the lower and upper columns
    select(-c(lower, upper))
  return(df_long)
}


apply_bmi_lookup <- function(data, data_point_col, age_col = agemos, data_source = 'cdc', type = 'bmiz') {
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
        age = !!age_col
      )
    ) %>%
    pull(bmi_value)
}

# lookup eBMI values

add_eBMI_to_df <- function(data, adult_height = adult_height) {
data <- data |>
  mutate(eBMI = apply_bmi_lookup(data, data_point_col = eBMIz))
       lower_eBMI = apply_bmi_lookup(data, data_point_col = lower_eBMIz),
       upper_eBMI = apply_bmi_lookup(data, data_point_col = upper_eBMIz))
  mutate(eWeight = solve_for_weight(bmi = eBMI, height = adult_height),
         lower_eWeight = solve_for_weight(bmi = lower_eBMI, height = adult_height),
         upper_eWeight = solve_for_weight(bmi = upper_eBMI, height = adult_height))
}


make_full_bmi_df <- function(data, participant = 'participant', adult_height = 'adult_height') {
  df_long <- process_bmiz_forecast(make_bmiz_forecast(data, participant = participant))
  cutoff_data <- cutoffs_by_participant(cull_demos(data, adult_height = adult_height))
  full_df <- merge(cutoff_data, df_long, by = c('participant', 'agemos'))
  full_df <- add_eBMI_to_df(full_df, adult_height = adult_height)
}


