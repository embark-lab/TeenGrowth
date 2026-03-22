# Clean Treatment Data for Percentile plotting

Clean Treatment Data for Percentile plotting

## Usage

``` r
pct_plot_clean(
  filtered_data,
  age_col_name = NULL,
  date_assessed_col_name = NULL,
  ht_col_name = NULL,
  wt_col_name = NULL,
  age_unit = NULL,
  ht_unit = "cm",
  wt_unit = "kg",
  bmi_col_name = NULL,
  bmiz_col_name = NULL,
  pct_col_name = NULL,
  data_source = "cdc",
  dob = NULL,
  sex = NULL,
  current_ht = NULL,
  age_current_ht = NULL,
  adult_ht = NULL,
  age_adult_ht = NULL,
  ed_aoo = NULL,
  tx_start_date = NULL,
  intake_wt = NULL
)
```

## Arguments

- filtered_data:

  A data frame containing the filtered data for a single participant.

- age_col_name:

  The name of the column containing the age of the participant

- date_assessed_col_name:

  The name of the column containing the date the data was assessed

- ht_col_name:

  The name of the column containing the height of the participant

- wt_col_name:

  The name of the column containing the weight of the participant

- age_unit:

  The unit of the age column (e.g., years, months, days)

- ht_unit:

  The unit of the height column (e.g., cm, in)

- wt_unit:

  The unit of the weight column (e.g., kg, lb)

- bmi_col_name:

  The name of the column containing the BMI of the participant

- bmiz_col_name:

  The name of the column containing the BMI z-score of the participant

- pct_col_name:

  The name of the column containing the percentile of the participant

- data_source:

  The source of the data (e.g., cdc, who)

- dob:

  The date of birth of the participant

- sex:

  Sex

- current_ht:

  The current height of the participant

- age_current_ht:

  The age at which the current height was measured

- adult_ht:

  The adult height of the participant

- age_adult_ht:

  The age at which the adult height was measured

- ed_aoo:

  The age of onset of the eating disorder

- tx_start_date:

  The start date of the treatment

- intake_wt:

  The intake weight of the participant

## Value

A cleaned and processed data frame with the age in years, expected BMI,
and expected weight, formatted appropriately.
