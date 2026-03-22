# Clean and Process Data

Clean and Process Data

## Usage

``` r
clean_and_process(
  data,
  central_value = "mean",
  ci = 95,
  id_col_name = NULL,
  age_col_name = NULL,
  dob_col_name = NULL,
  date_assessed_col_name = NULL,
  age_unit = NULL,
  sex_col_name = NULL,
  ht_col_name = NULL,
  ht_unit = "cm",
  wt_col_name = NULL,
  wt_unit = "kg",
  bmi_col_name = NULL,
  bmiz_col_name = NULL,
  pct_col_name = NULL,
  data_source = "cdc",
  adult_ht_col_name = NULL,
  age_adult_ht_col_name = NULL,
  lower_margin = 0.5,
  upper_margin = 0.5,
  ed_aoo_col_name = NULL
)
```

## Arguments

- data:

  A data frame containing BMI data.

- central_value:

  The central value for the user-defined forecast.

- ci:

  The confidence interval level.

- id_col_name:

  Column name for IDs (default: NULL).

- age_col_name:

  Column name for age (default: NULL).

- dob_col_name:

  Column name for date of birth (default: NULL).

- date_assessed_col_name:

  Column name for date assessed (default: NULL).

- age_unit:

  Unit for age (default: NULL).

- sex_col_name:

  Column name for sex (default: NULL).

- ht_col_name:

  Column name for height (default: NULL).

- ht_unit:

  Unit for height (default: 'cm').

- wt_col_name:

  Column name for weight (default: NULL).

- wt_unit:

  Unit for weight (default: 'kg').

- bmi_col_name:

  Column name for BMI (default: NULL).

- bmiz_col_name:

  Column name for BMI z-score (default: NULL).

- pct_col_name:

  Column name for BMI percentile (default: NULL).

- data_source:

  Data source for BMI lookup (default: 'cdc').

- adult_ht_col_name:

  Column name for adult height (default: NULL).

- age_adult_ht_col_name:

  Column name for age at adult height (default: NULL).

- lower_margin:

  The lower margin for the user-defined forecast.

- upper_margin:

  The upper margin for the user-defined forecast.

- ed_aoo_col_name:

  Column name for age at onset of eating disorder (default: NULL).

## Value

A full BMI data frame with processed forecasts and cutoffs.
