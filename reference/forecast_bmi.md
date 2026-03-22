# Make Full BMI Data Frame

Create a full BMI data frame by processing forecasts, generating
cutoffs, and adding eBMI values.

## Usage

``` r
forecast_bmi(
  data,
  ci,
  adult_height = "adult_height_in",
  id = "id",
  lower_margin = 0.5,
  upper_margin = 0.5,
  central_value = "mean"
)
```

## Arguments

- data:

  A cleaned data frame containing BMI data.

- ci:

  The confidence interval level.

- adult_height:

  Column name for adult height (default: 'adult_height').

- id:

  Column name for IDs (default: 'id').

- lower_margin:

  The lower margin for the user-defined forecast.

- upper_margin:

  The upper margin for the user-defined forecast.

- central_value:

  The central value for the user-defined forecast.

## Value

A full BMI data frame with processed forecasts and cutoffs.
