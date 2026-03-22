# Clean Forecast Data

This function cleans and processes forecast data by filtering based on
patient ID, model type, and confidence interval. It formats the data to
include only ages divisible by 6 and rounds all numerical values to one
decimal place. It also formats the expected BMI and expected weight.

## Usage

``` r
clean_forecast_data(data, px, model)
```

## Arguments

- data:

  A data frame containing forecast data.

- px:

  The patient ID to filter the data.

- model:

  The model type to filter the data.

## Value

A cleaned and processed data frame with the age in years, expected BMI,
and expected weight, formatted appropriately.
