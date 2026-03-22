# Fit User Defined Forecast for BMIz Data

Fit User Defined Forecast for BMIz Data

## Usage

``` r
user_defined_forecast(
  data,
  lower_margin = NULL,
  upper_margin = NULL,
  central_value = c("mean", "max", "most_recent", "mean+most_recent")
)
```

## Arguments

- data:

  A tsibble containing the BMIz data.

- lower_margin:

  Lower margin for the forecast.

- upper_margin:

  Upper margin for the forecast.

- central_value:

  Central value for the forecast. Options are "mean", "max",
  "most_recent", "mean+most_recent".

## Value

A forecast object for each id.
