# Fit Models and Generate Forecasts for BMIz Data

Fit Models and Generate Forecasts for BMIz Data

## Usage

``` r
fit_and_forecast_bmiz(
  ts_data,
  central_value = c("mean", "max", "most_recent", "mean+most_recent"),
  ci = 95,
  lower_margin = NULL,
  upper_margin = NULL
)
```

## Arguments

- ts_data:

  A tsibble containing the BMIz data.

- central_value:

  Central value for the forecast. Options are "mean", "max",
  "most_recent", "mean+most_recent".

- ci:

  Confidence interval for the forecast.

- lower_margin:

  Lower margin for the forecast.

- upper_margin:

  Upper margin for the forecast.

## Value

A forecast object for each id.
