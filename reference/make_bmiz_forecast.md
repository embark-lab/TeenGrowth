# Create and Forecast BMIz Data

Create and Forecast BMIz Data

## Usage

``` r
make_bmiz_forecast(
  data,
  ci = 95,
  lower_margin = NULL,
  upper_margin = NULL,
  central_value = c("mean", "max", "most_recent", "mean+most_recent")
)
```

## Arguments

- data:

  A data frame containing the input data.

- ci:

  Confidence interval for the forecast.

- lower_margin:

  Lower margin for the forecast.

- upper_margin:

  Upper margin for the forecast.

- central_value:

  Central value for the forecast. Options are "mean", "max",
  "most_recent", "mean+most_recent".

## Value

A forecast object for each id.
