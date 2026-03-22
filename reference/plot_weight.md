# Plot Weight Forecast

This function plots the weight forecast data for a given participant,
model, and confidence interval.

## Usage

``` r
plot_weight(
  clean_data,
  forecast_data,
  px,
  agemos_ed_onset = NA,
  a_height = NULL
)
```

## Arguments

- clean_data:

  A data frame containing the clean data.

- forecast_data:

  A data frame containing the forecast data.

- px:

  The participant ID.

- agemos_ed_onset:

  The age of onset of the eating disorder.

- a_height:

  The adult height of the participant.

## Value

A ggplot object displaying the weight forecast.

## Examples

``` r
# Assuming forecast_data is your dataframe and embarktools is loaded
# plot_weight(forecast_data, "ARIMA", "95%", 1, a_height = 65)
```
