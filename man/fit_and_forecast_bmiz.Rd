% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/3.bmiz_forecast.R
\name{fit_and_forecast_bmiz}
\alias{fit_and_forecast_bmiz}
\title{Fit Models and Generate Forecasts for BMIz Data}
\usage{
fit_and_forecast_bmiz(
  ts_data,
  central_value = c("mean", "max", "most_recent", "mean+most_recent"),
  ci = 95,
  lower_margin = NULL,
  upper_margin = NULL
)
}
\arguments{
\item{ts_data}{A tsibble containing the BMIz data.}

\item{central_value}{Central value for the forecast. Options are "mean", "max", "most_recent", "mean+most_recent".}

\item{ci}{Confidence interval for the forecast.}

\item{lower_margin}{Lower margin for the forecast.}

\item{upper_margin}{Upper margin for the forecast.}
}
\value{
A forecast object for each id.
}
\description{
Fit Models and Generate Forecasts for BMIz Data
}
