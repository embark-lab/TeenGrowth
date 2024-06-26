% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/3.bmiz_forecast.R
\name{user_defined_forecast}
\alias{user_defined_forecast}
\title{Fit User Defined Forecast for BMIz Data}
\usage{
user_defined_forecast(
  data,
  lower_margin = NULL,
  upper_margin = NULL,
  central_value = c("mean", "max", "most_recent", "mean+most_recent")
)
}
\arguments{
\item{data}{A tsibble containing the BMIz data.}

\item{lower_margin}{Lower margin for the forecast.}

\item{upper_margin}{Upper margin for the forecast.}

\item{central_value}{Central value for the forecast. Options are "mean", "max", "most_recent", "mean+most_recent".}
}
\value{
A forecast object for each id.
}
\description{
Fit User Defined Forecast for BMIz Data
}
