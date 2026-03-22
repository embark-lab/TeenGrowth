# Determine the age unit (days, weeks, months, or years) based on the input age.

This function assumes that the input age is between 5 and 21 years.

## Usage

``` r
age_unit(age)
```

## Arguments

- age:

  Numeric value representing the age.

## Value

A character string indicating the age unit (days, weeks, months, or
years).

## Examples

``` r
age_unit(365)  # Returns "days"
#> [1] "months"
age_unit(10*365)  # Returns "years"
#> [1] "days"
```
