# Convert age from various units to days.

This function takes an age input that can be in years, months, days, or
as a combination of birthdate and date of assessment.

## Usage

``` r
age_in_days(age = NULL, age_unit = NULL, dob = NULL, date_assessed = NULL)
```

## Arguments

- age:

  Numeric value representing the age.

- age_unit:

  (Optional) A character string specifying the unit of the input age
  ('years', 'months', 'days').

- dob:

  (Optional) The date of birth as a character string in the format
  'YYYY-MM-DD'.

- date_assessed:

  (Optional) The date of assessment as a character string in the format
  'YYYY-MM-DD'.

## Value

Numeric value representing the age in days.

## Examples

``` r
age_in_days(10, "years")  # Returns the age in days
#> [1] 3652.5
age_in_days(120, "months")  # Returns the age in days
#> [1] 3652.5
```
