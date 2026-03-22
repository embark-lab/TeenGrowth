# Convert age to age in months (rounded).

Convert age to age in months (rounded).

## Usage

``` r
age_in_months(age, age_unit = NULL, dob = NULL, date_assessed = NULL)
```

## Arguments

- age:

  age

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

Numeric value representing the age in months (rounded to the nearest
month).

## Examples

``` r
age_in_months(365)  # Returns 12
#> [1] 365
age_in_months(730)  # Returns 24
#> [1] 730
```
