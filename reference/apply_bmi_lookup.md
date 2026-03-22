# Apply BMI Lookup

Apply the BMI lookup function to a data frame.

## Usage

``` r
apply_bmi_lookup(
  data,
  data_point_col,
  age_col = agemos,
  data_source = "cdc",
  type = "bmiz"
)
```

## Arguments

- data:

  A data frame containing BMI data.

- data_point_col:

  Column name for data points.

- age_col:

  Column name for age.

- data_source:

  Data source for BMI lookup (default: 'cdc').

- type:

  Type of BMI data (default: 'bmiz').

## Value

A vector of BMI values.
