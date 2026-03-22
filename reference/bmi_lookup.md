# Lookup BMI

Lookup BMI

## Usage

``` r
bmi_lookup(
  data_point,
  type = "bmiz",
  data_source = "cdc",
  sex = 2,
  age,
  age_unit = NULL,
  dob = NULL,
  date_assessed = NULL
)
```

## Arguments

- data_point:

  Data point (BMI Z-score or percentile).

- type:

  Type of data point ('bmiz' or 'pct').

- data_source:

  Data source ('cdc' or 'wgsr').

- sex:

  Sex of the individual (1 for male, 2 for female).

- age:

  Age of the individual.

- age_unit:

  Unit of age ('months', 'years', etc.).

- dob:

  Date of birth (optional).

- date_assessed:

  Date of assessment (optional).

## Value

BMI value corresponding to the given data point.
