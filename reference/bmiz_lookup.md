# Lookup BMI Z-Score

Lookup BMI Z-Score

## Usage

``` r
bmiz_lookup(
  bmi,
  sex = 2,
  age,
  data_source = "cdc",
  age_unit = NULL,
  dob = NULL,
  date_assessed = NULL
)
```

## Arguments

- bmi:

  BMI value.

- sex:

  Sex of the individual (1 for male, 2 for female).

- age:

  Age of the individual.

- data_source:

  Data source ('cdc' or 'wgsr').

- age_unit:

  Unit of age ('months', 'years', etc.).

- dob:

  Date of birth (optional).

- date_assessed:

  Date of assessment (optional).

## Value

Z-score corresponding to the given BMI.
