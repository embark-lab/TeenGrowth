# Add BMI to Data Frame

This function adds a BMI column to a data frame based on age, sex, and
normalized BMI (BMI percentile or BMI z-score).

## Usage

``` r
add_bmi(
  data,
  age,
  normed_bmi,
  data_source = "cdc",
  type = "bmiz",
  sex = 2,
  dob = NULL,
  date_assessed = NULL,
  age_unit = NULL
)
```

## Arguments

- data:

  A data frame containing the necessary columns.

- age:

  A column in `data` representing the age of individuals.

- normed_bmi:

  A column in `data` representing the normalized BMI (either BMI
  percentile or BMI z-score).

- data_source:

  A string specifying the data source, either 'cdc' or 'wgsr'. Default
  is 'cdc'.

- type:

  A string specifying the type of normalized BMI, either 'bmiz' for BMI
  z-score or 'pct' for BMI percentile. Default is 'bmiz'.

- sex:

  A column in `data` representing the sex of individuals, where 1 = Male
  and 2 = Female. Default is 2 (Female).

- dob:

  (Optional) A column in `data` representing the date of birth.

- date_assessed:

  (Optional) A column in `data` representing the date of assessment.

- age_unit:

  (Optional) A string specifying the unit of the age column.

## Value

A data frame with an added BMI column.
