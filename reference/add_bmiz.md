# Calculate BMI-Z scores for a dataset.

This function takes a dataset and calculates BMI-Z scores.

## Usage

``` r
add_bmiz(
  data,
  wt = NULL,
  ht = NULL,
  age = NULL,
  bmi = NULL,
  wt_units = "kg",
  ht_units = "cm",
  sex = 2,
  dob = NULL,
  date_assessed = NULL,
  data_source = "cdc"
)
```

## Arguments

- data:

  A data frame containing weight, height, age, and sex information.

- wt:

  A vector representing weight (optional).

- ht:

  A vector representing height (optional).

- age:

  A vector representing age.

- bmi:

  A vector representing BMI (optional).

- wt_units:

  A vector of weight units ('kg' or 'lb').

- ht_units:

  A vector of height units ('cm', 'm', 'in', 'ft').

- sex:

  A vector representing sex (default set to 2 = female).

- dob:

  Date of birth (optional).

- date_assessed:

  Date of assessment (optional).

- data_source:

  Data source ('cdc' or 'wgsr') - Default to 'cdc'.

## Value

A data frame with BMI-Z scores added as the 'bmiz' column.
