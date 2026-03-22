# Calculate BMI-Z scores for a dataset.

This function takes a dataset and calculates BMI-Z scores.

## Usage

``` r
bmiz_zscorer(
  data,
  wt,
  ht,
  age,
  wt_units = "kg",
  ht_units = "cm",
  sex = 2,
  dob = NULL,
  date_assessed = NULL
)
```

## Arguments

- data:

  A data frame containing weight, height, age, and sex information.

- wt:

  A vector representing weight.

- ht:

  A vector representing height.

- age:

  A vector representing age.

- wt_units:

  A vector of weight units ('kg' or 'lb').

- ht_units:

  A vector of height units ('cm', 'm', 'in', 'ft').

- sex:

  A vector representing sex (default set to 2 = female)

- dob:

  Date of birth (optional).

- date_assessed:

  Date of assessment (optional).

## Value

A data frame with BMI-Z scores added as 'bmiz' column.
