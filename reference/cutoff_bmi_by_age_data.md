# Cutoff BMI

Makes Cutoff BMIs

## Usage

``` r
cutoff_bmi_by_age_data(
  age = agemos,
  data_source = "cdc",
  sex = 2,
  adult_height = NA
)
```

## Arguments

- age:

  'age in months'

- data_source:

  'cdc' or 'who'

- sex:

  '1 = Male, 2 = Female'

- adult_height:

  'adult height in inches'

## Value

bmi scores based on bmiz and age
