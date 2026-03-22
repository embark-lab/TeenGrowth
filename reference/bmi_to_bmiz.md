# Convert BMI to Z-Score

Convert BMI to Z-Score

## Usage

``` r
bmi_to_bmiz(l, m, s, bmi, sigma = NULL, p95 = NULL)
```

## Arguments

- l:

  L parameter from the LMS table.

- m:

  M parameter from the LMS table.

- s:

  S parameter from the LMS table.

- bmi:

  BMI value.

- sigma:

  Standard deviation (optional).

- p95:

  95th percentile BMI value (optional).

## Value

Z-score corresponding to the given BMI.
