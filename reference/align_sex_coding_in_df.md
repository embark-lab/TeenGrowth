# Align Sex Coding in Data Frame

This function aligns the sex coding in a data frame by detecting how the
sex variable is coded ('Male/Female', 'M/F', or '0(Male)/1(Female)') and
converting them to 1/2.

## Usage

``` r
align_sex_coding_in_df(data, sex_column = "sex")
```

## Arguments

- data:

  A data frame containing a column with sex values.

- sex_column:

  The name of the column containing the sex values. Defaults to 'sex'.

## Value

The data frame with the sex column values aligned to 1 for male and 2
for female.

## Examples

``` r
data <- data.frame(sex = c("Male", "Female", "Male"), age = c(25, 30, 35))
align_sex_coding_in_df(data)
#>   sex age
#> 1   1  25
#> 2   2  30
#> 3   1  35
```
