# Align Sex Coding

This function detects how the sex variable is coded ('Male/Female',
'M/F', or '0(Male)/1(Female)') and converts them to 1/2.

## Usage

``` r
align_sex_coding(sex)
```

## Arguments

- sex:

  A vector of sex values coded as 'Male/Female', 'M/F',
  '0(Male)/1(Female)', or '1(Male)/2(Female)'.

## Value

A list containing the detected coding system and the aligned sex vector
with values 1 for male and 2 for female.

## Examples

``` r
sex_vector <- c("Male", "Female", "Male")
align_sex_coding(sex_vector)
#> $coding
#> [1] "Male/Female"
#> 
#> $sex
#> [1] "1" "2" "1"
#> 
```
