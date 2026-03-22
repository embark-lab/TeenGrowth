# Clean BMI data This function intakes raw data and creates columns for id, agemos, sex, adult height, BMI, and BMIZ. It handles various formats for sex and height/weight units, calculates age in months, and manages missing columns.

Clean BMI data This function intakes raw data and creates columns for
id, agemos, sex, adult height, BMI, and BMIZ. It handles various formats
for sex and height/weight units, calculates age in months, and manages
missing columns.

## Usage

``` r
clean_data(
  data,
  id_col_name = NULL,
  age_col_name = NULL,
  dob_col_name = NULL,
  date_assessed_col_name = NULL,
  age_unit = NULL,
  sex_col_name = NULL,
  ht_col_name = NULL,
  ht_unit = "cm",
  wt_col_name = NULL,
  wt_unit = "kg",
  bmi_col_name = NULL,
  bmiz_col_name = NULL,
  pct_col_name = NULL,
  data_source = "cdc",
  adult_ht_col_name = NULL,
  age_adult_ht_col_name = NULL,
  ed_aoo_col_name = NULL
)
```

## Arguments

- data:

  A data frame containing the raw data.

- id_col_name:

  The name of the column containing unique identifiers. Defaults to
  NULL.

- age_col_name:

  The name of the column containing age values. Defaults to NULL.

- dob_col_name:

  The name of the column containing date of birth. Defaults to NULL.

- date_assessed_col_name:

  The name of the column containing the date of assessment. Defaults to
  NULL.

- age_unit:

  The unit of age values ('days', 'months', or 'years'). Defaults to
  NULL.

- sex_col_name:

  The name of the column containing sex values. Defaults to NULL.

- ht_col_name:

  The name of the column containing height values. Defaults to NULL.

- ht_unit:

  The unit of height values ('cm' or 'inches'). Defaults to 'cm'.

- wt_col_name:

  The name of the column containing weight values. Defaults to NULL.

- wt_unit:

  The unit of weight values ('kg' or 'lbs'). Defaults to 'kg'.

- bmi_col_name:

  The name of the column containing BMI values. Defaults to NULL.

- bmiz_col_name:

  The name of the column containing BMIZ values. Defaults to NULL.

- pct_col_name:

  The name of the column containing percentile values. Defaults to NULL.

- data_source:

  The source of the data ('cdc' or other). Defaults to 'cdc'.

- adult_ht_col_name:

  The name of the column containing adult height values. Defaults to
  NULL.

- age_adult_ht_col_name:

  The name of the column containing age at adult height values. Defaults
  to NULL.

- ed_aoo_col_name:

  The name of the column containing age at onset of puberty values.
  Defaults to NULL.

## Value

A data frame with columns for id, agemos, sex, adult_height, bmi, and
bmiz.
