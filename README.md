# TeenGrowth

#### A package for wrangling and predicting BMI-related data in adolescent samples

#### by Katherine Schaumberg

## Overview

The `TeenGrowth` package includes functions for:

-   Cleaning and processing growth data.

-   Calculating Body Mass Index (BMI) and BMI z-scores (BMIZ).

-   Forecasting growth metrics.

-   Visualizing growth data and weight restoration plans.

## Installation

To install the package using the \`install_github\` function: use the following code:

```         
devtools::install.packages("remotes") 
library(remotes)
install_github("embark-lab/embarktools")
install_github("embark-lab/TeenGrowth")
```

## Usage

Here is a brief example of how to use the `TeenGrowth` package:

```         
# Load the package
library(TeenGrowth)

# Example data
data <- read.csv("path/to/your/data.csv")

# Clean the data
cleaned_data <- clean_raw_data(data)

# Forecast BMI
bmi_forecast <- forecast_bmi(clean_data)

# Plot the results of participant # 1
plot_eBMI(clean_data = cleaned_data, forecast_data = bmi_forecast, px = 1)
```

For a more detailed outline of how to use the R package, refer to the [package vignette](https://embark-lab.github.io/TeenGrowth/articles/Vignette.html).

## Shiny Application

For clinical application, the `TeenGrowth` package includes a [Shiny application](https://embark-lab.shinyapps.io/teengrowthapp/) to provide an interactive interface for data input, model selection, and weight restoration planning.

### Shiny App Structure

The Shiny application is structured into several tabs:

1.  **Data Input**: Upload your data file (CSV or Excel) or use provided demo data.

2.  **Data Specification**: Map your data columns to the required fields.

3.  **Model Selection**: Select a model to run on your data and view the predictions.

4.  **Weight Restoration Planning**: Plan a weight restoration strategy based on the predictions.

5.  **Background and FAQ**: Provides additional information and usage instructions.

### Example Workflow in the Shiny App

1.  **Data Input**: Upload your data or select demo data.

2.  **Data Specification**: Specify the columns corresponding to required fields.

3.  **Model Selection**: Select a model and generate predictions.

4.  **Weight Restoration Planning**: Input relevant parameters and visualize the restoration plan.

## Contributing

We welcome contributions from the community. If you have any suggestions, bug reports, or feature requests, please open an issue or submit a pull request on GitHub.
