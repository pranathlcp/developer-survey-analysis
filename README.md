# Survey Analysis using R

This repository contains an R script that performs data cleaning and statistical sampling methods on a developer survey dataset. The dataset analyzed in this project contains responses regarding online learning resources.

## Table of Contents

1. [Dataset Overview](#dataset-overview)
2. [Cleaning the Dataset](#cleaning-the-dataset)
    - [Helper Functions](#helper-functions)
    - [Data Filtering](#data-filtering)
3. [Defining Study Variables](#defining-study-variables)
4. [Population Parameters](#population-parameters)
5. [Sampling Methods](#sampling-methods)
    - [Simple Random Sampling Without Replacement (SRSWOR)](#simple-random-sampling-without-replacement-srswor)
    - [Stratified Sampling](#stratified-sampling)
        - [Stratification by Age](#stratification-by-age)
        - [Stratification by Region](#stratification-by-region)
6. [Results](#results)

## Dataset Overview

The dataset used for this analysis is named `survey_results_public.csv` and consists of responses from a survey about online learning resources. The dataset contains 89,184 responses across 84 columns.

## Cleaning the Dataset

### Helper Functions

The helper function `string_contains_checker` checks if at least one of the required strings is present in a delimited string.

```
string_contains_checker <- function(input_str, delimiter, required_strs) {
  split_strings = unlist(strsplit(input_str, delimiter))
  
  for (req_str in required_strs) {
    if (req_str %in% split_strings) {
      return (1)
    }
  }
  return (0)
}
```

### Data Filtering
Only the columns "Age", "LearnCode", and "Country" are kept for analysis. Responses are filtered based on whether participants reported using online resources for learning.

```
interested_colnames = c("Age", "LearnCode", "Country")
dataset = dataset[, interested_colnames]
```

## Defining Study Variables
The following binary study variables are defined based on responses:

1. x: 1 if learning using online courses/certifications, and 0 otherwise.
2. y: 1 if learning using other online resources, and 0 otherwise.

## Population Parameters
The population parameters are calculated based on the cleaned dataset, including sample sizes and variances for both x and y.
```
N = dim(dataset)[1]
mu_x.pop = mean(dataset$x)
sigma2_x.pop = var(dataset$x)
```

## Sampling Methods

### Simple Random Sampling Without Replacement (SRSWOR)
This section implements SRSWOR to obtain samples from the dataset. The mean and variance for the sampled variables are calculated.

```
srswor_indices = sample(1:N, n, replace = FALSE)
sampled_df = dataset[srswor_indices, ]
```

### Stratified Sampling
Stratified sampling is conducted by both age and region, allowing for more accurate estimates within each stratum.

#### Stratification by Age
Stratification by age divides the dataset into strata based on age groups, using proportional allocation to determine sample sizes.

```
ages = unique(dataset$Age)
strata.prop.n_A = rep(NA, length(ages))
```

#### Stratification by Region
Similar to age stratification, the dataset is stratified by region to enhance sample representation.

```
regions = unique(dataset$Region)
strata.prop.n_R = rep(NA, length(regions))
```
## Results
The results from both SRSWOR and stratified sampling are saved in CSV files and can be accessed for further analysis.

The results include means, variances, and sample sizes for each stratum.

## Installation
To run the script, ensure you have R and the necessary libraries installed. You can install required packages with:

```
install.packages("countrycode")
```

