##############################
#### Cleaning the Dataset ####
##############################

### HELPER FUNCTIONS ###


# Inputs:
#  input_str: A delimited string
#  delimiter: The delimiter symbol of the input_str
#  required_str: A vector containing strings of which the presence in input_str needs to be checked
# Output: 
#  1 if at least one of the items from required_strs is available in the  input_str, and 0 for otherwise
string_contains_checker <- function(input_str, delimiter, required_strs) {
  split_strings = unlist(strsplit(input_str, delimiter))
  
  for (req_str in required_strs) {
    if (req_str %in% split_strings) {
      return (1)
    }
  }
  return (0)
}

### ----- END ------ ###



dataset = read.csv("survey_results_public.csv")
head(dataset)

# We have 89184 responses
dim(dataset)

### Filtering based on people who use online resources

# We have 84 columns.
colnames(dataset)

# We're only interested in the following columns.
# Age, LearnCode, Country

interested_colnames = c("Age", "LearnCode", "Country")

dataset = dataset[, interested_colnames]
dim(dataset)


# We are interested in the LearnCode column. It has 791 unique values
length(unique(dataset$LearnCode))

# We will consider only the responses where the user has checked the "Online Courses or Certification"
# and "Other online resources (e.g., videos, blogs, forum)" checkboxes.
is_online_learner_values = c("Other online resources (e.g., videos, blogs, forum)", "Online Courses or Certification")

# Apply the function to create the 'IsOnlineLearner' column
dataset$IsOnlineLearner = sapply(dataset$LearnCode, string_contains_checker, delimiter = ";", required_strs = is_online_learner_values)

# We have 12157 responses saying that they don't use online resources for learning
# We have 77027 responses saying that they use an online resource for learning
table(dataset$IsOnlineLearner)

# Now, we will consider only the cases where it says that they use online resources
# for learning (i.e. 77,027 responses)
dataset = dataset[dataset$IsOnlineLearner == 1, ]

## ============================================================================##

### Cleansing based on stratification criteria (Age and Regions)

## Age column is already a categorical variable suitable for us to use directly
unique(dataset$Age)

# Let's get rid of the responses that say "Prefer not to say" (246 responses)
dim(dataset[dataset$Age == "Prefer not to say", ])
dataset = dataset[dataset$Age != "Prefer not to say", ]

# New Dataset dimensions (76,781 records)
dim(dataset)
table(dataset$Age)



## Adding a Region Column
# We have records from 183 different countries, and we have to assign the Region 
# for each record
unique(dataset$Country)

library(countrycode)

dataset$Region = countrycode(sourcevar = dataset[, "Country"],
                             origin = "country.name",
                             destination = "region")

# Nomadic doesn't get mapped to a Region
unique(dataset$Region)
unique(dataset[is.na(dataset$Region), "Country"])

# Nomadic refers to people who don't have a permanent country/location
# Therefore, we will remove the 46 records
# Now we have 76,735 records
dim(dataset[dataset$Country == 'Nomadic',])
dataset = dataset[dataset$Country != "Nomadic", ]
dim(dataset)

# Now, we have records from North America, East Asia & Pacific, Europe & Central Asia,
# South Asia, Sub-Saharan Africa, Latin America & Caribbean, Middle East & North Africa
unique(dataset$Region)
table(dataset$Region)

## ============================================================================##



###########################################################################
##################### Defining the Study Variables ########################
###########################################################################

## 1) x = 1 if learning using online courses/certifications, and 0 for otherwise
## 2) y = 1 if learning using other online resources, and 0 for otherwise

dataset$x = sapply(dataset$LearnCode, string_contains_checker, delimiter = ";",
                   required_strs = c("Online Courses or Certification"))
dataset$y = sapply(dataset$LearnCode, string_contains_checker, delimiter = ";",
                   required_strs = c("Other online resources (e.g., videos, blogs, forum)"))

sum(dataset$x)
sum(dataset$y)


###########################################################################
################## Defining the Population Parameters #####################
###########################################################################

# Finally, this will be our cleaned dataset.
# Therefore, our population total (N) will be the number of observations in 'dataset'
# N = 76735
N = dim(dataset)[1]
N

# mu_x.pop = 0.5612823
mu_x.pop = mean(dataset$x)
mu_x.pop

# sigma2_x.pop = 0.2462477
sigma2_x.pop = var(dataset$x)
sigma2_x.pop

# mu_y.pop = 0.9119567
mu_y.pop = mean(dataset$y)
mu_y.pop

# sigma2_y.pop = 0.0802927
sigma2_y.pop = var(dataset$y)
sigma2_y.pop


## Sample Size Calculation
alpha = 0.05
error = 0.01
Z_alphaBy2 = qnorm(alpha/2, lower.tail = FALSE)
Z_alphaBy2

# n_x = 8,421
n_x = round( ( Z_alphaBy2^2 * sigma2_x.pop ) / ( error^2 + (Z_alphaBy2^2 * sigma2_x.pop / N) ) )
n_x

# n_y = 2,965
n_y = round( ( Z_alphaBy2^2 * sigma2_y.pop ) / ( error^2 + (Z_alphaBy2^2 * sigma2_y.pop / N) ) )
n_y

# We set n = n_x
n = n_x

# Fraction of n/N
frac = n/N

set.seed(1654)


###########################################################################
######### 2.1 Simple Random Sampling Without Replacement (SRSWOR) #########
###########################################################################

## ============================== ##
## SRSWOR to be implemented below ##

srswor_indices = sample(1:N, n, replace = FALSE)
sampled_df = dataset[srswor_indices, ]

### (1) x_i

## (a) Sample Mean

# xBar.srswor = 0.564541
xBar.srswor = mean(sampled_df$x)
xBar.srswor


## (b) The design-based variance of xBar.srswor is given by:

# V_xBar.srswor = 2.603303e-05
V_xBar.srswor = (1 - n/N) * (sigma2_x.pop / n)
V_xBar.srswor

## (c) An unbiased variance estimator for xBar.srswor is given by:

# s2_x.srswor = 0.2458637
s2_x.srswor = (1/(n-1)) * sum( (sampled_df$x - xBar.srswor)^2 )
s2_x.srswor

# v_xBar.srswor = 2.599243e-05
v_xBar.srswor = (1 - n/N) * (s2_x.srswor/n)
v_xBar.srswor

### (2) y_i

## (a) Sample Mean

# yBar.srswor = 0.9120057
yBar.srswor = mean(sampled_df$y)
yBar.srswor


## (b) The design-based variance of yBar.srswor is given by:

# V_xBar.srswor = 8.488454e-06
V_yBar.srswor = (1 - n/N) * (sigma2_y.pop / n)
V_yBar.srswor

## (c) An unbiased variance estimator for yBar.srswor is given by:

# s2_y.srswor = 0.08026083
s2_y.srswor = (1/(n-1)) * sum( (sampled_df$y - yBar.srswor)^2 )
s2_y.srswor

# v_yBar = 8.485085e-06
v_yBar.srswor = (1 - n/N) * (s2_y.srswor/n)
v_yBar.srswor

# Saving the results
results.srswor = data.frame("Study Variable" = rep(NA, 2), "mean (yBar)" = rep(NA, 2), 
                            "Design-based variance ( V(yBar) )"  = rep(NA, 2),
                            "Unbiased Var Est ( vHat(yBar) )" = rep(NA, 2), 
                            check.names = FALSE)

results.srswor[1, ] = list("x", xBar.srswor, V_xBar.srswor, v_xBar.srswor)
results.srswor[2, ] = list("y", yBar.srswor, V_yBar.srswor, v_yBar.srswor)
# write.csv(results.srswor, "results/results.srswor.csv", row.names = FALSE)

## SRSWOR to be implemented above ##
## ============================== ##




###########################################################################
######################### 2.2 Stratified Sampling #########################
###########################################################################

### --------------------------- ###
### 2.2.1 Stratification by AGE ###
### --------------------------- ###

# We have 7 Strata
ages = unique(dataset$Age)
ages

strata.N_A = Wh_A = strata.sigma2.x_A = strata.sigma2.y_A = rep(NA, length(ages))

###############################
### PROPORTIONAL ALLOCATION ###
###############################

# sample size, sample mean, sample variance of each strata
strata.prop.n_A = strata.prop.avg_Ax = strata.prop.var_Ax = strata.prop.avg_Ay = 
  strata.prop.var_Ay = rep(NA, length(ages))

for(i in 1:length(ages)) {
  age = ages[i]
  filtered_df = dataset[which(dataset$Age == age), ]
  strata.N_A[i] = dim(filtered_df)[1]
  Wh_A[i] = strata.N_A[i]/N
  strata.sigma2.x_A[i] = var(filtered_df$x)
  strata.sigma2.y_A[i] = var(filtered_df$y)
  
  strata.prop.n_A[i] = ceiling(frac*strata.N_A[i])
  
  # In the last iteration, we need to make sure that sum(strata.prop.n_A) = n
  if (i == length(ages) && sum(strata.prop.n_A) > n) {
    strata.prop.n_A[which.max(strata.prop.n_A)] = 
      strata.prop.n_A[which.max(strata.prop.n_A)] - (sum(strata.prop.n_A) - n)
  }
  
  set.seed(1654)
  sampled_indices = sample(1:strata.N_A[i], strata.prop.n_A[i], replace = FALSE)
  
  prop.x = filtered_df$x
  strata.sample.SRSWOR_Ax = prop.x[sampled_indices]
  strata.prop.avg_Ax[i] = mean(strata.sample.SRSWOR_Ax, na.rm = TRUE)
  strata.prop.var_Ax[i] = var(strata.sample.SRSWOR_Ax, na.rm = TRUE)
  
  prop.y = filtered_df$y
  strata.sample.SRSWOR_Ay = prop.y[sampled_indices]
  strata.prop.avg_Ay[i] = mean(strata.sample.SRSWOR_Ay, na.rm = TRUE)
  strata.prop.var_Ay[i] = var(strata.sample.SRSWOR_Ay, na.rm = TRUE)
}


result.prop.Age = data.frame(Age = ages, Nh = strata.N_A,nh = strata.prop.n_A, 
                        AverageX = strata.prop.avg_Ax, VarianceX = strata.prop.var_Ax,
                        Sigma2_X = strata.sigma2.x_A, AverageY = strata.prop.avg_Ay,
                        VarianceY = strata.prop.var_Ay, Sigma2_Y = strata.sigma2.y_A,
                        Wh = Wh_A)
result.prop.Age
# write.csv(result.prop.Age, "results/result.prop.Age.csv", row.names = FALSE)

### (a) Unbiased Mean Estimator (Proportional Allocation) 
# xBar.prop.Age.str = 0.5552669
# yBar.prop.Age.str = 0.9120478
xBar.prop.Age.str = sum(result.prop.Age$Wh*result.prop.Age$AverageX)
xBar.prop.Age.str
yBar.prop.Age.str = sum(result.prop.Age$Wh*result.prop.Age$AverageY)
yBar.prop.Age.str

### (b) The design-based variance of xBar.str and yBar.str are given by:
# V_xBar.prop.Age.str = 2.585287e-05
# V_yBar.prop.Age.str = 8.481539e-06
V_xBar.prop.Age.str = sum( result.prop.Age$Wh^2 * (1 - result.prop.Age$nh / result.prop.Age$Nh ) * ( result.prop.Age$Sigma2_X / result.prop.Age$nh )  )
V_xBar.prop.Age.str

V_yBar.prop.Age.str = sum( result.prop.Age$Wh^2 * (1 - result.prop.Age$nh / result.prop.Age$Nh ) * ( result.prop.Age$Sigma2_Y / result.prop.Age$nh )  )
V_yBar.prop.Age.str


### (c) An unbiased variance estimator for v_xBar.prop.Age.str and v_yBar.prop.Age.str are given by:
# v_xBar.prop.Age.str = 2.590949e-05
# v_xBar.prop.Age.str = 8.465155e-06
v_xBar.prop.Age.str = sum( result.prop.Age$Wh^2 * (1 - result.prop.Age$nh / result.prop.Age$Nh ) * ( result.prop.Age$VarianceX / result.prop.Age$nh )  )
v_xBar.prop.Age.str

v_yBar.prop.Age.str = sum( result.prop.Age$Wh^2 * (1 - result.prop.Age$nh / result.prop.Age$Nh ) * ( result.prop.Age$VarianceY / result.prop.Age$nh )  )
v_yBar.prop.Age.str




#########################
### NEYMAN ALLOCATION ###
#########################

# sample size, sample mean, sample variance of each strata
strata.neym.n_Ax = strata.neym.n_Ay = strata.neym.avg_Ax = strata.neym.var_Ax =
  strata.neym.avg_Ay = strata.neym.var_Ay = rep(NA, length(ages))

for(i in 1:length(ages)) {
  age = ages[i]
  filtered_df = dataset[which(dataset$Age == age), ]

  ## Handing the x-variable
  nh_denominator.x = sum(strata.N_A*strata.sigma2.x_A)
  strata.neym.n_Ax[i] = ceiling(n*strata.N_A[i]*strata.sigma2.x_A[i] / nh_denominator.x)
  
  # In the last iteration, we need to make sure that sum(strata.neym.n_A) = n
  if (i == length(ages) && sum(strata.neym.n_Ax) > n) {
    strata.neym.n_Ax[which.max(strata.neym.n_Ax)] = 
      strata.neym.n_Ax[which.max(strata.neym.n_Ax)] - (sum(strata.neym.n_Ax) - n)
  }
  
  set.seed(1654)
  sampled_indices.x = sample(1:strata.N_A[i], strata.neym.n_Ax[i], replace = FALSE)
  
  neym.x = filtered_df$x
  strata.sample.SRSWOR_Ax = neym.x[sampled_indices.x]
  strata.neym.avg_Ax[i] = mean(strata.sample.SRSWOR_Ax, na.rm = TRUE)
  strata.neym.var_Ax[i] = var(strata.sample.SRSWOR_Ax, na.rm = TRUE)
  
  
  ## Handling the y-variable
  nh_denominator.y = sum(strata.N_A*strata.sigma2.y_A)
  strata.neym.n_Ay[i] = ceiling(n*strata.N_A[i]*strata.sigma2.y_A[i] / nh_denominator.y)
  
  # In the last iteration, we need to make sure that sum(strata.neym.n_A) = n
  if (i == length(ages) && sum(strata.neym.n_Ay) > n) {
    strata.neym.n_Ay[which.max(strata.neym.n_Ay)] = 
      strata.neym.n_Ay[which.max(strata.neym.n_Ay)] - (sum(strata.neym.n_Ay) - n)
  }
  
  set.seed(1654)
  sampled_indices.y = sample(1:strata.N_A[i], strata.neym.n_Ay[i], replace = FALSE)
  
  neym.y = filtered_df$y
  strata.sample.SRSWOR_Ay = neym.y[sampled_indices.y]
  strata.neym.avg_Ay[i] = mean(strata.sample.SRSWOR_Ay, na.rm = TRUE)
  strata.neym.var_Ay[i] = var(strata.sample.SRSWOR_Ay, na.rm = TRUE)
}


result.neym.Age = data.frame(Age = ages, Nh = strata.N_A,nh.x = strata.neym.n_Ax, 
                             AverageX = strata.neym.avg_Ax, VarianceX = strata.neym.var_Ax,
                             Sigma2_X = strata.sigma2.x_A, nh.y = strata.neym.n_Ay, 
                             AverageY = strata.neym.avg_Ay, VarianceY = strata.neym.var_Ay, 
                             Sigma2_Y = strata.sigma2.y_A, Wh = Wh_A)

result.neym.Age
# write.csv(result.neym.Age, "results/result.neym.Age.csv", row.names=FALSE)

### (a) Unbiased Mean Estimator (Neyman Allocation) 
# xBar.neym.Age.str = 0.5554036
# yBar.neym.Age.str = 0.9123841
xBar.neym.Age.str = sum(result.neym.Age$Wh*result.neym.Age$AverageX)
xBar.neym.Age.str
yBar.neym.Age.str = sum(result.neym.Age$Wh*result.neym.Age$AverageY)
yBar.neym.Age.str

### (b) The minimized variance is given by:
# V_xBar.neym.Age.str = 2.5852e-05
# V_yBar.neym.Age.str = 8.46117e-06
V_xBar.neym.Age.str = (1/n) * ( sum(result.neym.Age$Wh * sqrt(result.neym.Age$Sigma2_X)) )^2 - 
  (1/N) * sum( result.neym.Age$Wh * result.neym.Age$Sigma2_X )
V_xBar.neym.Age.str

V_yBar.neym.Age.str = (1/n) * ( sum(result.neym.Age$Wh * sqrt(result.neym.Age$Sigma2_Y)) )^2 - 
  (1/N) * sum( result.neym.Age$Wh * result.neym.Age$Sigma2_Y )
V_yBar.neym.Age.str
  

### (c) An unbiased variance estimator for v_xBar.neym.Age.str and v_yBar.neym.Age.str are given by:
# v_xBar.neym.Age.str = 2.590999e-05
# v_yBar.neym.Age.str = 8.415813e-06
v_xBar.neym.Age.str = sum( result.neym.Age$Wh^2 * (1 - result.neym.Age$nh.x / result.neym.Age$Nh ) * ( result.neym.Age$VarianceX / result.neym.Age$nh.x )  )
v_xBar.neym.Age.str

v_yBar.neym.Age.str = sum( result.neym.Age$Wh^2 * (1 - result.neym.Age$nh.y / result.neym.Age$Nh ) * ( result.neym.Age$VarianceY / result.neym.Age$nh.y )  )
v_yBar.neym.Age.str




### ------------------------------ ###
### 2.2.1 Stratification by REGION ###
### ------------------------------ ###

# We have 7 Strata
regions = unique(dataset$Region)
regions

strata.N_R = Wh_R = strata.sigma2.x_R = strata.sigma2.y_R = rep(NA, length(regions))

###############################
### PROPORTIONAL ALLOCATION ###
###############################

# sample size, sample mean, sample variance of each strata
strata.prop.n_R = strata.prop.avg_Rx = strata.prop.var_Rx = strata.prop.avg_Ry = 
  strata.prop.var_Ry = rep(NA, length(regions))

for(i in 1:length(regions)) {
  region = regions[i]
  filtered_df = dataset[which(dataset$Region == region), ]
  strata.N_R[i] = dim(filtered_df)[1]
  Wh_R[i] = strata.N_R[i]/N
  strata.sigma2.x_R[i] = var(filtered_df$x)
  strata.sigma2.y_R[i] = var(filtered_df$y)
  
  strata.prop.n_R[i] = ceiling(frac*strata.N_R[i])
  
  # In the last iteration, we need to make sure that sum(strata.prop.n_R) = n
  if (i == length(regions) && sum(strata.prop.n_R) > n) {
    strata.prop.n_R[which.max(strata.prop.n_R)] = 
      strata.prop.n_R[which.max(strata.prop.n_R)] - (sum(strata.prop.n_R) - n)
  }
  
  set.seed(1654)
  sampled_indices = sample(1:strata.N_R[i], strata.prop.n_R[i], replace = FALSE)
  
  prop.x = filtered_df$x
  strata.sample.SRSWOR_Rx = prop.x[sampled_indices]
  strata.prop.avg_Rx[i] = mean(strata.sample.SRSWOR_Rx, na.rm = TRUE)
  strata.prop.var_Rx[i] = var(strata.sample.SRSWOR_Rx, na.rm = TRUE)
  
  prop.y = filtered_df$y
  strata.sample.SRSWOR_Ry = prop.y[sampled_indices]
  strata.prop.avg_Ry[i] = mean(strata.sample.SRSWOR_Ry, na.rm = TRUE)
  strata.prop.var_Ry[i] = var(strata.sample.SRSWOR_Ry, na.rm = TRUE)
}


result.prop.Region = data.frame(Region = regions, Nh = strata.N_R, nh = strata.prop.n_R,
                                AverageX = strata.prop.avg_Rx, VarianceX = strata.prop.var_Rx,
                                Sigma2_X = strata.sigma2.x_R, AverageY = strata.prop.avg_Ry,
                                VarianceY = strata.prop.var_Ry, Sigma2_Y = strata.sigma2.y_R,
                                Wh = Wh_R)
result.prop.Region
# write.csv(result.prop.Region, "results/result.prop.Region.csv", row.names = FALSE)

### (a) Unbiased Mean Estimator (Proportional Allocation) 
# xBar.prop.Region.str = 0.5588952
# yBar.prop.Region.str = 0.9145488
xBar.prop.Region.str = sum(result.prop.Region$Wh*result.prop.Region$AverageX)
xBar.prop.Region.str
yBar.prop.Region.str = sum(result.prop.Region$Wh*result.prop.Region$AverageY)
yBar.prop.Region.str

### (b) The design-based variance of xBar.prop.Region.str and yBar.prop.Region.str are given by:
# V_xBar.prop.Region.str = 2.540813e-05
# V_yBar.prop.Region.str = 8.450971e-06
V_xBar.prop.Region.str = sum( result.prop.Region$Wh^2 * (1 - result.prop.Region$nh / result.prop.Region$Nh ) * ( result.prop.Region$Sigma2_X / result.prop.Region$nh )  )
V_xBar.prop.Region.str

V_yBar.prop.Region.str = sum( result.prop.Region$Wh^2 * (1 - result.prop.Region$nh / result.prop.Region$Nh ) * ( result.prop.Region$Sigma2_Y / result.prop.Region$nh )  )
V_yBar.prop.Region.str


### (c) An unbiased variance estimator for xBar.prop.Region.str and yBar.prop.Region.str are given by:
# v_xBar.prop.Region.str = 2.544691e-05
# v_xBar.prop.Region.str = 8.227684e-06
v_xBar.prop.Region.str = sum( result.prop.Region$Wh^2 * (1 - result.prop.Region$nh / result.prop.Region$Nh ) * ( result.prop.Region$VarianceX / result.prop.Region$nh )  )
v_xBar.prop.Region.str

v_yBar.prop.Region.str = sum( result.prop.Region$Wh^2 * (1 - result.prop.Region$nh / result.prop.Region$Nh ) * ( result.prop.Region$VarianceY / result.prop.Region$nh )  )
v_yBar.prop.Region.str




#########################
### NEYMAN ALLOCATION ###
#########################

# sample size, sample mean, sample variance of each strata
strata.neym.n_Rx = strata.neym.n_Ry = strata.neym.avg_Rx = strata.neym.var_Rx =
  strata.neym.avg_Ry = strata.neym.var_Ry = rep(NA, length(regions))

for(i in 1:length(regions)) {
  region = regions[i]
  filtered_df = dataset[which(dataset$Region == region), ]
  
  ## Handing the x-variable
  nh_denominator.x = sum(strata.N_R*strata.sigma2.x_R)
  strata.neym.n_Rx[i] = ceiling(n*strata.N_R[i]*strata.sigma2.x_R[i] / nh_denominator.x)
  
  # In the last iteration, we need to make sure that sum(strata.neym.n_Rx) = n
  if (i == length(regions) && sum(strata.neym.n_Rx) > n) {
    strata.neym.n_Rx[which.max(strata.neym.n_Rx)] = 
      strata.neym.n_Rx[which.max(strata.neym.n_Rx)] - (sum(strata.neym.n_Rx) - n)
  }
  
  set.seed(1654)
  sampled_indices.x = sample(1:strata.N_R[i], strata.neym.n_Rx[i], replace = FALSE)
  
  neym.x = filtered_df$x
  strata.sample.SRSWOR_Rx = neym.x[sampled_indices.x]
  strata.neym.avg_Rx[i] = mean(strata.sample.SRSWOR_Rx, na.rm = TRUE)
  strata.neym.var_Rx[i] = var(strata.sample.SRSWOR_Rx, na.rm = TRUE)
  
  
  ## Handling y-variable
  nh_denominator.y = sum(strata.N_R*strata.sigma2.y_R)
  strata.neym.n_Ry[i] = ceiling(n*strata.N_R[i]*strata.sigma2.y_R[i] / nh_denominator.y)
  
  # In the last iteration, we need to make sure that sum(strata.neym.n_Ry) = n
  if (i == length(regions) && sum(strata.neym.n_Ry) > n) {
    strata.neym.n_Ry[which.max(strata.neym.n_Ry)] = 
      strata.neym.n_Ry[which.max(strata.neym.n_Ry)] - (sum(strata.neym.n_Ry) - n)
  }
  
  set.seed(1654)
  sampled_indices.y = sample(1:strata.N_R[i], strata.neym.n_Ry[i], replace = FALSE)
  
  neym.y = filtered_df$y
  strata.sample.SRSWOR_Ry = neym.y[sampled_indices.y]
  strata.neym.avg_Ry[i] = mean(strata.sample.SRSWOR_Ry, na.rm = TRUE)
  strata.neym.var_Ry[i] = var(strata.sample.SRSWOR_Ry, na.rm = TRUE)
}


result.neym.Region = data.frame(Region = regions, Nh = strata.N_R,nh.x = strata.neym.n_Rx,
                                AverageX = strata.neym.avg_Rx, VarianceX = strata.neym.var_Rx,
                                Sigma2_X = strata.sigma2.x_R, nh.y = strata.neym.n_Ry,
                                AverageY = strata.neym.avg_Ry, VarianceY = strata.neym.var_Ry,
                                Sigma2_Y = strata.sigma2.y_R, Wh = Wh_R)

result.neym.Region
# write.csv(result.neym.Region, "results/result.neym.Region.csv", row.names = FALSE)

### (a) Unbiased Mean Estimator (Neyman Allocation) 
# xBar.neym.Region.str = 0.5607461
# yBar.neym.Region.str = 0.9142223
xBar.neym.Region.str = sum(result.neym.Region$Wh*result.neym.Region$AverageX)
xBar.neym.Region.str
yBar.neym.Region.str = sum(result.neym.Region$Wh*result.neym.Region$AverageY)
yBar.neym.Region.str

### (b) The minimized variance is given by:
# V_xBar.neym.Region.str = 2.536263e-05
# V_yBar.neym.Region.str = 8.377309e-06
V_xBar.neym.Region.str = (1/n) * ( sum(result.neym.Region$Wh * sqrt(result.neym.Region$Sigma2_X)) )^2 - 
  (1/N) * sum( result.neym.Region$Wh * result.neym.Region$Sigma2_X )
V_xBar.neym.Region.str

V_yBar.neym.Region.str = (1/n) * ( sum(result.neym.Region$Wh * sqrt(result.neym.Region$Sigma2_Y)) )^2 - 
  (1/N) * sum( result.neym.Region$Wh * result.neym.Region$Sigma2_Y )
V_yBar.neym.Region.str


### (c) An unbiased variance estimator for xBar.neym.Region.str and yBar.neym.Region.str are given by:
# v_xBar.neym.Region.str = 2.539535e-05
# v_yBar.neym.Region.str = 8.21731e-06
v_xBar.neym.Region.str = sum( result.neym.Region$Wh^2 * (1 - result.neym.Region$nh.x / result.neym.Region$Nh ) * ( result.neym.Region$VarianceX / result.neym.Region$nh.x )  )
v_xBar.neym.Region.str

v_yBar.neym.Region.str = sum( result.neym.Region$Wh^2 * (1 - result.neym.Region$nh.y / result.neym.Region$Nh ) * ( result.neym.Region$VarianceY / result.neym.Region$nh.y )  )
v_yBar.neym.Region.str
