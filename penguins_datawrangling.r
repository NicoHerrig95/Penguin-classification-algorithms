# First script to run - includes kNN-algorithm to classify sex of penguins

# install.packages("palmerpenguins")

library(palmerpenguins)
library(tidyverse)
library(class)



source("functions.R")

data <- penguins %>% 
  # dropping observations where predictive features are missing
  drop_na(bill_length_mm, bill_depth_mm, flipper_length_mm)  



# Classification of sex with k-nearest neighbour classification algorithm
# separating training set and testig set with 80/20 separation


# sampling for train set observations (80% of data)
obs_train <- sample(x = seq(1, nrow(data)),
                    size = nrow(data)*0.8)

# sampling for test set observations (20% of data)
obs_test <- seq(1, nrow(data))[- obs_train]


# Normalization function
normalizer <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}


# manipulating data for kNN algorithm testing
data_knn <- data %>% 
  mutate(bill_length_mm = normalizer(bill_length_mm),
         bill_depth_mm = normalizer(bill_depth_mm),
         flipper_length_mm = normalizer(flipper_length_mm),
         body_mass_g = normalizer(body_mass_g),
         # transforming character factor into numerics
         species = as.numeric(species), 
         island = as.numeric(island),
         year = as.factor(year)) # year into factor



# observations where sex is latent
latent_obs_sex <- which(is.na(data$sex))

data_sex_latent <- data[latent_obs_sex,] %>% 
  mutate(bill_length_mm = normalizer(bill_length_mm),
         bill_depth_mm = normalizer(bill_depth_mm),
         flipper_length_mm = normalizer(flipper_length_mm),
         body_mass_g = normalizer(body_mass_g),
         species = as.numeric(species), 
         island = as.numeric(island))


data_sex_available <- data[-latent_obs_sex,] %>%
  mutate(bill_length_mm = normalizer(bill_length_mm),
         bill_depth_mm = normalizer(bill_depth_mm),
         flipper_length_mm = normalizer(flipper_length_mm),
         body_mass_g = normalizer(body_mass_g),
         species = as.numeric(species), 
         island = as.numeric(island))

# extracting true classification values from non-latent data set
true_class <- data_sex_available$sex


# omitting column sex from data sets
data_sex_latent <- data_sex_latent %>% 
  select(- sex)

data_sex_available <- data_sex_available %>% 
  select(- sex)


# calculating k
initial_k <- round(sqrt(dim(data_sex_available)[1]))
if (initial_k %% 2 == 1) {
  initial_k = initial_k} else {
    initial_k = initial_k + 1} # +1 if k is even, as we want odd k

# predictions
predictions_sex_latent <- knn(train = data_sex_available,
                              test = data_sex_latent,
                              cl = true_class,
                              k = initial_k)


# implementing predicted values into initial data set
data$sex[latent_obs_sex] <- predictions_sex_latent

# NOTE: path needs to be altered if function is run! 
# exporting data frame as csv. 
write.csv(data,
          "/Users/nicoherrig/Desktop/Data Science Projects/Penguin-classification-algorithm/penguins_sex.csv",
          row.names = FALSE)



