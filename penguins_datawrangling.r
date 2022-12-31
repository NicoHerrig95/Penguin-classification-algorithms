# First script to run - includes kNN-algorithm to classify sex of penguins

# install.packages("palmerpenguins")

library(palmerpenguins)
library(tidyverse)
library(ggplot2)
library(class)



source("functions.R")

data <- penguins %>% 
  # dropping observations where predictive features are missing
  drop_na(bill_length_mm, bill_depth_mm, flipper_length_mm)  



# Classification of sex with k-nearest neighbour classification algorithm
# separating training set and testig set with 80/20 separation

set.seed(0911) #reproducibility

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
         island = as.numeric(island))


# generating trainset and testset
train_set <- data_knn[obs_train,] %>% 
  drop_na() #dropping observations where sex is latent
test_set <- data_knn[obs_test,] %>% 
  drop_na()


# extracting target column (sex)
trainset_target <- train_set$sex
testset_target <- test_set$sex


# exluding variable sex from train set and test set
train_set <- train_set %>% 
  select(- sex)

test_set <- test_set %>% 
  select(- sex)


# Algorithm
# defining INITIAL k (using sqare root as rule of thumb)
initial_k <- round(sqrt(dim(train_set)[1]))
if (initial_k %% 2 == 1) {
  initial_k = initial_k} else {
  initial_k = initial_k + 1} # +1 if k is even, as we want odd k


# predictions with kNN algorithm
predictions <- knn(train = train_set,
                   test = test_set,
                   cl = trainset_target,
                   k = initial_k)

# calculating accuracy of kNN algorithm
accuracy_testing(vector_predictions = predictions,
                 vector_trueclasses = testset_target)



#2. Predicting missing values for sex with kNN


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


# exporting data frame as csv. 
write.csv(data,
          "/Users/nicoherrig/Desktop/Data Science Projects/Penguin-classification-algorithm/penguins_sex.csv",
          row.names = FALSE)


