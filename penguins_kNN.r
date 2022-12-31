# install.packages("palmerpenguins")


library(palmerpenguins)
library(tidyverse)
library(ggplot2)
library(Hmisc)
library(class)


data <- penguins %>% 
  # dropping observations where predictive features are missing
  drop_na(bill_length_mm, bill_depth_mm, flipper_length_mm)  


# Basic Exploratory data analysis (Overview about data)
# Distribution of observations by species
data %>% 
  ggplot(aes(y = species, fill = species))+
  geom_bar()+
  geom_text(aes(label = ..count..),
            stat = "count",
            hjust=1.5,
            colour = "black",
            size = 5)+
  theme(legend.position = "none")


data %>% 
  ggplot(aes(x = bill_length_mm, fill = species))+
  geom_histogram(alpha = 0.5)+
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Bill Length")


data %>% 
  ggplot(aes(x = bill_depth_mm, fill = species))+
  geom_histogram(alpha = 0.5)+
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Bill Depth")


data %>% 
  ggplot(aes(x = body_mass_g, fill = species))+
  geom_histogram(alpha = 0.5)+
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Body Mass")


data %>% 
  filter(!is.na(sex)) %>% 
  filter(species == "Adelie") %>% 
  group_by(sex) %>% 
  summarise(avg_bill_length = mean(bill_length_mm, na.rm = TRUE),
            avg_bill_depth = mean(bill_depth_mm, na.rm = TRUE),
            avg_flipper_lengt = mean(flipper_length_mm, na.rm = TRUE),
            avg_weight_g = mean(body_mass_g, na.rm = TRUE))


data %>% 
  filter(!is.na(sex)) %>% 
  filter(species == "Gentoo") %>% 
  group_by(sex) %>% 
  summarise(avg_bill_length = mean(bill_length_mm, na.rm = TRUE),
            avg_bill_depth = mean(bill_depth_mm, na.rm = TRUE),
            avg_flipper_lengt = mean(flipper_length_mm, na.rm = TRUE),
            avg_weight_g = mean(body_mass_g, na.rm = TRUE))


data %>% 
  filter(!is.na(sex)) %>% 
  filter(species == "Chinstrap") %>% 
  group_by(sex) %>% 
  summarise(avg_bill_length = mean(bill_length_mm, na.rm = TRUE),
            avg_bill_depth = mean(bill_depth_mm, na.rm = TRUE),
            avg_flipper_lengt = mean(flipper_length_mm, na.rm = TRUE),
            avg_weight_g = mean(body_mass_g, na.rm = TRUE))


data %>% 
  ggplot(aes(x = bill_length_mm, y = body_mass_g, colour = species))+
  geom_point()+
  ggtitle("Cluster 1")+
  theme(plot.title = element_text(hjust = 0.5))


data %>% 
  ggplot(aes(x = bill_depth_mm, y = body_mass_g, colour = species))+
  geom_point()+
  ggtitle("Cluster 2")+
  theme(plot.title = element_text(hjust = 0.5))


data %>% 
  ggplot(aes(x = flipper_length_mm, y = body_mass_g, colour = species))+
  geom_point()+
  ggtitle("Cluster 3")+
  theme(plot.title = element_text(hjust = 0.5))



# Classification of sex with k-nearest neighbour classification algorithm

# Finding optimal values for kNN algorithm
# Testing accuracy with train set and test set
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
accuracy_initial_k <- sum(predictions == testset_target) / length(predictions)
print(accuracy_initial_k)


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






