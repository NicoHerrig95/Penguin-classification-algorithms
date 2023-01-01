# Main script
# Second script to run 
# Compares three classification algorithms:
# -> kNN, logistic Regression, Random Forest

library(tidyverse)
library(ggplot2)
library(class)
library(randomForest)
library(nnet)


# source file for functions
source("functions.R")


# reading in cleaned and completed data set
penguins_data <- read_csv("penguins_sex.csv") %>% 
  mutate(species = as.factor(species),
         island = as.factor(island),
         sex = as.factor(sex),
         year = as.factor(year))


# Basic Exploratory data analysis (Overview about data)
# Distribution of observations by species
penguins_data %>% 
  ggplot(aes(y = species, fill = species))+
  geom_bar()+
  geom_text(aes(label = ..count..),
            stat = "count",
            hjust=1.5,
            colour = "black",
            size = 5)+
  theme(legend.position = "none")


penguins_data %>% 
  ggplot(aes(x = bill_length_mm, fill = species))+
  geom_histogram(alpha = 0.5)+
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Bill Length")


penguins_data %>% 
  ggplot(aes(x = bill_depth_mm, fill = species))+
  geom_histogram(alpha = 0.5)+
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Bill Depth")


penguins_data %>% 
  ggplot(aes(x = body_mass_g, fill = species))+
  geom_histogram(alpha = 0.5)+
  theme(legend.position = "top",
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Body Mass")


penguins_data %>% 
  filter(species == "Adelie") %>% 
  group_by(sex) %>% 
  summarise(avg_bill_length = mean(bill_length_mm, na.rm = TRUE),
            avg_bill_depth = mean(bill_depth_mm, na.rm = TRUE),
            avg_flipper_lengt = mean(flipper_length_mm, na.rm = TRUE),
            avg_weight_g = mean(body_mass_g, na.rm = TRUE))


penguins_data %>% 
  filter(species == "Gentoo") %>% 
  group_by(sex) %>% 
  summarise(avg_bill_length = mean(bill_length_mm, na.rm = TRUE),
            avg_bill_depth = mean(bill_depth_mm, na.rm = TRUE),
            avg_flipper_lengt = mean(flipper_length_mm, na.rm = TRUE),
            avg_weight_g = mean(body_mass_g, na.rm = TRUE))


penguins_data %>% 
  filter(species == "Chinstrap") %>% 
  group_by(sex) %>% 
  summarise(avg_bill_length = mean(bill_length_mm, na.rm = TRUE),
            avg_bill_depth = mean(bill_depth_mm, na.rm = TRUE),
            avg_flipper_lengt = mean(flipper_length_mm, na.rm = TRUE),
            avg_weight_g = mean(body_mass_g, na.rm = TRUE))


penguins_data %>% 
  ggplot(aes(x = bill_length_mm, y = body_mass_g, colour = species))+
  geom_point()+
  ggtitle("Cluster 1")+
  theme(plot.title = element_text(hjust = 0.5))


penguins_data %>% 
  ggplot(aes(x = bill_depth_mm, y = body_mass_g, colour = species))+
  geom_point()+
  ggtitle("Cluster 2")+
  theme(plot.title = element_text(hjust = 0.5))


penguins_data %>% 
  ggplot(aes(x = flipper_length_mm, y = body_mass_g, colour = species))+
  geom_point()+
  ggtitle("Cluster 3")+
  theme(plot.title = element_text(hjust = 0.5))




# generate general trainset and testset
# sets are used for all algorithms but are eventually further modified for each
# algorithm individually

set.seed(0911) # setting seed for reproducibility
sets <- set_generator(data = penguins_data) # storage variable for both sets

trainset <- as.data.frame(sets[1])%>%
  rename(species = trainset.species,
         bill_length_mm = trainset.bill_length_mm,
         bill_depth_mm = trainset.bill_depth_mm,
         flipper_length_mm = trainset.flipper_length_mm,
         body_mass_g = trainset.body_mass_g,
         sex = trainset.sex, 
         island = trainset.island,
         year = trainset.year)

testset <- as.data.frame(sets[2])%>%
  rename(species = testset.species,
         bill_length_mm = testset.bill_length_mm,
         bill_depth_mm = testset.bill_depth_mm,
         flipper_length_mm = testset.flipper_length_mm,
         body_mass_g = testset.body_mass_g,
         sex = testset.sex, 
         island = testset.island,
         year = testset.year)

rm(sets) #removing storage variable for both sets


# extracting target columns
trainset_species <- trainset$species
testset_species <- testset$species


# CLASSIFICATION ALGORITHMS

# 1. KNN

# specifically manipulating data sets for kNN algorithm
trainset_knn <- trainset  %>% 
  mutate(bill_length_mm = normalizer(bill_length_mm),
         bill_depth_mm = normalizer(bill_depth_mm),
         flipper_length_mm = normalizer(flipper_length_mm),
         body_mass_g = normalizer(body_mass_g),
         # transforming character factor into numerics
         sex = as.numeric(sex), 
         island = as.numeric(island)) %>% 
  select(- species)
  


testset_knn <- testset %>%
  mutate(bill_length_mm = normalizer(bill_length_mm),
         bill_depth_mm = normalizer(bill_depth_mm),
         flipper_length_mm = normalizer(flipper_length_mm),
         body_mass_g = normalizer(body_mass_g),
         # transforming character factor into numerics
         sex = as.numeric(sex), 
         island = as.numeric(island)) %>% 
  select(- species)


# calculating k
k <- round(sqrt(dim(trainset_knn)[1]))
if (k %% 2 == 1) {
  k = k} else {
    k = k + 1}
  
  
# predicting species via kNN
predictions_kNN <- knn(train = trainset_knn,
                       test = testset_knn,
                       cl = trainset_species,
                       k = k)

# storing accuracy in accuracy_kNN
accuracy_kNN <- accuracy_testing(vector_predictions = predictions_kNN,
                 vector_trueclasses = testset_species)



# 2. logistic regression
# Using a multinominal logit model for predictions with three response
# categories


# manipulating testset for predictions (omitting species)
testset_logistic <- testset %>% 
  select(- species)

# training logistic regression model (logit model)
logit_model1 <- multinom(species ~ island + bill_length_mm + bill_depth_mm +
                           flipper_length_mm + body_mass_g + sex + year,
                         data = trainset)

# computing predictions using logit model
predictions_logit <- predict(logit_model1, testset_logistic)

# computing and storing accuracy 
accuracy_logistic <- accuracy_testing(vector_predictions = predictions_logit,
                                      vector_trueclasses = testset_species)


