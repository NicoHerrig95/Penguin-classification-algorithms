# Main script
# Second script to run 
# Compares three classification algorithms:
# -> kNN, logistic Regression, Random Forest

library(tidyverse)
library(ggplot2)
library(class)
library(randomForest)
library(nnet)
library(car)
library(effects)


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
  select(- species) # omitting species from set
  


testset_knn <- testset %>%
  mutate(bill_length_mm = normalizer(bill_length_mm),
         bill_depth_mm = normalizer(bill_depth_mm),
         flipper_length_mm = normalizer(flipper_length_mm),
         body_mass_g = normalizer(body_mass_g),
         # transforming character factor into numerics
         sex = as.numeric(sex), 
         island = as.numeric(island)) %>% 
  select(- species) # omitting species from set


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

# model assessment
confusionMatrix(data = predictions_kNN, reference = testset_species)



# 2. logistic regression
# Using a multinominal logit model for predictions with three response
# categories


# defining equation for algorithms
penguin.equation <- as.formula("species ~ island + bill_length_mm + bill_depth_mm +
                           flipper_length_mm + body_mass_g + sex + year")


# manipulating testset for predictions (omitting species)
testset_logistic <- testset %>% 
  select(- species)

# training logistic regression model (logit model)
logit_model1 <- multinom(penguin.equation,
                         data = trainset)

# computing predictions using logit model
predictions_logit <- predict(logit_model1, testset_logistic)


# model assessment
confusionMatrix(data = predictions_logit, reference = testset_species)


# 3. random forest

# manipulating testset for random forest
testset_randomforest <- testset %>% 
  select(- species)


# fitting random forest model
rf.model <- randomForest(penguin.equation, data = trainset)

# computing predictions
predictions_randomforest <- predict(rf.model,
                                    newdata = testset_randomforest)

# calculating accuracy
# model assessment
confusionMatrix(data = predictions_randomforest, reference = testset_species)


# computing importance of features
importance(rf.model)
importance(rf.model, type = 1)



# Results accuracy
results_accuracy <- data.frame(algorithm = c("kNN", "logistic regression", "random forest"),
                               accuracy = c(accuracy_testing(vector_predictions = predictions_kNN,
                                                             vector_trueclasses = testset_species),
                                            accuracy_testing(vector_predictions = predictions_logit,
                                                                           vector_trueclasses = testset_species),
                                            accuracy_testing(vector_predictions = predictions_randomforest,
                                                             vector_trueclasses = testset_species)))



# NOTE: path needs to be altered if function is run! 
# exporting data frame as csv. 
write.csv(results_accuracy,
          "/Users/nicoherrig/Desktop/Data Science Projects/Penguin-classification-algorithm/results_accuracy.csv",
          row.names = FALSE)









