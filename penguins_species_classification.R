# Main script
# Second script to run 
# Compares three classification algorithms:
# -> kNN, logistic Regression, Random Forest

library(tidyverse)
library(ggplot2)
library(class)
library(randomForest)


# source file for functions
source("functions.R")


# reading in cleaned and completed data set
penguins_data <- read_csv("penguins_sex.csv")


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




# generate trainset and testset
set.seed(0911)
sets <- set_generator(data = penguins_data) # storage variable

trainset <- as.data.frame(sets[1])
testset <- as.data.frame(sets[2])
rm(sets) #removing storage variable for both sets


# CLASSIFICATION ALGORITHMS

# 1. KNN

# data manipulation
trainset_knn <- trainset %>%
  mutate(bill_length_mm = normalizer(bill_length_mm),
         bill_depth_mm = normalizer(bill_depth_mm),
         flipper_length_mm = normalizer(flipper_length_mm),
         body_mass_g = normalizer(body_mass_g),
         # transforming character factor into numerics
         sex = as.numeric(sex), 
         island = as.numeric(island))
  






