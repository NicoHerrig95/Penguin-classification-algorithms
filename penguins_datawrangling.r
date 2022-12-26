
install.packages("palmerpenguins")


library(palmerpenguins)
library(tidyverse)
library(ggplot2)
library(Hmisc)


data <- penguins
  


# Exploratory data analysis

# Distribution of observations by species
data %>% 
  ggplot(aes(y = species, fill = species))+
  geom_bar()+
  geom_text(aes(label = ..count..), stat = "count", hjust=1.5, colour = "black", size = 5)+
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


# Difference between male/female, per species
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


# 1. Predicting body mass using linear regression
# Cluster Visualisation for body mass
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

# Linear Regression Model

# initial model
model_bodymass_full <- lm(data = data,
   body_mass_g ~ species*sex + island + bill_length_mm + bill_depth_mm)

step(model_bodymass) # omitting island due to AIC criterion

model_bodymass <- lm(data = data,
                     body_mass_g ~ species*sex + island + bill_length_mm + bill_depth_mm)




