
install.packages("palmerpenguins")


library(palmerpenguins)
library(tidyverse)
library(ggplot2)
library(Hmisc)


data <- penguins_raw %>% 
  mutate(Species = as.factor(Species)) %>% 
  select(- `Delta 15 N (o/oo)`,
         - `Delta 13 C (o/oo)`)


# Exploratory data analysis

# Distribution of observations by species
data %>% 
  ggplot(aes(y = Species, fill = Species))+
  geom_bar()+
  geom_text(aes(label = ..count..), stat = "count", hjust=1.5, colour = "black", size = 5)+
  theme(legend.position = "none")
