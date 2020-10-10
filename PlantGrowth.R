# PlantGrowth Analysis
# Alreaan, Abdullah 

library(tidyverse)

data("PlantGrowth")
PlantGrowth <- as_tibble(PlantGrowth)
PlantGrowth

glimpse(PlantGrowth)

summarise(PlantGrowth, weight_mean = mean(weight))
mean(PlantGrowth$weight)

#average by group 
PlantGrowth %>% 
  group_by(group) %>% 
  summarise(Average = mean(weight))


ggplot(PlantGrowth, aes(x = group, y = weight)) +
  geom_boxplot()


ggplot(PlantGrowth, aes(group, weight)) +
  geom_jitter(width = 0.15) +
  stat_summary(fun.data = mean_sdl, 
               fun.args = list(mult = 1), 
               col = "red")


ggplot(PlantGrowth, aes(x = group, y = weight)) +
  geom_jitter(width = 0.15) +
  stat_summary(col = "red")

plant_lm <- lm(weight ~ group, data = PlantGrowth)
plant_lm

anova(plant_lm)










