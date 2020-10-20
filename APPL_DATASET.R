install.packages("readxl")
library("readxl")
library(dplyr)
library(tidyverse)
library(ggplot2)
data <- read_excel("C:/Users/ASUS/Downloads//HistoricalQuotes.xlsx")

glimpse(data)

# average share price in the last 10 yrs in opening time
summarise(data, mean_Open =mean(Open))

summarise(data, mean_of_Volume = mean(Volume),
          mean_of_closing = mean(`Close/Last`))

#data %>%
#group_by(Date) %>%
#summarise(mean_date = mean(`Close/Last`))

# yearly average share price. 
p <- data %>%
  # mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>%
  mutate (year = format(Date, "%Y")) %>% 
  group_by(year) %>%
  summarise(MEAN = mean(`Close/Last`))

ggplot(data = p, aes(x = MEAN, y = year)) +
  geom_line()




plot1 <- ggplot(p, aes(MEAN, year, group = 1)) +
  geom_point() +
  geom_line() +
  labs(x = "PRICE", y = "YEAR", 
       title = "AVG APPL STOCK PRICE")

plot1



plot2 <- ggplot(p, aes(x = year, y = MEAN, fill = year)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(
    x = "YEAR",
    y = "PRICE",
    title = paste(
      "APPL AVG STOCK PRICE IN LAST 10 YRS"
    )
  )
plot2
