# Title
# Name
# Date
# Description

# Describe the research question you are addressing in this analysis
# ...
# ...
# ...

# load packages
library(tidyverse)
library(lubridate)

# Import data
apple <- read_csv("HistoricalQuotes.csv")

glimpse(apple)

# Data munging
# Clean up the data - 
# Make sure our $ columns are actually numeric
# Make sure the date is actually a date
apple <- apple %>% 
  mutate(Date = mdy(Date),
         Close = parse_number(`Close/Last`),
         Open = parse_number(Open),
         High = parse_number(High),
         Low = parse_number(Low),
         Year = year(Date)
  ) %>% 
  select(-c(`Close/Last`))


# explore data:
summary(apple)

# average share price Open for each of the last 10 yrs
apple %>% 
  group_by(Year) %>% 
  summarise(mean_Open = mean(Open))

# Now try to calculate the year-on-year percentage increase:
# ???

# Global average share price in the last 10 yrs
apple %>%
  summarise(mean_Open = mean(Open))

apple %>% 
  summarise(mean_of_Volume = mean(Volume),
            mean_of_closing = mean(Close))

#apple %>%
#group_by(Date) %>%
#summarise(mean_date = mean(`Close/Last`))

# yearly average share price. 
apple_summary <- apple %>%
  # mutate(month = format(Date, "%m"), year = format(Date, "%Y")) %>%
  mutate (year = format(Date, "%Y")) %>% 
  group_by(year) %>%
  summarise(MEAN = mean(Close))

ggplot(apple_summary, aes(x = MEAN, y = year)) +
  geom_line()

# plot1
ggplot(apple_summary, aes(MEAN, year, group = 1)) +
  geom_point() +
  geom_line() +
  labs(x = "PRICE", 
       y = "YEAR", 
       title = "AVG APPL STOCK PRICE")

# plot2
ggplot(apple_summary, aes(year, MEAN, fill = year)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(x = "YEAR",
       y = "PRICE",
       title = paste("APPL AVG STOCK PRICE IN LAST 10 YRS"))

