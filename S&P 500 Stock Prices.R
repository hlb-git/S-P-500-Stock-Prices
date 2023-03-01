
library(tidyverse)
library(janitor)
library(skimr)

# to import the csv file that contains the dataset
read_csv("S&P 500 Stock Prices.csv")

# need to assign the table a name for easier usage
sp500 <- read_csv("S&P 500 Stock Prices.csv")

# to check the column names of the table
names(sp500)

# to take a quick glimpse of the first 6 rows of the table
view(head(sp500))

# this line of code counts the total number of records in our dataset
summarize(sp500, number_of_records = count(sp500))

# this line of code checks the day with the highest volume traded
sp500 %>% select(date, volume) %>% 
  group_by(date) %>%
  summarize(vol_sum = sum(volume)) %>% 
  arrange(-vol_sum)

# this line of code shows the most traded stocks on the highest trading day
sp500 %>% filter(date == '2015-08-24') %>% 
  select(symbol, volume) %>% 
  group_by(symbol) %>%
  summarize(vol_sum = sum(volume)) %>% 
  arrange(-vol_sum)

#
sp500 %>% 
  mutate(weekday = weekdays(date)) %>% 
  select(date, volume, weekday) %>% 
  group_by(weekdays(date)) %>%
  summarize(vol_sum = sum(volume)) %>% 
  arrange(-vol_sum)
  
# to visualize this data, we need to create an object
sp500days <- sp500 %>% mutate(weekday = weekdays(date))
  

# the line of code below creates the plot
sp500days %>% ggplot(aes(reorder(weekday, -volume, sum),volume)) + 
  geom_col( fill = '#189AB4') + theme_grey() +
  labs(title = 'Weekdays vs Volume traded',
       x = 'Weekdays',
       y = 'Volume')

# this line of code shows the day AMZN stock was the most volatile
sp500 %>% filter(symbol == 'AMZN') %>%
  select(symbol, date, open, close) %>% 
  mutate(volatility = abs(open - close)) %>% 
  arrange(-volatility)

#
sp500_in_2014_and_2017 <- sp500 %>% 
  filter(date == '2014-1-2' | date == '2017-12-29') %>% arrange(symbol) %>%
  select(symbol, date, open, close)



price_diff_2014_and_2017 <- sp500_in_2014_and_2017 %>% 
  group_by(symbol) %>% 
  mutate(price_diff = c(NA, diff(close))) %>%
  arrange(-price_diff) %>% 
  print(n = 10)





