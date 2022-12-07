rm(list = ls())
setwd("C:/Users/yatin/OneDrive/Documents/BigDataAnalytics/Pizza Challenge")
#install.packages("tidyverse")
#install.packages("skimr")
#install.packages("janitor")
#install.packages("lubridate")
#install.packages("readxl")
library(tidyverse)
library(skimr)
library(janitor)
library(lubridate)
library(readxl)

pizza_orders <- read.csv("Pizza Sales.csv.csv")
summary(pizza_orders)

#null values check
is.null(pizza_orders)

#number of unique orders
n_unique(pizza_orders$order_id)

#What days and times does Plato's Pizza tend to be the busiest?
pizza_orders %>%
  group_by(month) %>%
  summarize(orders = sum(quantity)) %>%
  ggplot() + 
  geom_col(mapping = aes(x = month, y = orders, fill = orders)) + 
  labs(title = "Monthwise pizzas for Plato's Pizza", y = "Total pizzas")

pizza_orders %>%
  group_by(weekday) %>%
  summarize(orders = sum(quantity)) %>%
  ggplot() + 
  geom_col(mapping = aes(x = weekday, y = orders, fill = weekday)) + 
  labs(title = "Number of pizzas per weekday")


pizza_orders %>%
  group_by(hour) %>%
  summarize(orders = sum(quantity)) %>%
  ggplot() + 
  geom_line(mapping = aes(x = hour, y = orders, color = "red")) + 
  annotate("rect", xmin = 12, xmax = 14, ymin = 0, ymax = 7000, color = "white", fill = "white", alpha = 0.25) + 
  annotate("rect", xmin = 17, xmax = 19, ymin = 0, ymax = 7000, color = "white", fill = "white", alpha = 0.25) + 
  labs(title = "Daily hourly orders", y = "Total orders per hour")

#How many pizzas is the restaurant making during peak periods?

hour_pizzas <- pizza_orders %>%
  group_by(hour) %>%
  summarise(total_pizzas = sum(quantity)) %>%
  mutate(perc = total_pizzas/sum(total_pizzas)) %>%
  mutate(perc = scales::percent(perc))

hour_pizzas[4,2]
hour_pizzas[10,2]

#percentage each time of the day represents from the total pizzas.
pizza_orders <- pizza_orders %>%
  mutate(period = case_when(
    hour >= 6 & hour < 12 ~ "Morning",
    hour >= 12 & hour < 18 ~ "Afternoon",
    hour >= 18 & hour < 24 ~ "Evening"))


period_perc <- pizza_orders %>%
  group_by(period) %>%
  summarise(orders = sum(quantity)) %>%
  mutate(total_perc = orders/sum(orders)) %>%
  mutate(total_perc = scales::percent(total_perc))

period_perc$period <- ordered(period_perc$period, levels = c("Morning", "Afternoon", "Evening"))


ggplot(period_perc, aes(x = period , y = orders, fill = period)) + 
  geom_col() + 
  geom_text(aes(label = orders), position = position_stack(vjust = 0.5)) + 
  labs(title = "Distribution of orders based on the time of day")

#What are the best and worst selling pizzas?

type_pizza <- pizza_orders %>%
  group_by(pizza_name) %>%
  summarise(orders = sum(quantity)) 

ggplot(type_pizza, aes(x = pizza_name, y = orders, fill = orders)) + 
  geom_bar(stat = "identity") + 
  coord_flip() +
  labs(title = "Sales on pizza by type of pizza", x = "Pizza name")

type_pizza %>%
  arrange(desc(orders)) %>%
  slice(1:5)

size_category <- pizza_orders %>%
  group_by(pizza_size) %>%
  summarise(number = sum(quantity)) %>%
  mutate(total_perc = number/sum(number)) %>%
  mutate(total_perc = scales::percent(total_perc))

size_category$pizza_size <- ordered(size_category$pizza_size, levels = c("S", "M", "L", "XL", "XXL"))

ggplot(size_category, aes(x = pizza_size, y = number, fill = pizza_size)) + 
  geom_col() + 
  geom_text(aes(label = number), position = position_stack(vjust = 0.5)) + 
  labs(title = "Sales by size of pizza", x = "Pizza size")

category <- pizza_orders %>%
  group_by(pizza_category) %>%
  summarise(orders = sum(quantity)) %>%
  mutate(total_perc = orders/sum(orders)) %>%
  mutate(total_perc = scales::percent(total_perc)) 

ggplot(category, aes(x = pizza_category, y = orders, fill = pizza_category)) + 
  geom_col() + 
  geom_text(aes(label = orders), position = position_stack(vjust = 0.5)) + 
  labs(title = "Sales breakdown by pizza category", x = "Pizza category")

#What is the average order value?

mean(pizza_orders$total_price)

mean(pizza_orders$quantity)

sum(pizza_orders$quantity)

sum(pizza_orders$total_price)

max(pizza_orders$total_price)

#How well is the restaurant utilizing its seating capacity?

# being July the busiest month, and Friday the busiest day for Plato's Pizza, 
#we will see how well the restaurant utilized its seating capacity on a Friday from July, specifically on Friday 17th

seat_cap <- pizza_orders %>%
  filter(order_date == "17-07-2015")

friday_july <- seat_cap %>%
  group_by(hour) %>%
  summarise(total_pizzas = sum(quantity), orders = n_unique(order_id))

ggplot(friday_july, aes(x = hour, y = orders, fill = orders)) + 
  geom_col() + 
  geom_text(aes(label = orders), position = position_stack(vjust = 0.5)) + 
  labs(title = "Seating capacity on 17th July")





























