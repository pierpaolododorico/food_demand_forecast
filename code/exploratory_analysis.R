# Import packages
library(tidyverse)
library(ggplot2)
library(ggcorrplot)

# Import data
centers = read.csv("data/fulfilment_center_info.csv")
meals = read.csv("data/meal_info.csv")
sales = read.csv("data/train.csv")

# The business problem

"A meal delivery company operates in multiple cities. 
 They have various fulfillment centers in these cities for dispatching meal orders to their customers. 
 We need to provide these centers the demand forecasting for upcoming weeks 
 so that these centers will plan the stock of raw materials accordingly."

# Task

"Predict the demand for the next 10 weeks!"

# Exploration of centers csv

kable(head(centers,10)) 
"
- **center_id**: Fulfilment identifier
- **city_code**: City id in which the center is located on
- **region_code**: Region id in which the center is located on
- **center_type**: Type of the center
- **op_area**: Size of the operational area
"
for (col in seq(length(colnames(centers)))){
  cat("Unique values in",colnames(centers)[col],": ")
  cat(dim(unique(centers[col]))[1], "\n")
}


# Exploration of meals csv
kable(head(meals,10)) 
"
- **meal_id**: Meal identifier
- **category**: Category of food
- **cuisine**: Category of cuisine
"
for (col in seq(length(colnames(meals)))){
  cat("Unique values in",colnames(meals)[col],": ")
  cat(dim(unique(meals[col]))[1], "\n")
}


# Exploration of sales csv
kable(head(sales,10)) 
"
- **id**: Id of the single transaction
- **week**: Temporal variable, we have 145 unique weeks 
- **center_id**: Fulfilment identifier
- **meal_id**: Meal identifier
- **checkout_price**: Paid price for the product
- **base_price**: Full price of the product without promotion
- **emailer_for_promotion**: Binomial variable, it's the product sent by a promotion email?
- **homepage_featured**: Binomial variable, it's the product on website's homepage?
- **num_orders**: Number of orders for the meal and center (it's our target!)
"
for (col in seq(length(colnames(sales)))){
  cat("Unique values in",colnames(sales)[col],": ")
  cat(dim(unique(sales[col]))[1], "\n")
}

# Create unique dataset
X = merge(sales, centers, by = "center_id")
X = merge(X, meals,by = "meal_id")
head(X)

# Search for NAs
print(sum(is.na(X))) # There are no NA values in dataset

# Plot single
plot_single_ts <- function(c_id, m_id, dataset) {
  title = paste("Orders of meal", m_id ,"in center" , c_id)
  ggplot(X[X$meal_id ==m_id & X$center_id == c_id,], aes(x=week, y=num_orders)) +
    geom_line() + xlab("week") + ggtitle(title) +
    xlab("Week") + ylab("Number of orders") }

head(X[order(X$week),]) # dataset ordered by week

X[X$center_id==89, c(4,9)] # extract week and num orders


# aggregate by center

aggregate_c <- function(center, dataset) {
  " Given the center_id and dataset returns the mean orders from the center ts"
  t1 = dataset[dataset$center_id==center, c(4,9)]
  t2 = aggregate(num_orders ~ week, t1, mean)
  t3 = t2[order(t2$week),]
  return(t3)
}

ts89 <- aggregate_c(center = 89, dataset = X)

plot(aggregate_c(center = 89, dataset = X), type = "l")
i = 0
for (c_id in unique(X[,"center_id"])[0:15]){
  i = i + 1
  lines(aggregate_c(center = c_id, dataset = X), type = "l", col = i)
}

# dynamic
for (c_id in unique(X[,"center_id"])){
  title = paste("Mean orders for center", c_id)
  plot(aggregate_c(center = c_id, dataset = X), type = "l", col = "darkred",
       main = title, ylab = "Number of orders", xlab = "Week")
  Sys.sleep(1.5)
}

# aggregate by meal
aggregate_m <- function(meal, dataset) {
  " Given the center_id and dataset returns the mean orders from the center ts"
  t1 = dataset[dataset$meal_id==meal, c(4,9)]
  t2 = aggregate(num_orders ~ week, t1, mean)
  t3 = t2[order(t2$week),]
  return(t3)
}

ts89 <- aggregate_m(meal = 1062, dataset = X)

plot(aggregate_m(meal = 1062, dataset = X), type = "l")
i = 0
for (m_id in unique(X[,"meal_id"])){
  i = i + 1
  lines(aggregate_m(meal = m_id, dataset = X), type = "l", col = i)
}

# dynamic
i = 1
for (m_id in unique(X[,"meal_id"])){
  title = paste("Mean orders for meal number", i, "of 51")
  
  plot(aggregate_m(meal = m_id, dataset = X), type = "l", col = "darkred",
       main = title, ylab = "Number of orders", xlab = "Week")
  i = i + 1
  Sys.sleep(1.5)
}

