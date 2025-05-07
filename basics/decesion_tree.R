 # Decision Tree using MTcars

# Predict whether a car has automatic or manual transmission (am) using other features like mpg, hp, wt, etc.

# 0 for automatic, 1 for manual

# install packages & libraries
#install.packages("rpart")
#install.packages("rpart.plot")

# 1. Load libraries
library(rpart)
library(rpart.plot)
library(dplyr)
library(tidyverse)


# 2. prepare & load the data
data(mtcars)

# Convert 'am' (transmission) to factor for classification
mtcars$am <- factor(mtcars$am, label = c("Automatic", "Manual"))


# View structure
str(mtcars)


# 3. Split Data into Train & Test Sets

set.seed(123) # for reproducibility

# 70% training and 30% testing
sample_idx <- sample(1:nrow(mtcars), 0.7 * nrow(mtcars))
train_data <- mtcars[sample_idx, ]
test_data <- mtcars[-sample_idx, ]

sapply(list(train_data, test_data), dim)
table(train_data$am)
df1 <- train_data %>% filter(wt>=2.965)
table(df1$am)
df2 <- train_data %>% filter(wt < 2.865)
table(df2$am)


# 4. Train the Decision Tree Model
dt_model <- rpart(am ~., data = train_data, method = "class")

# Print Model
print(dt_model)

# Plot the tree
rpart.plot(dt_model, extra = 106) #extra = 106 shows prob and % at each mode

# 5. Make Predictions

# Predict on test data
predictions <- predict(dt_model, test_data, type = "class")

# View predicted  values
print(predictions)
test_data
cbind(test_data, predictions)

# 6.Evaluate Model Accuracy

# Confusion matrix
conf_matrix <- table(Predicted = predictions, Actual = test_data$am)

# Print confusion matrix
print(conf_matrix)

# Accuracy
accuracy <- sum(diag(conf_matrix)) / sum(conf_matrix)
cat("Accuracy : ", round(accuracy * 100, 2), "%\n")




# Decision tree of Iris dataset----

# check the data
head(iris)

# for reproducibility
set.seed(1234)

# 70% training and 30% testing
sample_idx <- sample(1:nrow(iris), 0.7 * nrow(iris))
train_data <- iris[sample_idx, ]
test_data <- iris[-sample_idx, ]

sapply(list(train_data, test_data), names)

sapply(list(train_data, test_data),dim)
table(train_data$am)

df1 <- train_data %>% filter(Petal.Length<2.45)
table(df1$am)
df2 <- train_data %>% filter(Petal.Length>=2.45)
table(df2$am)

train_data %>% filter(Petal.Length < 2.5) %>% tally()
31/105

train_data %>% filter(!Petal.Length < 2.5) %>% tally()
74/105


train_data %>% filter(!Petal.Length < 2.5) %>% filter(Petal.Width < 1.75) %>% tally()
39/105




# Train decision tree model:

dt_model <- rpart(Species ~ ., data = train_data, method = "class")
print(dt_model)
rpart.plot(dt_model, nn=T)

printcp(dt_model)
summary(dt_model)


iris.rp = rpart(Species ~., method = 'class', data = iris, control = rpart.control(minsplit=10, cp=0.005, maxdepth = 5, minbucket = round))
print(iris.rp)
test_data
printcp(iris.rp)
rpart.plot(dt_model, extra = 104, cex=1.0)
prediction <- predict(dt_model,test_data, type = 'class')
test_data

sample1 = data.frame(Sepal.Length=4.0, Sepal.Width=3.5, Petal.Length=2.6, Petal.Width=1.76)
predict(dt_model, sample1, type = 'class')


# View Predicted Values
print(prediction)
cbind(test_data, prediction)

conf_matrix <- table(Predicted = prediction, Actual = test_data$am)
print(conf_matrix)


 # Plot the tree
 rpart.plot(iris.rp, extra = 106) #extra = 106 shows prob and % at each mode
view(iris)

#?rpart



# Compute stats using sales dataset in ML----

# -------------------------------------------
# Here we want to understand sales data using Descriptive statistics

# Predict future sales using ML models (ed. Linear Regression, Decision trees)

# Gain Insights for decision-making
#--------------------------------------------



# Create dummy sales data

set.seed (123)

sales <- data.frame(
  date = as.Date('2024-01-01') + 0:29,
  store = sample(c("StoreA", "StoreB"), 30, replace = TRUE),
  product = sample(c('Laptop', 'Tablet', 'Phone'), 30, replace = T),
  units_sold = sample(5:50, 30, replace = T),
  price = sample(300:1000, 30, replace = T)
)


# Add revenue column
sales$revenue <- sales$units_sold * sales$price


# View first few rows
head(sales)


# Compute Descriptive Stats (Mean, Median, etc.)
summary(sales$revenue)
mean(sales$revenue)
median(sales$revenue)
sd(sales$revenue)


# Visualize Data
library(ggplot2)
sales %>% ggplot(., aes(y = revenue)) + geom_histogram(binwidth = 800, fill = "red", color = 'black') + labs(title = "Revenue Distribution")

# Bar Chart
sales %>% ggplot(., aes(x=factor(date), y=revenue, fill = product)) + geom_bar(stat = 'identity')


# apply ML model : Linear Regression----

# Goal: Predict revenue based on units sold and price.

# Build linear model
model <- lm(revenue ~ units_sold + price, data = sales)
head(sales)


# Summary of model
summary(model)


# Predict new Revenue using the Model :
new_data <- data.frame(units_sold = c(20, 35), price = c(600, 750))
predict(model, newdata = new_data)



# Evaluate the Model :
predicted <- predict(model, sales)
actual <- sales$revenue

# Root Mean Squared Error (RMSE)
rmse <- sqrt(mean((predicted - actual)^2))
rmse

(sqrt(mean((actual)^2)) *10/ 100)




# 1 Create dataset	data.frame()
# 2	Descriptive stats	summary(), mean()
# 3	Visualization	ggplot2::ggplot()
# 4	Model revenue prediction	lm()
# 5	Evaluate prediction error	predict(), rmse

