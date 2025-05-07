# House Rent Prediction

# Step 1: Install packages & Library----
pacman::p_load(tidyverse, caret, ggplot2, googlesheets4)



# Step 2: Loading the data from Google sheets
gsid = '1RtSyo7n2NCF4QT3U5vDeExUeca81OE9cwWPZXuUeSwc'
sheet_names(gsid)
data1 <- read_sheet(ss=gsid, sheet='houserent')
head(data1)


# Step 3: Data Exploration----
names(data1)
cn1 = c('postedDt', 'bhk','rent','size','floor','areatype','locality', 'city','furnishing', 'tenantPref', 'bathroom','poc')
ct1 = 'Diiiccccccic'
data1 <- read_sheet(ss=gsid, sheet='houserent', col_names = cn1, col_types = ct1, skip=2)
head(data1)

# Summary Statistics
summary(data1)


# Check for missing values
colSums(is.na(data1))
options(scipen = 999)



# Visualize the relationship between features
ggplot(data1, aes(x = size, y=rent)) + geom_point() + geom_smooth(method = 'lm') + labs(title = "Rented Area Size vs House Rent", x='toal Area of house', y='House Rent')


# Step 4: Data Preprocessing----

# Fill missing values
# Convert categorical variables to factors
# remove columns with too many missing values or irrelevant features


# Step 5: Split the Data : Training & Testing----
set.seed(123) # For reproducibility
train_index <- sample(1:nrow(data1), 0.8 * nrow(data1)) #80% for training
train_data <- data1[train_index, ]
test_data <- data1[-train_index, ]
# view(train_data)
sapply(list(train_data, test_data),dim)



# Step 6: Build the Linear Regression Model----
str(data1)
# Fit the model
model <- lm(rent ~ size + bhk, data = train_data)

# Summary of the model
summary(model)
head(test_data)


# Step 7: Make Predictions----
predictions <- predict(model, newdata = test_data)
cbind(test_data %>% select(c(bhk,rent, size)), predictions)
# Compare predictions with actual values
results <- data.frame(Actual = test_data$rent, Predicted = predictions)

head(results)


# Step 8: Evaluate the Model----

# Calculate RMSE
rmse <- sqrt(mean((results$Actual - results$Predicted)^2))
print(paste("RMSE:", rmse))


# Step 9: Visualize Predictions----
ggplot(results, aes(x=Actual, y=Predicted)) + geom_point() + geom_abline(slope=1, intercept = 0, color='red') + labs(title="Actual vs Predicted Rent Price", x="Actual Price", y="Predicted Price")






