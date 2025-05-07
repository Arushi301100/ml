# Load necessary libraries
library(forecast)
library(quantmod)
library(ggplot2)
library(lubridate)

# Your data is already loaded as GOLDBEES.NS
# Extract the closing prices as a time series
gold_ts <- ts(Cl(GOLDBEES.NS))
head(gold_ts)

# Method 1: Auto ARIMA - automatically selects the best ARIMA model
arima_model <- auto.arima(gold_ts)
summary(arima_model)

# Forecast for 30 days
forecast_30d <- forecast(arima_model, h=30)
# Forecast for 1 year (252 trading days approximately)
forecast_1y <- forecast(arima_model, h=252)

# Plot the forecasts
plot(forecast_30d, main="30-Day Forecast for GOLDBEES.NS", xlab="Time", ylab="Price")
plot(forecast_1y, main="1-Year Forecast for GOLDBEES.NS", xlab="Time", ylab="Price")

# For better visualization with ggplot2
# Convert forecasts to data frames with dates
end_date <- max(index(GOLDBEES.NS))
forecast_dates_30d <- seq(end_date + 1, by="day", length.out=30)
forecast_dates_1y <- seq(end_date + 1, by="day", length.out=252)

# For 30-day forecast
forecast_df_30d <- data.frame(date = forecast_dates_30d,
  forecast = forecast_30d$mean,
  lower95 = forecast_30d$lower[,2],
  upper95 = forecast_30d$upper[,2]
)  
  
# Combine historical and forecast data for plotting

historical_df <- data.frame(
  date = index(GOLDBEES.NS),
  price = as.numeric(Cl(GOLDBEES.NS)),
  type = "historical"
)

forecast_df_30d$type <- "forecast"
plot_df_30d <- rbind(
  data.frame(historical_df, lower95=NA, upper95=NA),
  data.frame(date=forecast_df_30d$date, price=forecast_df_30d$forecast, 
             type=forecast_df_30d$type, lower95=forecast_df_30d$lower95, 
             upper95=forecast_df_30d$upper95)
)

# Create the ggplot
ggplot(plot_df_30d, aes(x=date, y=price, color=type)) +
  geom_line() + 
  geom_ribbon(aes(ymin=lower95, ymax=upper95), fill="blue", alpha=0.2) +
  labs(title="GOLDBEES.NS with 30-Day Forecast", y="Price", x="Date") +
  theme_minimal() +
  scale_color_manual(values=c("historical"="black", "forecast"="blue"))



# Method 2: Exponential Smoothing (ETS)
ets_model <- ets(gold_ts)
ets_forecast_30d <- forecast(ets_model, h=30)
ets_forecast_1y <- forecast(ets_model, h=252)

# Plot ETS forecasts
plot(ets_forecast_30d, main="30-Day ETS Forecast for GOLDBEES.NS")
plot(ets_forecast_1y, main="1-Year ETS Forecast for GOLDBEES.NS")

# Method 3: Prophet (Facebook's forecasting tool)
# If you want to use Prophet, first install it:
# install.packages("prophet")
library(prophet)

# Prepare data for Prophet
prophet_df <- data.frame(
  ds = index(GOLDBEES.NS),
  y = as.numeric(Cl(GOLDBEES.NS))
)

# Fit Prophet model
m <- prophet(prophet_df)

# Create future dataframe for 30 days
future_30d <- make_future_dataframe(m, periods = 30)
# Create future dataframe for 1 year
future_1y <- make_future_dataframe(m, periods = 365)

# Forecast
prophet_forecast_30d <- predict(m, future_30d)
prophet_forecast_1y <- predict(m, future_1y)

# Plot Prophet forecast
plot(m, prophet_forecast_30d, main="Prophet 30-Day Forecast")
plot(m, prophet_forecast_1y, main="Prophet 1-Year Forecast")

# Compare models based on accuracy
# Use a training/test split
test_length <- 30  # Use last 30 data points as test set
train_end <- length(gold_ts) - test_length

# Training data
train_ts <- window(gold_ts, end=train_end)

# Test data
test_ts <- window(gold_ts, start=train_end+1)

# Fit models on training data
arima_train <- auto.arima(train_ts)
ets_train <- ets(train_ts)

# Make forecasts
arima_test_forecast <- forecast(arima_train, h=test_length)
ets_test_forecast <- forecast(ets_train, h=test_length)

# Calculate accuracy metrics
arima_accuracy <- accuracy(arima_test_forecast, test_ts)
ets_accuracy <- accuracy(ets_test_forecast, test_ts)

# Print accuracy metrics
print("ARIMA Model Accuracy:")
print(arima_accuracy)
print("ETS Model Accuracy:")
print(ets_accuracy)



# CANARA BANK DATA__________________________________________

getSymbols("CANBK.NS", src="yahoo", from="2020-01-01", to = Sys.Date())
stockData <- data.frame(Date = index(CANBK.NS), coredata(CANBK.NS)) %>% select(Date, price = CANBK.NS.Close)

head(stockData)
str(stockData)
library(prophet)
head(stockData)
prophet_data <- stockData %>% rename(ds = Date, y = price)

m <- prophet(prophet_data)

future <- make_future_dataframe(m, periods = 30)  # Adds 30 calendar days
forecast_prophet <- predict(m, future)
forecast_prophet
tail(forecast_prophet)

# Plot
plot(m, forecast_prophet)
prophet_plot_components(m, forecast_prophet)


# GOLDBEES.NS Data Example:
getSymbols("GOLDBEES.NS", src="yahoo", from="2020-01-01", to = Sys.Date())
stockData <- data.frame(Date = index(GOLDBEES.NS), coredata(GOLDBEES.NS)) %>% select(Date, price = GOLDBEES.NS.Close)

head(stockData)
str(stockData)
library(prophet)
head(stockData)
prophet_data <- stockData %>% rename(ds = Date, y = price)

m <- prophet(prophet_data)

future <- make_future_dataframe(m, periods = 30)  # Adds 30 calendar days
forecast_prophet <- predict(m, future)
forecast_prophet
tail(forecast_prophet)

AirPassengers


# Plot
plot(m, forecast_prophet)
prophet_plot_components(m, forecast_prophet)


