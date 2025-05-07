# Time Series Prediction of Stock Market:

# (1). Get ETF data (using quantmod or tidyquant package to download from yahoo Finance):
pacman::p_load(tidyverse, googlesheets4, patchwork, ggplot2, dplyr, xts)
library(quantmod)
library(forecast)

# Download data from Yahoo Finance:
getSymbols("GOLDBEES.NS", src="yahoo", from="2015-01-01", to = Sys.Date())
head(GOLDBEES.NS)
tail(GOLDBEES.NS)


#View(GOLDBEES.NS): This gives you the historical price of that ETF
GOLDBEES.NS %>% as.data.frame() %>% slice(1:5,(n()-5):n()) %>% select(GOLDBEES.NS.Close)
GOLDBEES.NS %>% as.data.frame() %>% select(GOLDBEES.NS.Close) %>% mutate(date = rowid_to_column(.)) %>% head()

# Calculate monthly average of closing price
monthly_avg <- period.apply(Cl(GOLDBEES.NS), endpoints(GOLDBEES.NS, on = "months"), FUN = colMeans)
head(monthly_avg, 12)
monthly_avg['2022:2025']


# (2). Visualize the Data:
gold_data <- data.frame(date=index(GOLDBEES.NS), coredata(GOLDBEES.NS))
ggplot(gold_data, aes(x=date, y=GOLDBEES.NS.Close)) + 
  geom_line(color="blue") +
  labs(title = "GOLD ETF Closing Prices", y="Price", x="Date")

# Another Graph making strategy:
etf_data <- GOLDBEES.NS$GOLDBEES.NS.Close
autoplot(etf_data) + ggtitle("Historical Closing Prices of GOLD")

# Convert to Time Series Format:
etf_ts <- ts(etf_data, frequency = 12)  # ~252 trading days/year

# Fit ARIMA Model for Forecasting:
fit <- auto.arima(etf_ts)
summary(fit)

# Example ____________________________________________
(x = round(runif(365, 50, 100)))
y1 <- ts(x, start=c(2024, 1, 1), frequency = 365)
m1 <- ts(x, start=c(2024, 1, 1), frequency = 12)
m1

m2 <- ts(x, start=c(2024, 1, 1), end=c(2025, 9, 1), frequency = 12)
plot(m2)
m2



# Forecast for Specific Future Date or Period:
# (You can forecast for next 30 days or weeks by adjusting frequency and aggregation)

future_forecast <- forecast(fit, h=30) # h = number of days
head(future_forecast)
autoplot(future_forecast) + 
  ggtitle("Forcasted GOLD prices (next 30 days)") +
  xlab("Days Ahead") + ylab("Price")


# Forecast for specific fture date:
future_date <- as.Date("2025-12-12")
days_ahead <- as.numeric(future_date - max(index(etf_data)))
predicted_value <- forecast(fit, h=days_ahead)$mean[days_ahead]
print(paste("Predicted value on", future_date, 'is', round(predicted_value, 2)))


# Visualize Historical + Forecasted data together:
autoplot(forecast(fit, h=30)) +
  autolayer(fitted(fit), series = "Fitted") +
  ggtitle("GOLD Forecast woth fitted values") +
  ylab("Closing Price") + xlab("Time")



# (3). Prepare Data for Modeling:
library(tidymodels)
library(lubridate)


gold_ts <- gold_data %>% 
  select(date, close = GOLDBEES.NS.Close) %>%
  mutate(lag1 = lag(close),
         lag2 = lag(close, 2)) %>%
  drop_na()



# (4).Train ML Model (eg. Random Forest, XGBoost)
library(randomForest)
set.seed(123)
model <- randomForest(close ~ lag1 + lag2, data=gold_ts)
gold_ts$predicted <- predict(model, gold_ts)


# (5). Predict Future Price:
latest <- tail(gold_ts, 1)
new_data <- data.frame(lag1 = latest$close, lag2 = latest$lag1)
predict(model, new_data)
head(new_data)

# (6). Extend with Time Series Models:

