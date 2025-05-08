# Full Project on Stock Market Statistical Analysis:

# loading required packages
#install.packages(c("quantmod", "PerformanceAnalytics", "TTR", "tidyquant"))
library(quantmod)
library(PerformanceAnalytics)
library(TTR)
library(tidyverse)
library(lubridate)
library(forecast)
library(tidyquant)

# Function to fetch and forecast the data:
predict_stock <- function() {
  
  # Prompt user for ticker and date range
  ticker <- readline(prompt = "Enter the Stock/ETF ticker (e.g., SILVERBEES.NS): ")
  start_date <- readline(prompt = "Enter the start Date (yyyy-mm-dd): ")
  end_date <- readline(prompt = "Enter end date (yyyy-mm-dd): ")
  
  # Convert input dates to date class:
  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  
  # Get Data
  tryCatch({
    getSymbols(ticker, src="yahoo", from = start_date, to = end_date, auto.assign = TRUE)
    
    # Extract closing Prices
    data_xts <- get(ticker)
    close_prices <- Cl(data_xts)
    
    # Convert to ts object for forecasting (assume daily frequency)
    ts_data <- ts(as.numeric(close_prices), frequency = 252) # approx trading days
    
    # Forecast using auto.arima
    fit <- auto.arima(ts_data)
    forecast_value <- forecast(fit, h=30)
    
    # Plot actual + forecast
    plot(forecast_value, main = paste("Forecast for ", ticker))
    
    # Print forecast table
    print(forecast_value)
  }, error = function(e){
    cat("Error fetchingthe data, check ticker or dates. \n")
    cat("Details: ", e$message, "\n")
  })
}

# Run the function:
predict_stock()



# Get the Data
stock <- SILVERBEES.NS
getSymbols('SILVERBEES.NS', src="yahoo", from="2020-01-01", to=Sys.Date())
head(SILVERBEES.NS)
#head(stock)

# Visualize the Stock Price
chartSeries(SILVERBEES.NS, type = 'line', theme = chartTheme("white"), TA=NULL)


# Calculate Daily/Monthly Returns
daily_returns <- dailyReturn(Cl(SILVERBEES.NS))
monthly_returns <- monthlyReturn(Cl(SILVERBEES.NS))

# Plot Returns
chart.CumReturns(daily_returns, main="Cumulative Daily Returns: SILVERBEES")


# Statistical Summary:
summary(daily_returns)
sd(daily_returns)
mean(daily_returns)


# Moving Averages & Indicators____________________________

# 20 day and 50 day Moving Average
SILVERBEES.NS$MA20 <- SMA(Cl(SILVERBEES.NS), n=20)
SILVERBEES.NS$MA50 <- SMA(Cl(SILVERBEES.NS), n=50)

# Plot with SMA
chartSeries(SILVERBEES.NS, TA = "addSMA(n=20);addSMA(n=50)", theme = chartTheme("white"))


# Technical Indicators
# RSI (Relative Strength Index)
SILVERBEES.NS$RSI14 <- RSI(Cl(SILVERBEES.NS), n=14)
SILVERBEES.NS_MACD <- MACD(Cl(SILVERBEES.NS), nFast = 12, nSlow=26, nSig = 9)

# Bollinger Bands
bb <- BBands(HLC(SILVERBEES.NS))
chartSeries(SILVERBEES.NS, TA = "addBBands()", theme = chartTheme("white"))


# Correlation with Other Stocks:
#getSymbols(c('SILVERBEES.NS', 'ZSILF', 'GOLDBEES.NS', 'HUZ.TO'), src = "yahoo", from = "2020-01-01")
tickers <- c('SILVERBEES.NS', 'GOLDBEES.NS', 'HUZ.TO')
getSymbols(tickers, src="yahoo", from = "2023-01-01")

# Calculate daily returns individually
silver <- dailyReturn(Cl(SILVERBEES.NS))
gold <- dailyReturn(Cl(GOLDBEES.NS))
huz <- dailyReturn(Cl(HUZ.TO))

# Combine all returns into one data frame
returns <- na.omit(merge(silver, gold, huz))
colnames(returns) <- c('SILVERBEES.NS', 'GOLDBEES', 'HUZ')

# Check first few rows
head(returns)
cor(returns)


# Forecasting (ML/Time Series Step):
# We can use time series forecasting with ARIMA, Prophet, or ML Models:

library(forecast)
auto_fit <- auto.arima(Cl(SILVERBEES.NS))
forecasted <- forecast(auto_fit, h=30)
plot(forecasted)










