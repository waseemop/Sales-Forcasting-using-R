library(tidyverse)
library(forecast)
fsales <- "https://raw.githubusercontent.com/multidis/hult-inter-bus-reports-r/main/forecasting/sales_weekly.csv"
sales <- read_csv(fsales)
nweek_now <- max(sales$Week)
sales_store3 <- sales %>%
filter(Store == 3)
sales_hist <- sales_store3 %>% filter(Week <= nweek_now - 13)
sales_last <- sales_store3 %>% filter(Week > nweek_now - 13)
sales_hist_ts <- ts(sales_hist$Weekly_Sales, frequency = 52)
arima_model <- auto.arima(sales_hist_ts, seasonal.test = "seas")
arima_pred <- forecast(arima_model, h = 13)  # Forecasting next 13 weeks
arma_model <- Arima(sales_hist_ts, order = c(1, 0, 1), seasonal = c(0, 0, 0))
arma_pred <- forecast(arma_model, h = 13)  # Forecasting next 13 weeks
naive_pred <- naive(sales_hist_ts, h = 13)
sales_pred_eval <- data.frame(
  Week = sales_last$Week,
  actual = sales_last$Weekly_Sales,
  arima_pred = as.numeric(arima_pred$mean),
  arma_pred = as.numeric(arma_pred$mean),
  naive_pred = as.numeric(naive_pred$mean)
)


sales_pred_eval %>%
  ggplot(aes(x = Week)) +
  geom_line(aes(y = actual, color = "Actual")) +
  geom_line(aes(y = arima_pred, color = "ARIMA Prediction")) +
  geom_line(aes(y = arma_pred, color = "ARMA Prediction")) +
  geom_line(aes(y = naive_pred, color = "Naïve Prediction")) +
  labs(title = "Sales Forecast Comparison",
       x = "Week",
       y = "Sales") +
  scale_color_manual(name = "Legend", values = c("Actual" = "green", "ARIMA Prediction" = "red", "ARMA Prediction" = "blue", "Naïve Prediction" = "purple"))
print(sales_pred_eval)
sales_pred_eval$actual
View(sales_pred_eval)
