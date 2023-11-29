  packages <- c("TSA", "forecast", "readxl", "knitr", "tidyverse", "skimr", 
                "GGally", "tsibble", "kableExtra", "plotly", "viridis", 
                "caret", "randomForest", "e1071", "rpart", "xgboost", "h2o", 
                "corrplot", "rpart.plot", "grid", "corrgram", "ggplot2", 
                "highcharter", "forcats", "ggthemes", "psych", "scales", 
                "repr", "cowplot", "magrittr", "ggpubr","RColorBrewer", 
                "tseries", "plotrix", "ggrepel", "tidyverse", "gridExtra", 
                "lubridate", "zoo")
  if ( length(missing_pkgs <- setdiff(packages, rownames(installed.packages()))) > 0) {
    message("Installing missing package(s): ", paste(missing_pkgs, collapse = ", "))
    install.packages(missing_pkgs)
  }
  lapply(packages, library, character.only = TRUE)
  
  ####Pre-Processing####
  
  df <- read.csv("C:/Users/ASUS/OneDrive/Documents/CSV Hold/avocado.csv")
  head(df)
  
  sum(is.na(df))
  
  df$Date <- as.Date(df$Date, "%Y-%m-%d")
  class(df$Date)
  
  # Sort the dates
  df <- df[order(as.Date(df$Date, format="%Y-%m-%d")),]
  
  
  # Filter by type
  df <- df %>% select(Date, AveragePrice, type)
  
  
  organic <- df %>% select(Date, AveragePrice, type) %>% filter(type == "organic")
  
  conventional <- df %>% select(Date, AveragePrice, type) %>% filter(type == "conventional")
  
  head(organic)
  
  head(conventional)
  
  # Convert 'type' column to factor
  organic$type <- factor(organic$type)
  conventional$type <- factor(conventional$type)
  
  ####Plot for Stationarity####
  
  #####Normal State Plot#####
  
  plot_organic <- ggplot(organic, aes(x = Date, y = AveragePrice, color = type)) +
    geom_line() +
    labs(x = "Date", y = "Average Price") +
    scale_color_manual(values = c("#ED7921")) +
    theme_minimal()
  
  # Create the plot for conventional data
  plot_conventional <- ggplot(conventional, aes(x = Date, y = AveragePrice, color = type)) +
    geom_line() +
    labs(x = "Date", y = "Average Price") +
    scale_color_manual(values = c("#62BE51")) +
    theme_minimal()
  
  # Combine the plots vertically using the `cowplot` package
  combined_plots <- plot_grid(plot_organic, plot_conventional, ncol = 1)
  
  # Display the combined plots
  print(combined_plots)
  
  
  # Combine the organic and conventional data frames
  combined_data <- rbind(organic, conventional)
  combined_data$type <- rep(c("Organic", "Conventional"), times = c(nrow(organic), nrow(conventional)))
  
  # Create the combined plot with distinct colors for "Organic" and "Conventional" (70% opacity)
  combined_plot <- ggplot(combined_data, aes(x = Date, y = AveragePrice, color = type, alpha = type)) +
    geom_line() +
    labs(x = "Date", y = "Average Price") +
    scale_color_manual(values = c("Organic" = "blue", "Conventional" = "red")) +
    scale_alpha_manual(values = c("Organic" = 0.5, "Conventional" = 0.5)) +
    theme_minimal()
  
  # Display the combined plot
  print(combined_plot)
  
  
  
  #####Differencing Plot#####
  
  ######Differencing Conventional#######
  
  # Perform the first differencing on the AveragePrice column within each type
  conventional$FirstDiff <- ave(conventional$AveragePrice, conventional$type, FUN = function(x) c(NA, diff(x)))
  diff_co <- conventional[complete.cases(conventional$FirstDiff), ]
  
  plot_co_diff <- ggplot(conventional, aes(x = Date, y = FirstDiff, color = type)) +
    geom_line() +
    labs(x = "Date", y = "Average Price") +
    scale_color_manual(values = c("#62BE51")) +
    theme_minimal()
  
  # Display the first differencing plot
  print(plot_co_diff)
  
  
  ######Differencing Organic######
  
  
  # Perform the first differencing on the AveragePrice column within each type
  organic$FirstDiff <- ave(organic$AveragePrice, organic$type, FUN = function(x) c(NA, diff(x)))
  diff_or <- organic[complete.cases(organic$FirstDiff), ]
  
  plot_or_diff <- ggplot(organic, aes(x = Date, y = FirstDiff, color = type)) +
    geom_line() +
    labs(x = "Date", y = "Average Price") +
    scale_color_manual(values = c("#ED7921")) +
    theme_minimal()
  
  # Display the first differencing plot
  print(plot_or_diff)

  
######Combined Differencing Plot######
  
  
# Combine the plots vertically using the `cowplot` package
combined_plots <- plot_grid(plot_or_diff, plot_co_diff, ncol = 1)

# Display the combined plots
print(combined_plots)

######Differencing Overall######

# Remove rows with missing values in the FirstDiff column
# Perform the first differencing on the AveragePrice column within each type
combined_data$FirstDiff <- ave(combined_data$AveragePrice, combined_data$type, FUN = function(x) c(NA, diff(x)))
combined_data <- combined_data[complete.cases(combined_data$FirstDiff), ]

# Create the plot for the first differencing
plot_diff <- ggplot(combined_data, aes(x = Date, y = FirstDiff, color = type)) +
  geom_line() +
  labs(x = "Date", y = "First Difference") +
  scale_color_manual(values = c("Organic" = "blue", "Conventional" = "red")) +
  scale_alpha_manual(values = c("Organic" = 0.5, "Conventional" = 0.5)) +
  theme_minimal()

# Display the first differencing plot
print(plot_diff)



####Line Plot For Detecting Seasonality#####
#####By Month#####

# Extract the month from the Date column and format as month names
df$Month <- format(df$Date, "%B")
organic$Month <- format(organic$Date, "%B")
conventional$Month <- format(conventional$Date, "%B")

# Define the order of months
month_order <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")



# Calculate the average price for each month
monthly_avg <- aggregate(AveragePrice ~ Month, df, mean)
monthly_avg_co <- aggregate(AveragePrice ~ Month, conventional, mean)
monthly_avg_or <- aggregate(AveragePrice ~ Month, organic, mean)
monthly_avg_co$Month <- factor(monthly_avg_co$Month, levels = month_order)
monthly_avg_or$Month <- factor(monthly_avg_or$Month, levels = month_order)

# Reorder the month levels in chronological order
monthly_avg$Month <- factor(monthly_avg$Month, levels = month.name)
monthly_avg_co$Month <- factor(monthly_avg$Month, levels = month.name)
monthly_avg_or$Month <- factor(monthly_avg$Month, levels = month.name)


# Create the line plot
line_plot <- ggplot(monthly_avg, aes(x = Month, y = AveragePrice, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 3) +
  scale_y_continuous(breaks = seq(0, max(monthly_avg$AveragePrice), by = 0.1), expand = c(0, 0),
                     sec.axis = sec_axis(~., breaks = NULL, labels = NULL)) +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#FFB6C1"))+
  labs(title = "Overall")

# Create the line plot
line_plot2 <- ggplot(monthly_avg_or, aes(x = Month, y = AveragePrice, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 3) +
  scale_y_continuous(breaks = seq(0, max(monthly_avg_or$AveragePrice), by = 0.1), expand = c(0, 0),
                     sec.axis = sec_axis(~., breaks = NULL, labels = NULL)) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#FFB6C1"))+
  labs(title = "Organic")

# Create the line plot
line_plot3 <- ggplot(monthly_avg_co, aes(x = Month, y = AveragePrice, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 3) +
  scale_y_continuous(breaks = seq(0, max(monthly_avg_co$AveragePrice), by = 0.1), expand = c(0, 0),
                     sec.axis = sec_axis(~., breaks = NULL, labels = NULL)) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#FFB6C1"))+
  labs(title = "Conventional")


# Display the line plot
print(line_plot)


# Combine line plots vertically
stacked_plots <- plot_grid(line_plot2, line_plot3, nrow = 2)

# Display the stacked plots
print(stacked_plots)


#####By Year and Month#####


# Extract the month from the Date column and format as month names
df$Year <- format(df$Date, "%Y")
organic$Year <- format(organic$Date, "%Y")
conventional$Year <- format(conventional$Date, "%Y")

# Calculate the average price for each month and year
monthly_avg2 <- aggregate(AveragePrice ~ Month + Year, df, mean)
monthly_avg_co2 <- aggregate(AveragePrice ~ Month + Year, conventional, mean)
monthly_avg_or2 <- aggregate(AveragePrice ~ Month + Year, organic, mean)

# Reorder the month levels in chronological order
monthly_avg2$Month <- factor(monthly_avg2$Month, levels = month.name)
monthly_avg_co2$Month <- factor(monthly_avg_co2$Month, levels = month.name)
monthly_avg_or2$Month <- factor(monthly_avg_or2$Month, levels = month.name)

# Create the line plot with colored lines and legends
line_plot4 <- ggplot(monthly_avg2, aes(x = Month, y = AveragePrice, group = Year, color = Year)) +
  geom_line() +
  geom_point(color = "red", size = 3) +
  scale_y_continuous(breaks = seq(0, max(monthly_avg2$AveragePrice), by = 0.1), expand = c(0, 0),
                     sec.axis = sec_axis(~., breaks = NULL, labels = NULL)) +
  scale_color_discrete(name = "Year") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#FFB6C1"))+
  labs(title = "Overall")

# Create the line plot with colored lines and legends
line_plot5 <- ggplot(monthly_avg_co2, aes(x = Month, y = AveragePrice, group = Year, color = Year)) +
  geom_line() +
  geom_point(color = "red", size = 3) +
  scale_y_continuous(breaks = seq(0, max(monthly_avg_co2$AveragePrice), by = 0.1), expand = c(0, 0),
                     sec.axis = sec_axis(~., breaks = NULL, labels = NULL)) +
  scale_color_discrete(name = "Year") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#FFB6C1"))+
  labs(title = "Conventional")

# Create the line plot with colored lines and legends
line_plot6 <- ggplot(monthly_avg_or2, aes(x = Month, y = AveragePrice, group = Year, color = Year)) +
  geom_line() +
  geom_point(color = "red", size = 3) +
  scale_y_continuous(breaks = seq(0, max(monthly_avg_or2$AveragePrice), by = 0.1), expand = c(0, 0),
                     sec.axis = sec_axis(~., breaks = NULL, labels = NULL)) +
  scale_color_discrete(name = "Year") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#FFB6C1"))+
  labs(title = "Organic")


# Display the line plot
print(line_plot4)
# Combine line plots vertically
stacked_plots2 <- plot_grid(line_plot5, line_plot6, nrow = 2)

# Display the stacked plots
print(stacked_plots2)

####Modelling####

aggregated_data <- aggregate(AveragePrice ~ Date, data = df, FUN = mean)

ts1 <- ts(aggregated_data$AveragePrice, frequency=52)


aggregated_data2 <- aggregate(AveragePrice ~ Date, data = organic, FUN = mean)

ts2 <- ts(aggregated_data2$AveragePrice, frequency=52)

aggregated_data3 <- aggregate(AveragePrice ~ Date, data = conventional, FUN = mean)

ts3 <- ts(aggregated_data3$AveragePrice, frequency=52)


#####Overall#####

######Overall Model ACF PACF EACF#####

adf.test(ts1)
acf(ts1)
pacf(ts1)
eacf(ts1)

######Overall Model ACF PACF EACF First Differencing######

differenced_ts <- diff(ts1, differences = 1)
adf.test(differenced_ts)
acf(differenced_ts)
pacf(differenced_ts)
eacf(differenced_ts)


######ARIMA Overall#######

# Create an empty data frame to store the results
results <- data.frame(Model = character(),
                      AIC = numeric(),
                      BIC = numeric(),
                      stringsAsFactors = FALSE)

# Iterate through different ARIMA models
for (p in 0:3) {
  for (q in 0:3) {
    # Fit the ARIMA model
    model <- arima(ts1, order = c(p, 1, q))
    
    # Get the AIC and BIC values
    aic <- AIC(model)
    bic <- BIC(model)
    
    # Create a new row in the results data frame
    results <- rbind(results, data.frame(Model = paste("ARIMA(", p, ",", 1, ",", q, ")"),
                                         AIC = aic,
                                         BIC = bic))
  
  }
}

# Find the index of the model with the minimum AIC and BIC values
min_aic <- which.min(results$AIC)
min_bic <- which.min(results$BIC)

# Format the table with highlighting for the best (minimum) AIC and BIC values
formatted_table <- kable(results, align = "c", format = "html") %>%
  kable_styling() %>%
  row_spec(min_aic, bold = TRUE, color = "red") %>%
  row_spec(min_bic, bold = TRUE, color = "red")

# Print the formatted table
formatted_table


#####Organic Model#####


######Organic Model ACF PACF EACF#####

adf.test(ts2)
acf(ts2)
pacf(ts2)
eacf(ts2)

######Organic Model ACF PACF EACF First Differencing######

differenced_ts2 <- diff(ts2, differences = 1)
adf.test(differenced_ts2)
acf(differenced_ts2)
pacf(differenced_ts2)
eacf(differenced_ts2)



results2 <- data.frame(Model = character(),
                      AIC = numeric(),
                      BIC = numeric(),
                      stringsAsFactors = FALSE)


# Iterate through different ARIMA models
for (p in 0:3) {
  for (q in 0:3) {
    # Fit the ARIMA model
    model2 <- arima(ts2, order = c(p, 1, q))
    
    # Get the AIC and BIC values
    aic2 <- AIC(model2)
    bic2 <- BIC(model2)
    
    # Create a new row in the results data frame
    results2 <- rbind(results2, data.frame(Model = paste("ARIMA(", p, ",", 1, ",", q, ")"),
                                         AIC = aic2,
                                         BIC = bic2))
  }

}

# Find the index of the model with the minimum AIC and BIC values
min_aic2 <- which.min(results2$AIC)
min_bic2 <- which.min(results2$BIC)

# Format the table with highlighting for the best (minimum) AIC and BIC values
formatted_table2 <- kable(results2, align = "c", format = "html") %>%
  kable_styling() %>%
  row_spec(min_aic2, bold = TRUE, color = "red") %>%
  row_spec(min_bic2, bold = TRUE, color = "red")

# Print the formatted table
formatted_table2

#####Conventional#####

######Conventional Model ACF PACF EACF#####

adf.test(ts3)
acf(ts3)
pacf(ts3)
eacf(ts3)

######Conventional Model ACF PACF EACF First Differencing######

differenced_ts3 <- diff(ts3, differences = 1)
adf.test(differenced_ts3)
acf(differenced_ts3)
pacf(differenced_ts3)
eacf(differenced_ts3)



results3 <- data.frame(Model = character(),
                      AIC = numeric(),
                      BIC = numeric(),
                      stringsAsFactors = FALSE)


# Iterate through different ARIMA models
for (p in 0:3) {
  for (q in 0:3) {
    # Fit the ARIMA model
    model3 <- arima(ts3, order = c(p, 1, q))
    
    # Get the AIC and BIC values
    aic3 <- AIC(model3)
    bic3 <- BIC(model3)
    
    # Create a new row in the results data frame
    results3 <- rbind(results3, data.frame(Model = paste("ARIMA(", p, ",", 1, ",", q, ")"),
                                         AIC = aic3,
                                         BIC = bic3))
  }
}

# Find the index of the model with the minimum AIC and BIC values
min_aic3 <- which.min(results3$AIC)
min_bic3 <- which.min(results3$BIC)

# Format the table with highlighting for the best (minimum) AIC and BIC values
formatted_table3 <- kable(results3, align = "c", format = "html") %>%
  kable_styling() %>%
  row_spec(min_aic3, bold = TRUE, color = "red") %>%
  row_spec(min_bic3, bold = TRUE, color = "red")

# Print the formatted table
formatted_table3


#####Assumption Test####

#Only testing the most fit model

######Independency######
#overall
checkresiduals(arima(ts1, order = c(2, 1, 2)))

#organic
checkresiduals(arima(ts2, order = c(2, 1, 2)))

#conventional
checkresiduals(arima(ts3, order = c(3, 1, 3)))



######Normality######
#overall
jarque.bera.test(residuals(arima(ts1, order = c(2, 1, 2))))
qqnorm(residuals(arima(ts1, order = c(2, 1, 2))),pch=1, frame=FALSE)
qqline(residuals(arima(ts1, order = c(2, 1, 2))), col = "steelblue", lwd=2)

#organic
jarque.bera.test(residuals(arima(ts2, order = c(2, 1, 2))))
qqnorm(residuals(arima(ts2, order = c(2, 1, 2))),pch=1, frame=FALSE)
qqline(residuals(arima(ts2, order = c(2, 1, 2))), col = "steelblue", lwd=2)

#conventional
jarque.bera.test(residuals(arima(ts3, order = c(3, 1, 3))))
qqnorm(residuals(arima(ts3, order = c(3, 1, 3))),pch=1, frame=FALSE)
qqline(residuals(arima(ts3, order = c(3, 1, 3))), col = "steelblue", lwd=2)

####Notes and Parameter####

#'Jadi Model terbaiknya:
#'Overall: ARIMA(2,1,2)
#'Organic: ARIMA(2,1,2)
#'Conventional: ARIMA(3,1,3)

#overall
arima(ts1, order=c(2,1,2))
#organic
arima(ts2, order=c(2,1,2))
#conventional
arima(ts3, order=c(3, 1, 3))

####Forecasting####


act <- window(ts1)
fit1 <- Arima(ts1, order=c(2,1,2))
forecast2<-forecast::forecast(fit1, h=12)
plot(forecast2, main="Forecast For Overall Type", xlab="Date", ylab="Average")


# Add a legend
legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "blue"), lty = 1)

# Add gridlines
grid(lty = 3)

# Add a horizontal line at the mean value
abline(h = mean(df$AveragePrice), col = "red", lty = 2)



act <- window(ts2)
fit2 <- Arima(ts2, order=c(2,1,2))
forecast3<-forecast::forecast(fit2,h=12)
plot(forecast3, main="Forecast For Organic Type", xlab="Date", ylab="Average")


# Add a legend
legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "blue"), lty = 1)

# Add gridlines
grid(lty = 3)

# Add a horizontal line at the mean value
abline(h = mean(organic$AveragePrice), col = "red", lty = 2)





act <- window(ts3)
fit3 <- Arima(ts3, order=c(3,1,3))
forecast4<-forecast::forecast(fit3,h=12)
plot(forecast4, main="Forecast For Conventional Type", xlab="Date", ylab="Average")


# Add a legend
legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "blue"), lty = 1)

# Add gridlines
grid(lty = 3)

# Add a horizontal line at the mean value
abline(h = mean(conventional$AveragePrice), col = "red", lty = 2)


forecast2
forecast3
forecast4


####Seasonality#####

######Naive Seasonality######

#######Overall#######
# Perform STL decomposition
decomp <- stl(ts1, s.window = "periodic")

# Extract the components
trend <- decomp$time.series[, "trend"]
seasonal <- decomp$time.series[, "seasonal"]
remainder <- decomp$time.series[, "remainder"]

# Plot the components
plot(aggregated_data$Date, trend, type = "l", main = "Trend Component")
plot(aggregated_data$Date, seasonal, type = "l", main = "Seasonal Component")
plot(aggregated_data$Date, remainder, type = "l", main = "Remainder Component")

# Fit the Seasonal Naive model
model4 <- snaive(ts1)

# Forecast future values
forecast_values <- forecast(model4, h = 52)  # Adjust 'h' as needed

# Plot the forecast
plot(forecast_values, main="Forecast For Seasonal Naive Forecast for Overall Type", xlab="Date", ylab="Average")


# Add a legend
legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "blue"), lty = 1)

# Add gridlines
grid(lty = 3)

# Add a horizontal line at the mean value
abline(h = mean(df$AveragePrice), col = "red", lty = 2)


#######Organic#######
# Perform STL decomposition
decomp2 <- stl(ts2, s.window = "periodic")

# Extract the components
trend <- decomp2$time.series[, "trend"]
seasonal <- decomp2$time.series[, "seasonal"]
remainder <- decomp2$time.series[, "remainder"]

# Plot the components
plot(aggregated_data2$Date, trend, type = "l", main = "Trend Component")
plot(aggregated_data2$Date, seasonal, type = "l", main = "Seasonal Component")
plot(aggregated_data2$Date, remainder, type = "l", main = "Remainder Component")

# Fit the Seasonal Naive model
model5 <- snaive(ts2)

# Forecast future values
forecast_values <- forecast(model5, h = 52)  # Adjust 'h' as needed

# Plot the forecast
plot(forecast_values, main="Forecast For Seasonal Naive Forecast for Organic Type", xlab="Date", ylab="Average")


# Add a legend
legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "blue"), lty = 1)

# Add gridlines
grid(lty = 3)

# Add a horizontal line at the mean value
abline(h = mean(organic$AveragePrice), col = "red", lty = 2)


#######Conventional#######
# Perform STL decomposition
decomp3 <- stl(ts3, s.window = "periodic")

# Extract the components
trend <- decomp3$time.series[, "trend"]
seasonal <- decomp3$time.series[, "seasonal"]
remainder <- decomp3$time.series[, "remainder"]

# Plot the components
plot(aggregated_data3$Date, trend, type = "l", main = "Trend Component")
plot(aggregated_data3$Date, seasonal, type = "l", main = "Seasonal Component")
plot(aggregated_data3$Date, remainder, type = "l", main = "Remainder Component")

# Fit the Seasonal Naive model
model6 <- snaive(ts3)

# Forecast future values
forecast_values <- forecast(model6, h = 52)  # Adjust 'h' as needed

# Plot the forecast
plot(forecast_values, main="Forecast For Seasonal Naive Forecast for Conventional Type", xlab="Date", ylab="Average")


# Add a legend
legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "blue"), lty = 1)

# Add gridlines
grid(lty = 3)

# Add a horizontal line at the mean value
abline(h = mean(conventional$AveragePrice), col = "red", lty = 2)



####Model Forecasting Checking####

#####Overall Model#####
length(ts1)
ts11 <- ts1[-length(ts1)]
length(ts11)

# Determine the split point
split_point1 <- length(ts11) %/% 2

# Split the data into train and test sets
train_data1 <- ts11[1:split_point1]
test_data1 <- ts11[(split_point1 + 1):length(ts11)]

# Check the lengths of train and test sets

train_data1 <- window(train_data1)
test_data1 <- window(test_data1)

length(train_data1)
length(test_data1)

# Fit ARIMA model with specified parameters

fit_arima <- function(x, h) {
  model <- Arima(x, order = order)
  forecast_values <- forecast(model, h = h)
  return(forecast_values)
}


order = c(2,1,2)

fitarima1 <- fit_arima(train_data1, h=length(test_data1))


# Generate time axis for the forecast
time_forecast <- seq(0, length(fitarima1$mean) - 1, length.out = length(fitarima1$mean))

# Plot forecast vs actual
plot(time_forecast, fitarima1$mean, type = "l", col = "red", ylim = range(train_data1, fitarima1$mean),
     xlab = "Time", ylab = "Average Price", main = "Forecast vs Actual ARIMA(2,1,2)")
lines(train_data1, col = "blue")
legend("bottomleft", legend = c("Actual", "Forecast"), col = c("blue", "red"), lty = 1)


#Naive#

time_forecast2 <- seq(0, length(model4$mean) - 1, length.out = length(model4$mean))

# Plot forecast vs actual
plot(time_forecast2, model4$mean, type = "l", col = "red", ylim = range(train_data1, model4$mean),
     xlab = "Time", ylab = "Average Price", main = "Forecast vs Actual Naive Seasonal Overall")
lines(train_data1, col = "blue")
legend("top", legend = c("Actual", "Forecast"), col = c("red", "blue"), lty = 1)

#####Organic######

length(ts2)
ts22 <- ts2[-length(ts2)]
length(ts22)

# Determine the split point
split_point2 <- length(ts22) %/% 2

# Split the data into train and test sets
train_data2 <- ts22[1:split_point2]
test_data2 <- ts22[(split_point2 + 1):length(ts22)]

# Check the lengths of train and test sets

train_data2 <- window(train_data2)
test_data2 <- window(test_data2)

length(train_data2)
length(test_data2)

# Fit ARIMA model with specified parameters

order=c(2,1,2)
fitarima2 <- fit_arima(train_data2, h=length(test_data2))


# Generate time axis for the forecast
time_forecast3 <- seq(0, length(fitarima2$mean) - 1, length.out = length(fitarima2$mean))

# Plot forecast vs actual
plot(time_forecast3, fitarima2$mean, type = "l", col = "red", ylim = range(train_data2, fitarima2$mean),
     xlab = "Time", ylab = "Average Price", main = "Forecast vs Actual ARIMA(2,1,2)")
lines(train_data2, col = "blue")
legend("bottomleft", legend = c("Actual", "Forecast"), col = c("blue", "red"), lty = 1)



#Naive#


time_forecast4 <- seq(0, length(model5$mean) - 1, length.out = length(model5$mean))

# Plot forecast vs actual
plot(time_forecast4, model5$mean, type = "l", col = "red", ylim = range(train_data2, model5$mean),
     xlab = "Time", ylab = "Average Price", main = "Forecast vs Actual Naive Seasonal Organic")
lines(train_data2, col = "blue")
legend("top", legend = c("Actual", "Forecast"), col = c("red", "blue"), lty = 1)



#####Conventional######

length(ts3)
ts33 <- ts3[-length(ts3)]
length(ts33)

# Determine the split point
split_point3 <- length(ts33) %/% 2

# Split the data into train and test sets
train_data3 <- ts33[1:split_point3]
test_data3 <- ts33[(split_point3 + 1):length(ts22)]

# Check the lengths of train and test sets

train_data3 <- window(train_data3)
test_data3 <- window(test_data3)

length(train_data3)
length(test_data3)

# Fit ARIMA model with specified parameters

order=c(3,1,3)
fitarima3 <- fit_arima(train_data3, h=length(test_data3))


# Generate time axis for the forecast
time_forecast5 <- seq(0, length(fitarima3$mean) - 1, length.out = length(fitarima3$mean))

# Plot forecast vs actual
plot(time_forecast5, fitarima3$mean, type = "l", col = "red", ylim = range(train_data3, fitarima3$mean),
     xlab = "Time", ylab = "Average Price", main = "Forecast vs Actual ARIMA(3,1,3)")
lines(train_data3, col = "blue")
legend("bottomleft", legend = c("Actual", "Forecast"), col = c("blue", "red"), lty = 1)


#Naive#


time_forecast6 <- seq(0, length(model6$mean) - 1, length.out = length(model6$mean))

# Plot forecast vs actual
plot(time_forecast6, model6$mean, type = "l", col = "red", ylim = range(train_data3, model6$mean),
     xlab = "Time", ylab = "Average Price", main = "Forecast vs Actual Naive Seasonal Conventional")
lines(train_data3, col = "blue")
legend("top", legend = c("Actual", "Forecast"), col = c("red", "blue"), lty = 1)


####Outliers Removed####

df <- df[format(df$Date, "%Y") != "2017", ]

organic <- df %>% select(Date, AveragePrice, type) %>% filter(type == "organic")

conventional <- df %>% select(Date, AveragePrice, type) %>% filter(type == "conventional")

head(organic)

head(conventional)


#####Line Plot For Detecting Seasonality######
######By Month######

# Extract the month from the Date column and format as month names
df$Month <- format(df$Date, "%B")
organic$Month <- format(organic$Date, "%B")
conventional$Month <- format(conventional$Date, "%B")

# Define the order of months
month_order <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")



# Calculate the average price for each month
monthly_avg <- aggregate(AveragePrice ~ Month, df, mean)
monthly_avg_co <- aggregate(AveragePrice ~ Month, conventional, mean)
monthly_avg_or <- aggregate(AveragePrice ~ Month, organic, mean)
monthly_avg_co$Month <- factor(monthly_avg_co$Month, levels = month_order)
monthly_avg_or$Month <- factor(monthly_avg_or$Month, levels = month_order)

# Reorder the month levels in chronological order
monthly_avg$Month <- factor(monthly_avg$Month, levels = month.name)
monthly_avg_co$Month <- factor(monthly_avg$Month, levels = month.name)
monthly_avg_or$Month <- factor(monthly_avg$Month, levels = month.name)


# Create the line plot
line_plot <- ggplot(monthly_avg, aes(x = Month, y = AveragePrice, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 3) +
  scale_y_continuous(breaks = seq(0, max(monthly_avg$AveragePrice), by = 0.1), expand = c(0, 0),
                     sec.axis = sec_axis(~., breaks = NULL, labels = NULL)) +
  scale_fill_gradient(low = "lightblue", high = "blue") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#FFB6C1"))+
  labs(title = "Overall")

# Create the line plot
line_plot2 <- ggplot(monthly_avg_or, aes(x = Month, y = AveragePrice, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 3) +
  scale_y_continuous(breaks = seq(0, max(monthly_avg_or$AveragePrice), by = 0.1), expand = c(0, 0),
                     sec.axis = sec_axis(~., breaks = NULL, labels = NULL)) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#FFB6C1"))+
  labs(title = "Organic")

# Create the line plot
line_plot3 <- ggplot(monthly_avg_co, aes(x = Month, y = AveragePrice, group = 1)) +
  geom_line(color = "blue") +
  geom_point(color = "red", size = 3) +
  scale_y_continuous(breaks = seq(0, max(monthly_avg_co$AveragePrice), by = 0.1), expand = c(0, 0),
                     sec.axis = sec_axis(~., breaks = NULL, labels = NULL)) +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#FFB6C1"))+
  labs(title = "Conventional")


# Display the line plot
print(line_plot)


# Combine line plots vertically
stacked_plots <- plot_grid(line_plot2, line_plot3, nrow = 2)

# Display the stacked plots
print(stacked_plots)


######By Year and Month#####


# Extract the month from the Date column and format as month names
df$Year <- format(df$Date, "%Y")
organic$Year <- format(organic$Date, "%Y")
conventional$Year <- format(conventional$Date, "%Y")

# Calculate the average price for each month and year
monthly_avg2 <- aggregate(AveragePrice ~ Month + Year, df, mean)
monthly_avg_co2 <- aggregate(AveragePrice ~ Month + Year, conventional, mean)
monthly_avg_or2 <- aggregate(AveragePrice ~ Month + Year, organic, mean)

# Reorder the month levels in chronological order
monthly_avg2$Month <- factor(monthly_avg2$Month, levels = month.name)
monthly_avg_co2$Month <- factor(monthly_avg_co2$Month, levels = month.name)
monthly_avg_or2$Month <- factor(monthly_avg_or2$Month, levels = month.name)

# Create the line plot with colored lines and legends
line_plot4 <- ggplot(monthly_avg2, aes(x = Month, y = AveragePrice, group = Year, color = Year)) +
  geom_line() +
  geom_point(color = "red", size = 3) +
  scale_y_continuous(breaks = seq(0, max(monthly_avg2$AveragePrice), by = 0.1), expand = c(0, 0),
                     sec.axis = sec_axis(~., breaks = NULL, labels = NULL)) +
  scale_color_discrete(name = "Year") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#FFB6C1"))+
  labs(title = "Overall")

# Create the line plot with colored lines and legends
line_plot5 <- ggplot(monthly_avg_co2, aes(x = Month, y = AveragePrice, group = Year, color = Year)) +
  geom_line() +
  geom_point(color = "red", size = 3) +
  scale_y_continuous(breaks = seq(0, max(monthly_avg_co2$AveragePrice), by = 0.1), expand = c(0, 0),
                     sec.axis = sec_axis(~., breaks = NULL, labels = NULL)) +
  scale_color_discrete(name = "Year") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#FFB6C1"))+
  labs(title = "Conventional")

# Create the line plot with colored lines and legends
line_plot6 <- ggplot(monthly_avg_or2, aes(x = Month, y = AveragePrice, group = Year, color = Year)) +
  geom_line() +
  geom_point(color = "red", size = 3) +
  scale_y_continuous(breaks = seq(0, max(monthly_avg_or2$AveragePrice), by = 0.1), expand = c(0, 0),
                     sec.axis = sec_axis(~., breaks = NULL, labels = NULL)) +
  scale_color_discrete(name = "Year") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "#FFB6C1"))+
  labs(title = "Organic")


# Display the line plot
print(line_plot4)
# Combine line plots vertically
stacked_plots2 <- plot_grid(line_plot5, line_plot6, nrow = 2)

# Display the stacked plots
print(stacked_plots2)

#####Modelling####

aggregated_data <- aggregate(AveragePrice ~ Date, data = df, FUN = mean)

ts1 <- ts(aggregated_data$AveragePrice, frequency=52)


aggregated_data2 <- aggregate(AveragePrice ~ Date, data = organic, FUN = mean)

ts2 <- ts(aggregated_data2$AveragePrice, frequency=52)

aggregated_data3 <- aggregate(AveragePrice ~ Date, data = conventional, FUN = mean)

ts3 <- ts(aggregated_data3$AveragePrice, frequency=52)


######Overall#####

#######Overall Model ACF PACF EACF#####

adf.test(ts1)
acf(ts1)
pacf(ts1)
eacf(ts1)

#######Overall Model ACF PACF EACF First Differencing######

differenced_ts <- diff(ts1, differences = 1)
adf.test(differenced_ts)
acf(differenced_ts)
pacf(differenced_ts)
eacf(differenced_ts)


########ARIMA Overall#######

# Create an empty data frame to store the results
results <- data.frame(Model = character(),
                      AIC = numeric(),
                      BIC = numeric(),
                      stringsAsFactors = FALSE)

# Iterate through different ARIMA models
for (p in 0:3) {
  for (q in 2:3) {
    # Fit the ARIMA model
    model <- arima(ts1, order = c(p, 1, q))
    
    # Get the AIC and BIC values
    aic <- AIC(model)
    bic <- BIC(model)
    
    # Create a new row in the results data frame
    results <- rbind(results, data.frame(Model = paste("ARIMA(", p, ",", 1, ",", q, ")"),
                                         AIC = aic,
                                         BIC = bic))
    
  }
}

# Find the index of the model with the minimum AIC and BIC values
min_aic <- which.min(results$AIC)
min_bic <- which.min(results$BIC)

# Format the table with highlighting for the best (minimum) AIC and BIC values
formatted_table <- kable(results, align = "c", format = "html") %>%
  kable_styling() %>%
  row_spec(min_aic, bold = TRUE, color = "red") %>%
  row_spec(min_bic, bold = TRUE, color = "red")

# Print the formatted table
formatted_table


#######Organic Model#####


########Organic Model ACF PACF EACF#####

adf.test(ts2)
acf(ts2)
pacf(ts2)
eacf(ts2)

########Organic Model ACF PACF EACF First Differencing######

differenced_ts2 <- diff(ts2, differences = 1)
adf.test(differenced_ts2)
acf(differenced_ts2)
pacf(differenced_ts2)
eacf(differenced_ts2)



results2 <- data.frame(Model = character(),
                       AIC = numeric(),
                       BIC = numeric(),
                       stringsAsFactors = FALSE)


# Iterate through different ARIMA models
for (p in 3:4) {
  for (q in 1:7) {
    # Fit the ARIMA model
    model2 <- arima(ts2, order = c(p, 1, q))
    
    # Get the AIC and BIC values
    aic2 <- AIC(model2)
    bic2 <- BIC(model2)
    
    # Create a new row in the results data frame
    results2 <- rbind(results2, data.frame(Model = paste("ARIMA(", p, ",", 1, ",", q, ")"),
                                           AIC = aic2,
                                           BIC = bic2))
  }
}

# Find the index of the model with the minimum AIC and BIC values
min_aic2 <- which.min(results2$AIC)
min_bic2 <- which.min(results2$BIC)

# Format the table with highlighting for the best (minimum) AIC and BIC values
formatted_table2 <- kable(results2, align = "c", format = "html") %>%
  kable_styling() %>%
  row_spec(min_aic2, bold = TRUE, color = "red") %>%
  row_spec(min_bic2, bold = TRUE, color = "red")

# Print the formatted table
formatted_table2

######Conventional#####

#######Conventional Model ACF PACF EACF#####

adf.test(ts3)
acf(ts3)
pacf(ts3)
eacf(ts3)

#######Conventional Model ACF PACF EACF First Differencing######

differenced_ts3 <- diff(ts3, differences = 1)
adf.test(differenced_ts3)
acf(differenced_ts3)
pacf(differenced_ts3)
eacf(differenced_ts3)



results3 <- data.frame(Model = character(),
                       AIC = numeric(),
                       BIC = numeric(),
                       stringsAsFactors = FALSE)


# Iterate through different ARIMA models
for (p in 0:3) {
  for (q in 0:3) {
    # Fit the ARIMA model
    model3 <- arima(ts3, order = c(p, 1, q))
    
    # Get the AIC and BIC values
    aic3 <- AIC(model3)
    bic3 <- BIC(model3)
    
    # Create a new row in the results data frame
    results3 <- rbind(results3, data.frame(Model = paste("ARIMA(", p, ",", 1, ",", q, ")"),
                                           AIC = aic3,
                                           BIC = bic3))
  }
}

# Find the index of the model with the minimum AIC and BIC values
min_aic3 <- which.min(results3$AIC)
min_bic3 <- which.min(results3$BIC)

# Format the table with highlighting for the best (minimum) AIC and BIC values
formatted_table3 <- kable(results3, align = "c", format = "html") %>%
  kable_styling() %>%
  row_spec(min_aic3, bold = TRUE, color = "red") %>%
  row_spec(min_bic3, bold = TRUE, color = "red")

# Print the formatted table
formatted_table3


######Parameters######
arima(ts1, order=c(1, 1, 3))
arima(ts2, order=c(3, 1, 1))
arima(ts3, order=c(1, 1, 1))

######Assumption Test####

#Only testing the most fit model

#######Independency######
#overall
checkresiduals(arima(ts1, order = c(1, 1, 3)))

#organic
checkresiduals(arima(ts2, order = c(3, 1, 1)))

#conventional
checkresiduals(arima(ts3, order = c(1, 1, 1)))



#######Normality######
#overall
jarque.bera.test(residuals(arima(ts1, order = c(1, 1, 3))))
qqnorm(residuals(arima(ts1, order = c(1, 1, 3))),pch=1, frame=FALSE)
qqline(residuals(arima(ts1, order = c(1, 1, 3))), col = "steelblue", lwd=2)

#organic
jarque.bera.test(residuals(arima(ts2, order = c(3, 1, 1))))
qqnorm(residuals(arima(ts2, order = c(3, 1, 1))),pch=1, frame=FALSE)
qqline(residuals(arima(ts2, order = c(3, 1, 1))), col = "steelblue", lwd=2)

#conventional
jarque.bera.test(residuals(arima(ts3, order = c(1, 1, 1))))
qqnorm(residuals(arima(ts3, order = c(1, 1, 1))),pch=1, frame=FALSE)
qqline(residuals(arima(ts3, order = c(1, 1, 1))), col = "steelblue", lwd=2)


#####Forecasting####

act <- window(ts1)
fit1 <- Arima(ts1, order=c(1,1,3))
forecast2<-forecast::forecast(fit1, h=52)
plot(forecast2, main="Forecast For Overall Type", xlab="Date", ylab="Average")

# Add a legend
legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "blue"), lty = 1)

# Add gridlines
grid(lty = 3)

# Add a horizontal line at the mean value
abline(h = mean(df$AveragePrice), col = "red", lty = 2)



act <- window(ts2)
fit2 <- Arima(ts2, order=c(3,1,1))
forecast3<-forecast::forecast(fit2,h=52)
plot(forecast3, main="Forecast For Organic Type", xlab="Date", ylab="Average")


# Add a legend
legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "blue"), lty = 1)

# Add gridlines
grid(lty = 3)

# Add a horizontal line at the mean value
abline(h = mean(organic$AveragePrice), col = "red", lty = 2)





act <- window(ts3)
fit3 <- Arima(ts3, order=c(1,1,1))
forecast4<-forecast::forecast(fit3,h=52)
plot(forecast4, main="Forecast For Conventional Type", xlab="Date", ylab="Average")


# Add a legend
legend("topleft", legend = c("Actual", "Forecast"), col = c("black", "blue"), lty = 1)

# Add gridlines
grid(lty = 3)

# Add a horizontal line at the mean value
abline(h = mean(conventional$AveragePrice), col = "red", lty = 2)

