x<-read.csv(file.choose())
x
head(x)
summary(x)

# Assuming your data is stored in a data frame named 'df'
# Create a data frame with the provided data
df <- data.frame(
  Sea.Level.Rise = c(0.7175060, 1.2057146, -0.1607830, -0.4759315, 1.1357566, 1.1222097),
  Precipitation = c(13.835237, 40.974084, 42.697931, 5.193341, 78.695280, 76.368331),
  Humidity = c(23.63126, 43.98295, 96.65260, 47.46794, 61.78967, 48.97389),
  Wind.Speed = c(18.492026, 34.249300, 34.124261, 8.554563, 8.001164, 30.398908)
)

# Load the ggplot2 library
library(ggplot2)

# Reshape the data from wide to long format for ggplot
library(reshape2)
df_long <- melt(df, id.vars = NULL)
df_long
# Create a line plot for each variable
ggplot(df_long, aes(x = variable, y = value, group = 1)) +
  geom_line() +
  geom_point() +
  labs(x = "Variable", y = "Value", title = "Trends in Environmental Variables")

summary(y)
# Assuming your data is stored in a data frame named 'df'
# Create a data frame with the provided data
df <- data.frame(
  Sea.Level.Rise = c(0.7175060, 1.2057146, -0.1607830, -0.4759315, 1.1357566, 1.1222097),
  Precipitation = c(13.835237, 40.974084, 42.697931, 5.193341, 78.695280, 76.368331),
  Humidity = c(23.63126, 43.98295, 96.65260, 47.46794, 61.78967, 48.97389),
  Wind.Speed = c(18.492026, 34.249300, 34.124261, 8.554563, 8.001164, 30.398908)
)

# Perform linear regression for each variable against another variable
sea_level_model <- lm(Sea.Level.Rise ~ Precipitation + Humidity + Wind.Speed, data = df)
precipitation_model <- lm(Precipitation ~ Sea.Level.Rise + Humidity + Wind.Speed, data = df)
humidity_model <- lm(Humidity ~ Sea.Level.Rise + Precipitation + Wind.Speed, data = df)
wind_speed_model <- lm(Wind.Speed ~ Sea.Level.Rise + Precipitation + Humidity, data = df)

# Print the summary of each regression model
summary(sea_level_model)
summary(precipitation_model)
summary(humidity_model)
summary(wind_speed_model)

# Create a sequence of years starting from 1970 up to the length of the data
start_year <- 1970
num_years <- nrow(x)
years_sequence <- seq(start_year, by = 1, length.out = num_years)
years_sequence
# Convert years to character format
years_as_char <- as.character(years_sequence)
years_as_char 
# Replace NA Date values with years from the sequence
x$Date[is.na(x$Date)] <- years_sequence
x$Date[is.na(x$Date)]
# Print the updated dataset
print(head(x))


# Load the forecast package
library(forecast)
# Assuming 'Temperature' is your target variable
ts_data <- ts(x$Temperature, start = c(1970), frequency = 1)
ts_data
# Split the data into training and testing sets
train_data <- window(ts_data, end = c(2000))
test_data <- window(ts_data, start = c(2001))
test_data
# Fit an ARIMA model
arima_model <- auto.arima(train_data)
arima_model
# Forecast future temperatures
forecast_result <- forecast(arima_model, h = 10)  # Forecast 10 periods ahead
forecast_result

# Perform a correlation test between CO2 emissions and global temperature
correlation_test <- cor.test(x$CO2.Emissions, x$Temperature)
correlation_test
# Print correlation coefficient and p-value
cat("Correlation coefficient:", correlation_test$estimate, "\n")
cat("p-value:", correlation_test$p.value, "\n")

# Interpret the results
if (correlation_test$p.value < 0.05) {
  cat("Conclusion: There is a significant relationship between CO2 emissions and global temperature.\n")
} else {
  cat("Conclusion: There is no significant relationship between CO2 emissions and global temperature.\n")
}





