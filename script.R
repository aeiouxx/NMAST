####1. DATA IMPORT ####
data = read.csv('all_stocks_5yr.csv');
str(data);
summary(data);
####2. DATA CLEANUP    ####
data$date = as.Date(data$date, format = "%Y-%m-%d"); # convert date to date format
colSums(is.na(data)); # check for missing entries 
#   date   open   high    low  close volume   Name 
#      0     11      8      8      0      0      0 
# remove rows with missing values (could also impute but not necessary because we have a lot of data and the missing data seems to be random)
data <- na.omit(data);
###3. ADDITIONAL DATA MANIPULATION ####
data$daily_return <- with(data, (close - open)/open); # add daily return column
data$log_return <- with(data, log(close/open)); # add log return column (tends to be more normally distributed thad raw prices, also time additive)
# install.packages("zoo");
library(zoo);
# Group data by stock name, sort by date, calculate moving average of closing price per 20 days (20-21 trading days in a month), add as a column and remove grouping
data <- data %>% group_by(Name) %>% arrange(date) %>% mutate(ma = rollmean(close, 20, fill = NA, align = "right")) %>% ungroup();
# visualization example:
apple_data <- data %>% filter(Name == "AAPL");
ggplot(apple_data, aes(x = date)) +
  geom_line(aes(y = close), color = "blue") +
  geom_line(aes(y = ma), color = "red") +
  labs(title = "Apple Stock Price and 20-Day Moving Average Over Time",
       x = "Date",
       y = "Price") +
  theme_minimal()

# calculate correlation matrices 
# install.packages("tidyr");
library(tidyr);
company_correlations <- raw_data %>%
  group_by(Name) %>%
  nest() %>%
  mutate(correlation_matrix = map(data, ~ cor(select(.x, open, high, low, close, volume, log_returns), 
                                             use = "complete.obs")))
apple_correlation_matrix <- company_correlations %>%
  filter(Name == "AAPL") %>%
  pull(correlation_matrix) %>%
  .[[1]]
print(apple_correlation_matrix)

# calculate covariance matrices
company_covariances <- data %>%
  group_by(Name) %>%
  nest() %>%
  mutate(covariance_matrix = map(data, ~ cov(select(.x, open, high, low, close, volume, log_returns), 
                                            use = "complete.obs")))
apple_covariance_matrix <- company_covariances %>%
  filter(Name == "AAPL") %>%
  pull(covariance_matrix) %>%
  .[[1]]
print(apple_covariance_matrix)
# descriptive statistics
# data visualization
# regression analysis
# time series
# hypothesis testing
# ANOVA
