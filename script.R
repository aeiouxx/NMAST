# install.packages("dplyr")
library(dplyr)
data = read.csv('all_stocks_5yr.csv');
data <- data[data$Name %in% c("JPM", "BAC"), ]
data$date = as.Date(data$date, format = "%Y-%m-%d");
colSums(is.na(data)); # check for missing entries 
data <- na.omit(data); # yeet
data <- data %>%
  group_by(Name) %>%
  arrange(date) %>%
  mutate(change = (close - lag(close))/lag(close) * 100);
# CHI-SQUARE TEST
# 1. KATEGORIE VOLUME
categorize_by_volume <- function(volume) {
  thirdQuantile <- quantile(volume, 0.33)
  twoThirdQuantile <- quantile(volume, 0.66)
  categorize <- function(volume) {
    if (volume < thirdQuantile) {
      return("LOW")
    } else if (volume < twoThirdQuantile) {
      return("MEDIUM")
    } else {
      return("HIGH")
    }
  }
  sapply(volume, categorize)
}
# 2. KATEGORIE RETURN
categorize_change <- function(change, low_threshold, moderate_threshold, high_threshold) {
  abs_change <- abs(change)
  if (is.na(change)) {
    return(NA)
  } else if (abs_change <= low_threshold) {
    return("NONE")
  } else if (abs_change <= moderate_threshold) {
    return("LOW")
  } else if (abs_change <= high_threshold) {
    return("MED")
  } else {
    return("HIG")
  }
}
# 3.
quantiles <- quantile(abs(data$change), probs = c(0.10, 0.50, 0.90), na.rm = TRUE)
low_threshold <- quantiles[1] # 10th percentile
medium_threshold <- quantiles[2] # 50th percentile
high_threshold <- quantiles[3] # 90th percentile
data <- data %>% 
  group_by(Name) %>%
  mutate(volume_category = categorize_by_volume(volume),
         change_category = sapply(change, categorize_change, low_threshold, medium_threshold, high_threshold))
bac_data <- data[data$Name == "BAC",];
jpm_data <- data[data$Name == "JPM",];

bac_contin_table <- table(bac_data$volume_category, bac_data$change_category);
jpm_contin_table <- table(jpm_data$volume_category, jpm_data$change_category);
chisq.test(bac_contin_table);
chisq.test(jpm_contin_table);
