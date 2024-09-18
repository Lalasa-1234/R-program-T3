# Load required libraries
library(dplyr)
library(ggplot2)
library(lubridate)
library(caret)

# Load the dataset
dataset <- read.csv("C:/Users/lalasa namepalli/desktop/R/train_revised.csv")
head(dataset)
tail(dataset)
dim(dataset)
str(dataset)
summary(dataset)
sapply(dataset, function(x) sum(is.na(x)))
length(unique(dataset$ride_id))

# Count plots for categorical variables
plot_count_cols <- c('payment_method', 'car_type', 'max_capacity')
for (col in plot_count_cols) {
  ggplot(dataset, aes_string(col)) + 
    geom_bar() + 
    labs(title = col) + 
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
}

# Bar plots for 'travel_from' and 'travel_time'
ggplot(dataset, aes(travel_from)) + geom_bar() + theme_minimal()
ggplot(dataset, aes(travel_time)) + geom_bar() + theme_minimal()

# Create a label for the number of tickets
label <- dataset %>%
  group_by(ride_id) %>%
  summarise(number_of_ticket = n()) %>%
  ungroup()

# Drop duplicates based on 'ride_id'
dataset <- dataset %>% distinct(ride_id, .keep_all = TRUE)

# Merge with the label
dataset <- left_join(dataset, label, by = "ride_id")

# Drop unnecessary columns
dataset <- dataset %>% select(-seat_number, -payment_method, -payment_receipt)

# Drop 'travel_to' column
dataset <- dataset %>% select(-travel_to)

# Concatenate 'travel_date' and 'travel_time' into a new 'date' column
dataset <- dataset %>% mutate(date = paste(travel_date, travel_time))

# Function to extract time features
time_features <- function(df) {
  df$date <- as.POSIXct(df$date)
  df <- df %>%
    mutate(day_of_week = wday(date) - 1,
           day_of_year = yday(date),
           day_of_month = day(date),
           year_woy = paste(year(date), isoweek(date)),
           hour = hour(date),
           minute = minute(date),
           is_weekend = ifelse(day_of_week %in% c(5, 6), 1, 0),
           year = year(date),
           quarter = quarter(date),
           month = month(date))
  return(df)
}

# Apply time features function
dataset_new <- time_features(dataset)

# Scatter plot for 'day_of_month' vs. 'number_of_ticket'
ggplot(dataset_new, aes(x = day_of_month, y = number_of_ticket)) + geom_point() + theme_minimal()

# Scatter plot for 'hour' vs. 'number_of_ticket'
ggplot(dataset_new, aes(x = hour, y = number_of_ticket)) + geom_point() + theme_minimal()

# Convert 'travel_time' to hours
dataset_new$travel_time <- sapply(strsplit(as.character(dataset_new$travel_time), ":"), function(x) {
  as.numeric(x[1]) + as.numeric(x[2]) / 60
})

# Create periods
dataset_new$period <- NA
dataset_new$period[dataset_new$travel_time < 7] <- 'em'
dataset_new$period[dataset_new$travel_time >= 7 & dataset_new$travel_time < 11] <- 'am'
dataset_new$period[dataset_new$travel_time >= 11 & dataset_new$travel_time < 15] <- 'mid'
dataset_new$period[dataset_new$travel_time >= 15 & dataset_new$travel_time < 19] <- 'eve'
dataset_new$period[dataset_new$travel_time >= 19 & dataset_new$travel_time <= 24] <- 'pm'

pcount <- table(dataset_new$period)
dataset_new$hourly_travelers <- log1p(sapply(dataset_new$period, function(x) pcount[x]))

dcount <- table(dataset_new$day_of_year)
dataset_new$daily_travelers <- log1p(sapply(dataset_new$day_of_year, function(x) dcount[x]))

# Prepare transport dataset
transport_dataset <- dataset_new %>%
  mutate(month = recode(month, `1` = 1, `2` = 1, `4` = 2, `5` = 3, `6` = 3,
                        `7` = 3, `8` = 3, `9` = 3, `10` = 3, `11` = 2, `12` = 1)) %>%
  mutate(day_of_month = recode(day_of_month, `1` = 2, `2` = 1, `3` = 1, `4` = 1, 
                               `12` = 1, `13` = 2, `14` = 2, `15` = 2, `16` = 2, 
                               `17` = 2, `18` = 2, `19` = 2, `20` = 2, `21` = 3, 
                               `22` = 3, `23` = 3, `24` = 3, `25` = 3, `26` = 3, 
                               `27` = 3, `28` = 2, `29` = 3, `30` = 3, `31` = 3))

# Function to find the difference between bus arrivals
find_difference_bw_bus <- function(data) {
  data <- data %>%
    arrange(travel_from, date) %>%
    group_by(travel_from) %>%
    mutate(Time_gap_btw_0_1_next_bus = as.numeric(difftime(lead(date), date, units = "hours")),
           Time_gap_btw_0_1_previous_bus = as.numeric(difftime(date, lag(date), units = "hours")),
           Time_gap_btw_0_2_next_bus = as.numeric(difftime(lead(date, 2), date, units = "hours")),
           Time_gap_btw_0_2_previous_bus = as.numeric(difftime(date, lag(date, 2), units = "hours")),
           Time_gap_btw_0_3_next_bus = as.numeric(difftime(lead(date, 3), date, units = "hours")),
           Time_gap_btw_0_3_previous_bus = as.numeric(difftime(date, lag(date, 3), units = "hours")),
           Time_gap_btw_next_previous_bus = as.numeric(difftime(lead(date), lag(date), units = "hours"))) %>%
    fill(c("Time_gap_btw_0_1_next_bus", "Time_gap_btw_0_1_previous_bus", 
           "Time_gap_btw_0_2_next_bus", "Time_gap_btw_0_2_previous_bus",
           "Time_gap_btw_0_3_next_bus", "Time_gap_btw_0_3_previous_bus",
           "Time_gap_btw_next_previous_bus"), .direction = "both")
  return(data)
}

# Apply the function to find differences
data <- find_difference_bw_bus(transport_dataset)

# Drop NA values
data <- na.omit(data)

# Map distances
distance <- c(Migori = 370, Keroka = 280, Homa_Bay = 360, Kisii = 305.1, 
              Keumbu = 295, Rongo = 332, Kijauri = 271, Oyugis = 330.6, 
              Awendo = 351, Sirare = 392, Nyachenge = 326, Kehancha = 387.7, 
              Kendu_Bay = 347, Sori = 399, Rodi = 348, Mbita = 401, 
              Ndhiwa = 371)
data$travel_from_distance <- distance[data$travel_from]

# Map travel times
time <- c(Migori = 7 * 60 + 8, Keroka = 5 * 60, Homa_Bay = 7 * 60, 
          Kisii = 5 * 60 + 34, Keumbu = 5 * 60 + 20, Rongo = 6 * 60 + 21, 
          Kijauri = 60 * 4 + 50, Oyugis = 5 * 60 + 50, Awendo = 6 * 60 + 38, 
          Sirare = 7 * 60 + 30, Nyachenge = 6 * 60 + 10, Kehancha = 7 * 60 + 10, 
          Kendu_Bay = 6 * 60 + 10, Sori = 7 * 60 + 30, Rodi = 6 * 60 + 40, 
          Mbita = 7 * 60 + 23, Ndhiwa = 7 * 60)
data$travel_from_time <- time[data$travel_from]

# Calculate speed
data$Speed <- data$travel_from_time / data$travel_from_distance

# Scatter plot for 'Speed' vs. 'number_of_ticket'
ggplot(data, aes(x = Speed, y = number_of_ticket)) + geom_point() + theme_minimal()

# Ensure travel_from_time is in minutes
data$travel_from_time <- as.numeric(data$travel_from_time)  # Make sure it's numeric

# Calculate arrival date
data$arrival_date <- as.POSIXct(data$date) + (data$travel_from_time * 60)

# Prepare data for modeling
model_data <- data %>%
  select(day_of_week, hour, is_weekend, number_of_ticket, Speed, daily_travelers, hourly_travelers)

# Split the dataset into training and testing sets
set.seed(123)  # For reproducibility
train_index <- createDataPartition(model_data$number_of_ticket, p = 0.8, list = FALSE)
train_data <- model_data[train_index, ]
test_data <- model_data[-train_index, ]

# Fit Linear Regression model
lm_model <- lm(number_of_ticket ~ ., data = train_data)
summary(lm_model)

# Predict on test data
predictions <- predict(lm_model, newdata = test_data)

# Calculate metrics
mse <- mean((test_data$number_of_ticket - predictions) ^ 2)
rmse <- sqrt(mse)
r_squared <- cor(test_data$number_of_ticket, predictions) ^ 2

cat("Linear Regression:\n")
cat("MSE:", mse, "\n")
cat("RMSE:", rmse, "\n")
cat("R-squared:", r_squared, "\n")

# Fit Lasso regression
lasso_model <- train(number_of_ticket ~ ., data = train_data, method = "glmnet", 
                     trControl = trainControl("cv"), 
                     tuneGrid = expand.grid(alpha = 1, lambda = seq(0.01, 0.1, by = 0.01)))

# Predict on test data
lasso_predictions <- predict(lasso_model, newdata = test_data)

# Calculate metrics
lasso_mse <- mean((test_data$number_of_ticket - lasso_predictions) ^ 2)
lasso_rmse <- sqrt(lasso_mse)
lasso_r_squared <- cor(test_data$number_of_ticket, lasso_predictions) ^ 2

cat("Lasso Regression:\n")
cat("MSE:", lasso_mse, "\n")
cat("RMSE:", lasso_rmse, "\n")
cat("R-squared:", lasso_r_squared, "\n")

# Fit Ridge regression
ridge_model <- train(number_of_ticket ~ ., data = train_data, method = "glmnet", 
                     trControl = trainControl("cv"), 
                     tuneGrid = expand.grid(alpha = 0, lambda = seq(0.01, 0.1, by = 0.01)))

# Predict on test data
ridge_predictions <- predict(ridge_model, newdata = test_data)

# Calculate metrics
ridge_mse <- mean((test_data$number_of_ticket - ridge_predictions) ^ 2)
ridge_rmse <- sqrt(ridge_mse)
ridge_r_squared <- cor(test_data$number_of_ticket, ridge_predictions) ^ 2

cat("Ridge Regression:\n")
cat("MSE:", ridge_mse, "\n")
cat("RMSE:", ridge_rmse, "\n")
cat("R-squared:", ridge_r_squared, "\n")

# Hyperparameter tuning for Ridge regression
ridge_grid <- expand.grid(alpha = 0, lambda = seq(0.001, 0.1, length = 10))
ridge_model_tuned <- train(number_of_ticket ~ ., data = train_data, method = "glmnet", 
                           trControl = trainControl("cv"), 
                           tuneGrid = ridge_grid)

# Predict on test data with tuned model
tuned_ridge_predictions <- predict(ridge_model_tuned, newdata = test_data)

# Calculate metrics
tuned_ridge_mse <- mean((test_data$number_of_ticket - tuned_ridge_predictions) ^ 2)
tuned_ridge_rmse <- sqrt(tuned_ridge_mse)
tuned_ridge_r_squared <- cor(test_data$number_of_ticket, tuned_ridge_predictions) ^ 2

cat("Tuned Ridge Regression:\n")
cat("MSE:", tuned_ridge_mse, "\n")
cat("RMSE:", tuned_ridge_rmse, "\n")
cat("R-squared:", tuned_ridge_r_squared, "\n")

