library(tidyr)
library(dplyr)
library(glmnet)
library(plotmo)

# Load ztc_emotion_metrics
ztc_emotion_metrics <- readRDS("data/ztc_emotion_metrics.rds")

# Add recording column back in
ztc_emotion_metrics <- ztc_emotion_metrics %>%
  mutate(recording = substr(session, 5, 5))




#Get last time point for each session, task

ztc_utterings <- readRDS("data/ztc_utterings.rds") #Load utterings
# Add task column
ztc_utterings <- ztc_utterings %>%
  mutate(task = substring(bundle, 6, 6))

end_times <- ztc_utterings %>%
  group_by(session, task) %>%
  summarise(end = max(end, na.rm = TRUE))

# Merge end times with ztc_emotion_metrics
ztc_emotion_metrics <- ztc_emotion_metrics %>%
  left_join(end_times, by = c("session", "task"))

#Save the updated ztc_emotion_metrics
saveRDS(ztc_emotion_metrics, file = "data/ztc_emotion_metrics_end_times.rds")

# Convert charcters a, b, c, d, e, f to numbers 1, 2, 3, 4, 5, 6
ztc_emotion_metrics <- ztc_emotion_metrics %>%
  mutate(task = case_when(
    task == "a" ~ 1,
    task == "b" ~ 2,
    task == "c" ~ 3,
    task == "d" ~ 4,
    task == "e" ~ 5,
    task == "f" ~ 6
  ))


exog <- as.matrix(ztc_emotion_metrics %>% 
                    select(-c(session, end)) %>% 
                    mutate_at(c("recording"), as.numeric))
endog <- as.numeric((ztc_emotion_metrics %>% select(end))[[1]])

# Exog somehow gets converted to a matrix of characters
#so we need to convert it back to numeric
class(exog) <- "numeric"

# Split the data into training and test sets
set.seed(123)

train_indices <- sample(seq_len(nrow(exog)), 0.8 * nrow(exog))
train_exog <- exog[train_indices, ]
train_endog <- endog[train_indices]

test_exog <- exog[-train_indices, ]
test_endog <- endog[-train_indices]

# Scale training and test data
train_exog_scaled <- scale(train_exog)
test_exog_scaled <- scale(test_exog,
                          center = attr(train_exog_scaled, "scaled:center"),
                          scale = attr(train_exog_scaled, "scaled:scale"))

#Fit lasso model
lasso_model <- cv.glmnet(train_exog_scaled, train_endog, nfolds = 5)
plot(lasso_model) # Plot the cross-validated error as a function of lambda


# Get the coefficients for the largest lambda value which is
# within 1 standard error of the minimum
# This ensures that we get a simpler model
coef(lasso_model, s = "lambda.1se")
#We notice that the alignments seem to not play a role at all and the
#Dominance score is the least important of the emotion metrics

fit <- lasso_model$glmnet.fit
#Plot the coefficients as a function of lambda so that we can see their behavior
plot_glmnet(fit)

# Predict the end time using the test data
test_pred <- predict(lasso_model, newx = test_exog_scaled, s = "lambda.1se")

# Calculate error measures
mse <- mean((test_pred - test_endog)^2)
mae <- mean(abs(test_pred - test_endog))
rmse <- sqrt(mse)
r_squared <- 1 - sum((test_pred - test_endog)^2) / sum((test_endog - mean(test_endog))^2)

# Calculate error measures for dummy model
dummy_pred <- mean(train_endog)
dummy_mse <- mean((dummy_pred - test_endog)^2)
dummy_mae <- mean(abs(dummy_pred - test_endog))
dummy_rmse <- sqrt(dummy_mse)
dummy_r_squared <- 1 - sum((dummy_pred - test_endog)^2) / sum((test_endog - mean(test_endog))^2)

# Print the results with context
cat("Lasso Model Error Measures:\n")
cat("Mean Squared Error (MSE):", mse, "\n")
cat("Mean Absolute Error (MAE):", mae, "\n")
cat("Root Mean Squared Error (RMSE):", rmse, "\n")
cat("R-squared (R²):", r_squared, "\n\n")

cat("Dummy Model Error Measures:\n")
cat("Mean Squared Error (MSE):", dummy_mse, "\n")
cat("Mean Absolute Error (MAE):", dummy_mae, "\n")
cat("Root Mean Squared Error (RMSE):", dummy_rmse, "\n")
cat("R-squared (R²):", dummy_r_squared, "\n")