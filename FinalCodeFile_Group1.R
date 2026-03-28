### ============================================================
### STAT 611 PROJECT: KING COUNTY HOUSING
### Final Progress Report Script 
### ============================================================

# Set the seed for reproducibility of random operations
set.seed(42)
# Create a directory named 'figs' to save plots, suppressing warning if it already exists
dir.create("figs", showWarnings = FALSE)

# Load required R packages for data manipulation, visualization, and regression diagnostics

library(dplyr)
library(knitr)
library(broom)
library(ggplot2)
library(GGally)
library(DataExplorer) 
library(car)
library(lmtest)
library(MASS)
library(reshape2)
library(scales)
library(corrplot)
library(emmeans)
library(effects)



### 1. Load Data

# Print current working directory
getwd()
# Set working directory to the project folder
setwd("C:/Users/Asus/OneDrive/Desktop/MSDS/THIRD SEM/Regression Analysis/Project")
# Load the raw King County house data from CSV file
KingCounty_House <- read.csv("kc_house_data611.csv")
# Add a unique row identifier column for later use (e.g., tracking outliers)
KingCounty_House$row_id <- 1:nrow(KingCounty_House)

### 2. Select Variables
# Select a subset of variables for the regression analysis
final_data <- KingCounty_House %>%
  dplyr::select(price, sqft_living, grade, bathrooms, view,
                sqft_basement, bedrooms, lat, waterfront, zipcode, row_id) %>%
  mutate(
    # Convert zipcode and waterfront to factors (categorical variables)
    zipcode = factor(zipcode),
    waterfront = factor(waterfront)
  )

### 3. Feature Engineering (basement + zip_group + log_price)
# Create two basement-related features
final_data <- final_data %>%
  mutate(
    # Binary indicator: 1 if basement area > 0, 0 otherwise
    has_basement = ifelse(sqft_basement > 0, 1, 0),
    # Quantitative measure: basement area, but 0 if no basement
    sqft_basement_pos = ifelse(sqft_basement > 0, sqft_basement, 0)
  )

# Convert the numerical basement indicator to a descriptive factor
final_data$has_basement <- factor(final_data$has_basement,
                                  levels = c(0, 1),
                                  labels = c("No basement", "Has basement"))

# Calculate the median price for each zipcode
zip_group_tbl <- final_data %>%
  group_by(zipcode) %>%
  summarise(med_price = median(price), .groups = "drop") %>%
  # Divide zipcodes into 3 tiers (quantiles) based on median price
  mutate(zip_group_num = ntile(med_price, 3))

# Join the zipcode group number back to the main dataset
final_data <- final_data %>%
  left_join(zip_group_tbl[, c("zipcode", "zip_group_num")], by = "zipcode")

# Print column names to check the new variable
names(final_data)

# Print the head of the data frame to inspect the new variable
head(final_data)
# Convert the numerical zip group tier into a descriptive factor
final_data <- final_data %>%
  mutate(
    zip_group = factor(
      zip_group_num,
      levels = c(1, 2, 3),
      labels = c("Low-price area", "Mid-price area", "High-price area")
    )
  )
# Remove the temporary numerical zip group variable
final_data <- final_data %>%
  dplyr::select(-zip_group_num)
# Print column names again
names(final_data)
# Show the distribution of the newly created zip group factor
table(final_data$zip_group)

#Individual Plots
# Define a helper function for consistent formatting of the y-axis (Price in Millions USD)
price_axis_millions <- scale_y_continuous(
  labels = label_dollar(scale = 1e-6, suffix = "M"),
  name= "Price (Millions of USD)"
)


# 1. Price vs Sqft Living (Scatterplot)
ggplot(final_data, aes(x = sqft_living, y = price)) +
  geom_point(alpha = 0.3) +
  price_axis_millions +
  labs(title = "Price vs Living Area (sqft)",
       x = "Living Area (sqft)")

# 2. Price vs Grade (Scatterplot)
ggplot(final_data, aes(x = grade, y = price)) +
  geom_point(alpha = 0.3) +
  price_axis_millions +
  labs(title = "Price vs Grade",
       x = "Grade")

# 3. Price vs Bathrooms (Scatterplot)
ggplot(final_data, aes(x = bathrooms, y = price)) +
  geom_point(alpha = 0.3) +
  price_axis_millions +
  labs(title = "Price vs Bathrooms",
       x = "Bathrooms")

# 4. Price vs Bedrooms (Scatterplot)
ggplot(final_data, aes(x = bedrooms, y = price)) +
  geom_point(alpha = 0.3) +
  price_axis_millions +
  labs(title = "Price vs Bedrooms",
       x = "Bedrooms")

# 5. Price vs Basement Area (Scatterplot)
ggplot(final_data, aes(x = sqft_basement, y = price)) +
  geom_point(alpha = 0.3) +
  price_axis_millions +
  labs(title = "Price vs Basement Area",
       x = "Basement Area (sqft)")

# 6. Price vs Latitude (Scatterplot)
ggplot(final_data, aes(x = lat, y = price)) +
  geom_point(alpha = 0.3) +
  price_axis_millions +
  labs(title = "Price vs Latitude",
       x = "Latitude")


# Price vs Waterfront (Boxplot for binary categorical variable)
ggplot(final_data, aes(x = waterfront, y = price)) +
  geom_boxplot() +
  price_axis_millions +
  labs(title = "Price vs Waterfront",
       x = "Waterfront (0 = No, 1 = Yes)")

# Price vs View (Boxplot for ordinal categorical variable)
ggplot(final_data, aes(x = factor(view), y = price)) +
  geom_boxplot() +
  scale_y_continuous(labels = label_dollar(scale = 1e-6, suffix = "M")) +
  labs(title = "Price vs View Rating",
       x = "View Rating (0–4)",
       y = "Price (Millions)")

# Price vs Zip Group (Boxplot for created categorical variable)
ggplot(final_data, aes(x = zip_group, y = price)) +
  geom_boxplot() +
  scale_y_continuous(labels = label_dollar(scale = 1e-6, suffix = "M")) +
  labs(
    title = "Price vs Zipcode Price Tiers",
    x = "Zipcode Group",
    y = "Price (Millions)"
  )


### 4. Correlation Matrix & Scatterplot Matrix

# Select only numeric variables for matrix plotting
matrix_data <- final_data %>%
  dplyr::select(price, sqft_living, grade, bathrooms,
                view, sqft_basement, bedrooms, lat)

# Take a sample of 2000 rows for the scatterplot matrix for faster plotting
matrix_sample <- matrix_data %>% dplyr::slice_sample(n = 2000)

# Generate the scatterplot matrix using GGally::ggpairs
ggpairs(matrix_sample,
        title = "Scatterplot Matrix for Numeric variables",
        progress = FALSE)

#Correlation Matrix

# Select numeric variables again
num_vars <- final_data %>%
  dplyr::select(price, sqft_living, grade, bathrooms,
                view, sqft_basement, bedrooms, lat)

# Compute correlation matrix (rounded to 2 decimals for display)
cor_mat <- round(cor(num_vars, use = "pairwise.complete.obs"), 2)

# Reshape the correlation matrix into long format for ggplot heatmap
melted_cor <- melt(cor_mat)

# Heatmap visualization of the correlation matrix with text labels
ggplot(data = melted_cor, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +
  geom_text(aes(label = value), color = "black", size = 4) +
  scale_fill_gradient2(low = "steelblue", high = "firebrick",
                       mid = "white", midpoint = 0, limit = c(-1,1),
                       name = "Correlation Meter") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title.x = element_blank(),
        axis.title.y = element_blank()) +
  labs(title = "Correlation Heatmap")


#scatterplots for categorical (Faceted Plots)
# Scatterplot of Price vs Bathrooms, faceted by Waterfront status
ggplot(final_data, aes(x = bathrooms, y = price)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~ waterfront) +
  scale_y_continuous(labels = label_dollar(scale = 1e-6, suffix = "M")) +
  labs(title = "Price vs Bathrooms by Waterfront Status",
       x = "Bathrooms", y = "Price (Millions)")

# Scatterplot of Price vs Bathrooms, faceted by View Rating
ggplot(final_data, aes(x = bathrooms, y = price)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~ factor(view)) +
  scale_y_continuous(labels = label_dollar(scale = 1e-6, suffix = "M")) +
  labs(title = "Price vs Bathrooms by View Rating",
       x = "Bathrooms", y = "Price (Millions)")

# Scatterplot of Price vs Bathrooms, faceted by Zipcode Price Group
ggplot(final_data, aes(x = bathrooms, y = price)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~ zip_group) +
  scale_y_continuous(labels = label_dollar(scale = 1e-6, suffix = "M")) +
  labs(title = "Price vs Bathrooms by Zipcode Price Tiers",
       x = "Bathrooms", y = "Price (Millions)")

# Scatterplot of Price vs Sqft Living, faceted by Waterfront status
ggplot(final_data, aes(x = sqft_living, y = price)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~ waterfront) +
  scale_y_continuous(labels = label_dollar(scale = 1e-6, suffix = "M")) +
  labs(title = "Price vs Sqft Living by Waterfront Status",
       x = "Living Area (sqft)", y = "Price (Millions)")

# Scatterplot of Price vs Sqft Living, faceted by View Rating
ggplot(final_data, aes(x = sqft_living, y = price)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~ factor(view)) +
  scale_y_continuous(labels = label_dollar(scale = 1e-6, suffix = "M")) +
  labs(title = "Price vs Sqft Living by View Rating",
       x = "Living Area (sqft)", y = "Price (Millions)")

# Scatterplot of Price vs Sqft Living, faceted by Zipcode Price Group
ggplot(final_data, aes(x = sqft_living, y = price)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~ zip_group) +
  scale_y_continuous(labels = label_dollar(scale = 1e-6, suffix = "M")) +
  labs(title = "Price vs Sqft Living by Zipcode Price Tiers",
       x = "Living Area (sqft)", y = "Price (Millions)")

# Scatterplot of Price vs Grade, faceted by Waterfront status
ggplot(final_data, aes(x = grade, y = price)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~ waterfront) +
  price_axis_millions +
  labs(title = "Price vs Grade by Waterfront",
       x = "Grade")

# Scatterplot of Price vs Grade, faceted by Zipcode Price Group
ggplot(final_data, aes(x = grade, y = price)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~ zip_group) +
  price_axis_millions +
  labs(title = "Price vs Grade by Zipcode Price Tiers",
       x = "Grade")

# Scatterplot of LOG Price vs Grade, faceted by Zipcode Price Group
ggplot(final_data, aes(x = grade, y = log(price))) +
  geom_point(alpha = 0.3) +
  facet_wrap(~ zip_group) +
  price_axis_millions +
  labs(title = "Log Price vs Grade by Zipcode Price Tiers",
       x = "Grade")

# Scatterplot of Price vs Bedrooms, faceted by Waterfront status
ggplot(final_data, aes(x = bedrooms, y = price)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~ waterfront) +
  price_axis_millions +
  labs(title = "Price vs Bedrooms by Waterfront",
       x = "Bedrooms")
# Scatterplot of Price vs Bedrooms, faceted by Zipcode Price Group
ggplot(final_data, aes(x = bedrooms, y = price)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~ zip_group) +
  price_axis_millions +
  labs(title = "Price vs Bedrooms by Zipcode Price Tiers",
       x = "Bedrooms")

# Scatterplot of Price vs Latitude, faceted by Waterfront status
ggplot(final_data, aes(x = lat, y = price)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~ waterfront) +
  price_axis_millions +
  labs(title = "Price vs Latitude by Waterfront",
       x = "Latitude")

# Scatterplot of Price vs Latitude, faceted by Zipcode Price Group
ggplot(final_data, aes(x = lat, y = price)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~ zip_group) +
  price_axis_millions +
  labs(title = "Price vs Latitude by Zipcode Price Tiers",
       x = "Latitude")

# Scatterplot of Price vs View, faceted by Waterfront status
ggplot(final_data, aes(x = view, y = price)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~ waterfront) +
  price_axis_millions +
  labs(title = "Price vs View Rating by Waterfront",
       x = "View Rating (0–4)")
# Scatterplot of Price vs View, faceted by Zipcode Price Group
ggplot(final_data, aes(x = view, y = price)) +
  geom_point(alpha = 0.3) +
  facet_wrap(~ zip_group) +
  price_axis_millions +
  labs(title = "Price vs View Rating by Zipcode Price Tiers",
       x = "View Rating (0–4)")


# numeric variables only


####MOdel
# Ensure all categorical variables are properly set as factors, and log_price is calculated
final_data <- final_data %>%
  mutate(
    log_price = log(price), # Calculate log-transformed price
    waterfront = factor(waterfront),
    view= factor(view),
    zip_group = factor(zip_group),
    # Re-confirm 'has_basement' is a factor with specified levels/order
    has_basement = factor(has_basement,
                          levels = c("No basement", "Has basement"))
  )

#MODEL-1 (Raw Price Model)
# Fit the initial Linear Regression model using the raw 'price' as the dependent variable
model_raw <- lm(
  price ~ sqft_living + grade + bathrooms + bedrooms +
    sqft_basement_pos + has_basement +
    lat + waterfront + view + zip_group,
  data = final_data
)
# Display the summary of the raw model
summary(model_raw)


# Inverse Fitted Value Plot for RAW model (checks for nonlinearity/transformation needs)
invResPlot(model_raw,
           main = "Inverse Fitted Value Plot (Raw Price Model)")
# Standard diagnostic plots for the raw model:
plot(model_raw, which = 1) # Residuals vs Fitted
plot(model_raw, which = 2) # Normal Q-Q
plot(model_raw, which = 3) # Scale-Location
plot(model_raw, which = 5) # Residuals vs Leverage
plot(model_raw, which = 4) # Cook's Distance


# Breusch–Pagan test for heteroskedasticity (null: homoskedasticity)
bptest(model_raw)

# Non-constant Variance test (ncvTest) for heteroskedasticity
ncvTest(model_raw)

# Component-plus-residual plots (CR plots) to check functional form of predictors
crPlots(model_raw)

# Calculate Cook's distances to identify influential observations
cooks_raw <- cooks.distance(model_raw)
# Count the number of observations where Cook's distance exceeds the 4/n cutoff
sum(cooks_raw > 4/nrow(final_data))
# Calculate Variance Inflation Factors (VIF) to check for multicollinearity
vif(model_raw)


# Add raw model residuals to the dataset
final_data$resid_raw <- resid(model_raw)

# Plot residuals against observation order (checks for non-independence/time effects)
ggplot(final_data, aes(x = row_id, y = resid_raw)) +
  geom_point(alpha = 0.4) +
  geom_smooth(se = FALSE, color = "red") +
  labs(title = "Residuals vs Observation Order (Raw Model)",
       x = "Observation Order", y = "Residuals")

#BOXCOX
# Perform Box-Cox transformation to find the optimal power transformation for 'price'
bc <- boxcox(model_raw)
# Extract the estimated optimal lambda
lambda <- bc$x[which.max(bc$y)]
lambda

#LOG(Price) Model
# Fit the Linear Regression model using log-transformed 'price'
model_log <- lm(log(price) ~ sqft_living + grade + bathrooms + bedrooms +
                  sqft_basement_pos + has_basement + lat +
                  waterfront + view + zip_group,
                data = final_data)
# Display the summary of the log-transformed model
summary(model_log)

# Inverse Fitted Value Plot (checks for the need for further transformation)
invResPlot(model_log,
           main = "Inverse Fitted Value Plot (Log-Transformed Model)")

# Standard diagnostic plots for the log-transformed model:
# 1. Residuals vs Fitted
plot(model_log, which = 1, caption = "")
title(main = "Residuals vs Fitted (Log-Transformed Model)")

# 2. Normal Q-Q Plot
plot(model_log, which = 2, caption = "")
title(main = "Q-Q Plot (Log-Transformed Model)")

# 3. Scale-Location Plot (Square root of standardized residuals vs fitted values)
plot(model_log, which = 3, caption = "")
title(main = "Scale-Location (Log-Transformed Model)")

# 4. Cook's Distance
plot(model_log, which = 4, caption = "")
title(main = "Cook's Distance (Log-Transformed Model)")

# 5. Residuals vs Leverage
plot(model_log, which = 5, caption = "")
title(main = "Residuals vs Leverage (Log-Transformed Model)")

# Diagnostic tests for the log-transformed model:
bptest(model_log) # Breusch–Pagan test
ncvTest(model_log) # Non-constant Variance test
crPlots(model_log) # Component-plus-residual plots
cooks_log <- cooks.distance(model_log) # Calculate Cook's distances
# Count influential points
sum(cooks_log > 4/nrow(final_data))
vif(model_log) # Calculate VIFs
final_data$resid_log <- resid(model_log) # Add log model residuals to dataset

# Plot residuals vs observation order for log model
ggplot(final_data, aes(x = row_id, y = resid_log)) +
  geom_point(alpha = 0.4) +
  geom_smooth(se = FALSE, color = "red") +
  labs(title = "Residuals vs Observation Order (Log Model)",
       x = "Observation Order", y = "Residuals")

#LOG-RULE
# Variables for which log transformation is considered beneficial (if range ratio > 10)
log_rule_vars <- c("price", "sqft_living", "sqft_basement_pos")

# Apply a function to calculate the ratio of max/min positive value for each variable
log_rule_summary <- lapply(log_rule_vars, function(v) {
  x <- final_data[[v]]
  x_pos <- x[x > 0]# remove zeros for ratio calculation
  ratio <- max(x_pos) / min(x_pos)
  data.frame(
    variable = v,
    min_positive = min(x_pos),
    max_value = max(x_pos),
    range_ratio = ratio
  )
})

# Combine the results into a single data frame
log_rule_summary <- do.call(rbind, log_rule_summary)
log_rule_summary

# Histograms to visually compare raw vs log-transformed distributions

# PRICE: raw vs log
ggplot(final_data, aes(x = price)) +
  geom_histogram(bins = 40, color = "black", fill = "grey70") +
  labs(title = "Histogram of Price (Raw Scale)", x = "Price", y = "Count")

ggplot(final_data, aes(x = log(price))) +
  geom_histogram(bins = 40, color = "black", fill = "grey70") +
  labs(title = "Histogram of log(Price)", x = "log(Price)", y = "Count")

# SQFT_LIVING: raw vs log (just for checking)
ggplot(final_data, aes(x = sqft_living)) +
  geom_histogram(bins = 40, color = "black", fill = "grey70") +
  labs(title = "Histogram of Living Area (sqft)", x = "sqft_living", y = "Count")

ggplot(final_data, aes(x = log(sqft_living))) +
  geom_histogram(bins = 40, color = "black", fill = "grey70") +
  labs(title = "Histogram of log(Living Area)", x = "log(sqft_living)", y = "Count")

# SQFT_BASEMENT_POS: raw vs log (only if you want to inspect)
ggplot(final_data, aes(x = sqft_basement_pos)) +
  geom_histogram(bins = 40, color = "black", fill = "grey70") +
  labs(title = "Histogram of Basement Area (sqft)", x = "sqft_basement_pos", y = "Count")

# Adding 1 for log transformation to handle zero values
ggplot(final_data, aes(x = log(sqft_basement_pos + 1))) +# +1 in case of zeros
  geom_histogram(bins = 40, color = "black", fill = "grey70") +
  labs(title = "Histogram of log(Basement Area + 1)", x = "log(sqft_basement_pos + 1)", y = "Count")



# OUTLIER REMOVAL based on Cook's Distance from the log model
cook_vals <- cooks.distance(model_log)
# Calculate the Cook's distance cutoff (4/n)
cutoff_cook <- 4 / nrow(final_data)
# Identify the row indices of influential points
high_cook_idx <- which(cook_vals > cutoff_cook)
# Print the count of influential points
length(high_cook_idx)


# Create a new dataset excluding the influential points
final_clean <- final_data[-high_cook_idx, ]

# Fit the final Linear Regression model on the cleaned, log-transformed data
model_clean <- lm(log_price ~ sqft_living + grade + bathrooms + view +
                    has_basement + sqft_basement_pos + bedrooms +
                    lat + waterfront + zip_group,
                  data = final_clean)

# Display the summary of the cleaned log model
summary(model_clean)
null_model <- lm(log_price ~ 1, data = final_clean)

##########################
#Section-C
########################
forward_model <- stepAIC(
  null_model,
  scope = list(lower = null_model, upper = model_clean),
  direction = "forward",
  trace = FALSE
)

summary(forward_model)

n <- nrow(final_clean)

backward_model <- step(
  model_clean,
  direction = "backward",
  k = log(n),     # BIC penalty
  trace = FALSE
)

summary(backward_model)
list(
  Forward = formula(forward_model),
  Backward = formula(backward_model),
  Clean_Log = formula(model_clean)
)


library(dplyr)
library(knitr)

# RMSE function
rmse <- function(model) sqrt(mean(resid(model)^2))


model_linear <- model_log
model_quad <- lm(log(price) ~ sqft_living + I(sqft_living^2) +
                   grade + bathrooms + view +
                   has_basement + sqft_basement_pos + bedrooms +
                   lat + waterfront + zip_group,
                 data = final_data)

stepwise_model <- stepAIC(
  model_clean,
  direction = "both",
  trace = FALSE
)

summary(stepwise_model)
n <- nrow(final_clean)

stepwise_BIC <- step(
  model_clean,
  direction = "both",
  k = log(n),
  trace = FALSE
)

summary(stepwise_BIC)


model_compare <- bind_rows(
  Forward     = get_metrics(forward_model),
  Backward    = get_metrics(backward_model),
  Stepwise    = get_metrics(stepwise_model),
  Clean_Log   = get_metrics(model_clean),
  .id = "Model"
)

kable(model_compare,
      caption = "Comparison of Forward, Backward, Stepwise, and Clean_Log Models")


# Formal tests for heteroskedasticity and functional form
bptest(forward_model)    # Breusch–Pagan test
ncvTest(forward_model)   # Non-constant variance test
crPlots(forward_model) 
vif(forward_model)
###=======================================
### DIAGNOSTICS FOR forward MODEL
###=======================================

# 1. Inverse Fitted Value Plot
invResPlot(forward_model,
           main = "Inverse Fitted Value Plot (Forward Model)")


# 2. Residuals vs Fitted
plot(forward_model, which = 1, caption = "")
title(main = "Residuals vs Fitted (Forward Model)")


# 3. Normal Q-Q Plot
plot(forward_model, which = 2, caption = "")
title(main = "Q-Q Plot (Forward Model)")


# 4. Scale-Location
plot(model_clean, which = 3, caption = "")
title(main = "Scale-Location (Forward Model)")


# 5. Cook's Distance
plot(forward_model, which = 4, caption = "")
title(main = "Cook's Distance (Forward Model)")


# 6. Residuals vs Leverage
plot(forward_model, which = 5, caption = "")
title(main = "Residuals vs Leverage (Forward Model)")


# 7. Residuals vs Observation Order
final_clean$resid_clean <- resid(forward_model)

ggplot(final_clean, aes(x = row_id, y = resid_clean)) +
  geom_point(alpha = 0.4) +
  geom_smooth(se = FALSE, color = "red") +
  labs(title = "Residuals vs Observation Order (Forward Model)",
       x = "Observation Order", y = "Residuals")


vif(forward_model)

###=======================================
### Section - D
###=======================================
summary(forward_model)
anova(forward_model)


coef_df <- summary(forward_model)$coefficients
coef_df <- as.data.frame(coef_df)
coef_df$Percent_Change <- (exp(coef_df$Estimate) - 1) * 100

coef_df

summary(forward_model)$adj.r.squared
# view has 5 levels
emm_view <- emmeans(forward_model, ~ view)
pairs(emm_view)

# has_basement has 2 levels
emm_basement <- emmeans(forward_model, ~ has_basement)
pairs(emm_basement)

# zip_group has 3 levels
emm_zip <- emmeans(forward_model, ~ zip_group)
pairs(emm_zip)

# waterfront has 2 levels
emm_water <- emmeans(forward_model, ~ waterfront)
pairs(emm_water)
#################################################
coef_df <- as.data.frame(summary(forward_model)$coefficients)
coef_df$Term <- rownames(coef_df)
rownames(coef_df) <- NULL

# Rename columns to safe names
names(coef_df) <- c("Estimate", "Std_Error", "t_value", "P_value", "Term")

# Rearrange
coef_df <- coef_df[, c("Term", "Estimate", "Std_Error", "t_value", "P_value")]

# Add percent change
coef_df$Percent_Change <- (exp(coef_df$Estimate) - 1) * 100

# Print table
knitr::kable(coef_df, digits = 4,
             caption = "Parameter Estimates and Percent Change in Price (Original Scale)")

#################################################

### 6. Effect plots


plot(allEffects(forward_model), multiline = TRUE, ci.style = "bands")

#Model Comparison: transformed vs Clean
# Extract Adjusted R-squared for comparison
adjR2_trans<- summary(model_log)$adj.r.squared
adjR2_clean <- summary(model_clean)$adj.r.squared

# Calculate Root Mean Squared Error (RMSE) for comparison
rmse_trans<- sqrt(mean(resid(model_log)^2))
rmse_clean <- sqrt(mean(resid(model_clean)^2))

# Extract Akaike Information Criterion (AIC) for comparison
AIC_trans<- AIC(model_log)
AIC_clean <- AIC(model_clean)

# Create a data frame to display the comparison metrics
comparison <- data.frame(
  Metric = c("Adjusted R2", "RMSE", "AIC"),
  Raw_Model= c(adjR2_trans, rmse_trans, AIC_trans),
  Clean_Model = c(adjR2_clean, rmse_clean, AIC_clean)
)
print(comparison)

### 16.Interaction Modes
# Fit model with interaction between bathrooms and waterfront
model_int1 <- lm(log_price ~ sqft_living + grade +
                   bathrooms*waterfront+view + has_basement + sqft_basement_pos +
                   bedrooms + lat + zip_group,
                 data = final_clean)
# Fit model with interaction between waterfront and view
model_int2 <- lm(log_price ~ sqft_living + grade +
                   bathrooms + waterfront*view + has_basement + sqft_basement_pos +
                   bedrooms + lat + zip_group,
                 data = final_clean)
# Summarize interaction models
summary(model_int1)
summary(model_int2)
# Use ANOVA to formally test if interaction 1 is significantly better than the clean model
anova(model_clean, model_int1)
# Use ANOVA to formally test if interaction 2 is significantly better than the clean model
anova(model_clean,model_int2)




### -------------------------------
### Nested model comparison (ANOVA)
### -------------------------------
anova(model_raw, model_log)      # raw price vs log(price)
anova(model_log, model_quad)





# Define a function to calculate Root Mean Squared Error (RMSE)
rmse <- function(model) {
  sqrt(mean(resid(model)^2))
}
# Collect all fitted models in a named list
models <- list(
  Raw_Price= model_raw,
  Log_AllData= model_log,
  Log_Clean= model_clean,
  Log_Clean_IntBath = model_int1,
  Log_Clean_IntWV= model_int2
)
# Calculate summary metrics (Adj R2, RMSE, AIC, BIC) for all models
model_comp <- lapply(models, function(m) {
  c(
    Adj_R2 = summary(m)$adj.r.squared,
    RMSE= rmse(m),
    AIC= AIC(m),
    BIC = BIC(m)
  )
})
# Combine the list of metrics into a single data frame
model_comp <- do.call(rbind, model_comp)
model_comp <- as.data.frame(model_comp)
# Move row names (model names) into a column
model_comp$Model <- rownames(model_comp)
rownames(model_comp) <- NULL
# Reorder columns for presentation
model_comp <- model_comp[, c("Model", "Adj_R2", "RMSE", "AIC", "BIC")]
# Print the final model comparison table
print(model_comp)
  
summary(model_clean)




coef_tab <- tidy(model_clean) %>%
  filter(term != "(Intercept)") %>%
  mutate(
    pct_change = round((exp(estimate) - 1) * 100, 2),
    estimate   = round(estimate, 4),
    std.error  = round(std.error, 4),
    statistic  = round(statistic, 2),
    p.value    = signif(p.value, 3)
  ) %>%
  rename(
    Term = term,
    Estimate = estimate,
    Std_Error = std.error,
    t_value = statistic,
    P_value = p.value,
    Percent_Change = pct_change
  ) %>%
  dplyr::select(Term, Estimate, Std_Error, t_value, P_value, Percent_Change)

kable(
  coef_tab,
  caption = "Parameter Estimates and Percent Change in Price (Original Scale)"
)


########################
# SECTION - E 
# Start from your final_clean data and forward_model

full_int_model <- lm(
  log_price ~ sqft_living + grade + bathrooms + sqft_basement_pos +
    has_basement + lat + waterfront + view + zip_group +
    bathrooms:waterfront + bathrooms:view + bathrooms:zip_group +
    bathrooms:has_basement +
    waterfront:view + waterfront:zip_group +
    view:zip_group +
    has_basement:view + has_basement:zip_group,
  data = final_clean
)
summary(full_int_model)
n <- nrow(final_clean)

final_int_model <- step(
  full_int_model,
  direction = "backward",
  k = log(n)   # BIC penalty
)
formula(final_int_model)
summary(full_int_model)

# VIF / GVIF for final interaction model
vif_int <- vif(final_int_model)
vif_int

# If there are categorical predictors, vif_int will be a matrix with GVIF & Df.
# This creates a tidy table with the adjusted GVIF = GVIF^(1/(2*Df))

if (is.matrix(vif_int)) {
  vif_table <- data.frame(
    Term     = rownames(vif_int),
    GVIF     = vif_int[, "GVIF"],
    Df       = vif_int[, "Df"],
    GVIF_adj = vif_int[, "GVIF"]^(1 / (2 * vif_int[, "Df"]))
  )
  rownames(vif_table) <- NULL
  vif_table
} else {
  # If only numeric predictors (unlikely here), this is enough:
  vif_table <- data.frame(
    Term = names(vif_int),
    VIF  = as.numeric(vif_int)
  )
  vif_table
}

# Standard diagnostic plots: residuals vs fitted, QQ, scale-location, leverage
par(mfrow = c(2, 2))
plot(final_int_model)
par(mfrow = c(1, 1))
invResPlot(final_int_model,
           main = "Inverse Fitted Value Plot (Final Interaction Model)")

library(lmtest)

bptest(final_int_model)   # Breusch–Pagan test
ncvTest(final_int_model)  # Non-constant variance test
cooks_int <- cooks.distance(final_int_model)

# How many points exceed 4/n?
sum(cooks_int > 4 / nrow(final_clean))

# Optional: quick Cook's distance plot
plot(final_int_model, which = 4)  # Cook's distance plot


final_clean$resid_int <- resid(final_int_model)

ggplot(final_clean, aes(x = row_id, y = resid_int)) +
  geom_point(alpha = 0.4) +
  geom_smooth(se = FALSE, color = "red") +
  labs(
    title = "Residuals vs Observation Order (Final Interaction Model)",
    x = "Observation Order",
    y = "Residuals"
  )



###############
#SECTION-F
##############
anova(final_int_model)
summary(final_int_model)


pairs(emmeans(final_int_model, ~ view))
pairs(emmeans(final_int_model, ~ waterfront))
pairs(emmeans(final_int_model, ~ has_basement | zip_group))
pairs(emmeans(final_int_model, ~ bathrooms | zip_group))
pairs(emmeans(final_int_model, ~ zip_group))
plot(allEffects(final_int_model))

coef_tab_int <- broom::tidy(final_int_model) %>%
  dplyr::filter(term != "(Intercept)") %>%
  dplyr::mutate(
    Percent_Change = round((exp(estimate) - 1) * 100, 2),
    Estimate       = round(estimate, 4),
    Std_Error      = round(std.error, 4),
    t_value        = round(statistic, 2),
    P_value        = format.pval(p.value, digits = 3),
    Term           = gsub(":", " × ", term)
  ) %>%
  dplyr::select(Term, Estimate, Std_Error, t_value, P_value, Percent_Change)

knitr::kable(coef_tab_int,
             caption = "Parameter Estimates and Percent Change in Price (BIC)"
)
anova(forward_model,final_int_model)
####################
# SECTION-G
rmse <- function(model) {
  sqrt(mean(residuals(model)^2))
}

# Forward Selection Model (Part C)
rmse_forward <- rmse(forward_model)
aic_forward  <- AIC(forward_model)
bic_forward  <- BIC(forward_model)

# Final Interaction Model (Part E)
rmse_int <- rmse(final_int_model)
aic_int  <- AIC(final_int_model)
bic_int  <- BIC(final_int_model)

# Create comparison table
model_metrics <- data.frame(
  Model = c("Forward Model", "Final Interaction Model"),
  RMSE  = c(rmse_forward, rmse_int),
  AIC   = c(aic_forward, aic_int),
  BIC   = c(bic_forward, bic_int)
)

model_metrics
######################
#SECTION - F
################
# 1. Identify influential points based on Cook’s Distance
cook_vals <- cooks.distance(final_int_model)
cutoff <- 4 / nrow(final_clean)
influential_ids <- which(cook_vals > cutoff)

influential_ids  # to see the indices
length(influential_ids)

# 2. Prediction intervals for these influential points
pred_inf <- predict(
  final_int_model,
  newdata = final_clean[influential_ids, ],
  interval = "prediction"
)

pred_inf

# 3. Fit model WITHOUT influential points
clean_no_inf <- final_clean[-influential_ids, ]

final_int_noinf_model <- lm(
  formula(final_int_model),
  data = clean_no_inf
)

# 4. Compare key metrics
compare_metrics <- data.frame(
  Model = c("Final Interaction Model (All Data)", "Interaction Model (No Influential Points)"),
  RMSE = c(
    sqrt(mean(final_int_model$residuals^2)),
    sqrt(mean(final_int_noinf_model$residuals^2))
  ),
  AIC  = c(AIC(final_int_model), AIC(final_int_noinf_model)),
  BIC  = c(BIC(final_int_model), BIC(final_int_noinf_model))
)

compare_metrics


