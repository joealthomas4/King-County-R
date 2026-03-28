🏠 House Price Prediction using Multiple Linear Regression

This project focuses on building a multiple linear regression model to predict housing prices using the King County House Sales Dataset (USA). It involves data cleaning, exploratory data analysis (EDA), feature engineering, model selection, and statistical validation.

📌 Project Overview

As highlighted in the introduction (page 1 ), this project analyzes how different housing features—such as size, quality, and location—affect property prices in the U.S. housing market.

The primary goal is to:

Build an accurate predictive model
Identify key factors influencing house prices
Interpret relationships between features and price
📊 Dataset
Dataset: King County House Sales Dataset
Source: Kaggle
Region: King County, Washington (USA)
Response Variable: price (sale price of house)
Data Cleaning Steps (page 2)
No missing values detected
Removed 14 duplicate records
Filtered unrealistic values (e.g., invalid sqft, bedrooms)
Created engineered features:
has_basement
sqft_basement_pos
zip_group (low/mid/high price areas)
🔍 Exploratory Data Analysis (EDA)
Key Insights (pages 3–5)
Strong correlation between price and sqft_living (~0.7)
Grade and bathrooms also show high correlation
Presence of multicollinearity among predictors
Price distribution is right-skewed → required log transformation
Evidence of:
Heteroscedasticity
Non-linearity
🔄 Model Transformation

To improve model performance:

Applied log transformation on price
Reduced skewness
Improved linearity and variance stability
🤖 Model Building
Base Model

Log-linear regression model:

log(price) ~ sqft_living + grade + bathrooms + bedrooms + sqft_basement_pos + has_basement + lat + waterfront + view + zip_group

📈 Model Selection Techniques
1. Forward Selection (page 9)
Adjusted R² ≈ 0.798
Most predictors statistically significant
2. Backward Elimination (page 10)
Similar performance to forward selection
All variables significant
3. Stepwise Selection (page 11)
Balanced model complexity and performance
✅ Final Choice:

Forward/Stepwise model selected due to:

Best performance
Simplicity
Lower AIC/BIC (page 13)
⚠️ Outlier Analysis (page 12)
All models detected the same 8 outliers
Forward model had the most stable residuals
Chosen as most robust model
🧪 Model Diagnostics
Performance Metrics (page 7)
Adjusted R² ≈ 0.798
RMSE ≈ 0.2366
Strong overall model fit
Assumptions Checked:
Linearity ✔️
Normality ✔️
Multicollinearity (VIF < 5) ✔️ (page 14)
Homoscedasticity improved after log transform
🔗 Interaction Model

To capture real-world relationships, interaction terms were added (page 15):

bathrooms × zip_group
bathrooms × view
has_basement × zip_group
Results (page 17–19)
Improved Adjusted R² ≈ 0.8002
Better predictive accuracy
Captured location-dependent effects
📊 Key Findings
sqft_living is the strongest predictor of price
grade significantly impacts house value
location (zip_group, latitude) plays a major role
waterfront & view add premium value
Interaction effects reveal:
Features behave differently across locations
Basement value depends on neighborhood
⚠️ Limitations

(page 19)

Cannot capture unobserved factors:
Crime rates
School quality
Less accurate for extremely high-priced homes
Correlation does not imply causation
🚀 Conclusion

The final interaction model provides:

High predictive accuracy
Strong interpretability
Real-world insights into housing price behavior

It demonstrates how combining statistical modeling with feature engineering and domain knowledge leads to meaningful results.

🧠 Concepts Used
Multiple Linear Regression
Log Transformation
Feature Engineering
Correlation Analysis
Model Selection (Forward, Backward, Stepwise)
Multicollinearity (VIF)
ANOVA Testing
Residual Diagnostics
Interaction Effects
👤 Author

Joeal Bijoy Thomas
