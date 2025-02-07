
# Load necessary libraries
library(dplyr)
library(gamlr)
library(openxlsx)


# Read the dataset
data <- read.csv("C:/Users/aksab/Downloads/cleaned_weekly_2017_2019.csv")

# Compute weekly returns for GSPC and RUA
data <- data %>%
  mutate(
    GSPC_Returns = ((Adj.Close_.GSPC - lag(Adj.Close_.GSPC)) / lag(Adj.Close_.GSPC)) * 100,
    RUA_Returns = ((Adj.Close_.RUA - lag(Adj.Close_.RUA)) / lag(Adj.Close_.RUA)) * 100
  )

# Compute the risk-free rate (convert annualized IRX to a weekly rate)
data <- data %>%
  mutate(
    Weekly_Risk_Free_Rate = ((1 + Adj.Close_.IRX / 100)^(1/52) - 1) * 100, # used compounding interest formula for making it more realistic
  )

# Compute risk-adjusted returns for GSPC and RUA
data <- data %>%
  mutate(
    GSPC_Risk_Adjusted = GSPC_Returns - Weekly_Risk_Free_Rate,
    RUA_Risk_Adjusted = RUA_Returns - Weekly_Risk_Free_Rate
  )

# Create lagged variables for returns and music sentiment
data <- data %>%
  mutate(
    Lagged_GSPC_Returns = lag(GSPC_Returns),
    Lagged_RUA_Returns = lag(RUA_Returns),
    Lagged_Music_Sentiment = lag(Music_Sentiment),
    Lagged_GSPC_RA = lag(GSPC_Risk_Adjusted),
    Lagged_RUA_RA = lag(RUA_Risk_Adjusted)
  ) %>% na.omit()

dim(data)

# Write the data frame to an Excel file
write.xlsx(data, file = "C:/Users/aksab/Downloads/Transformeddata.xlsx",sheetName="Data", rowNames = FALSE)

# Run regression models for Panel A: Contemporaneous Effects
# Model 1: S&P 500 Returns

model_gspc <- glm(GSPC_Returns ~ Music_Sentiment + Lagged_GSPC_Returns, data = data)
summary(model_gspc)

# Model 2: Russell 3000 Returns
model_rua <- glm(RUA_Returns ~ Music_Sentiment + Lagged_RUA_Returns, data = data)
summary(model_rua)

# Model 3: S&P 500 Risk-Adjusted Returns
model_gspc_ra <- glm(GSPC_Risk_Adjusted ~ Music_Sentiment + Lagged_GSPC_RA, data = data)
summary(model_gspc_ra)

# Model 4: Russell 3000 Risk-Adjusted Returns
model_rua_ra <- glm(RUA_Risk_Adjusted ~ Music_Sentiment + Lagged_RUA_RA, data = data)
summary(model_rua_ra)

# Run regression models for Panel B: Lagged Effects
# Model 5: S&P 500 Returns with Lagged Music Sentiment
model_lag_gspc <- glm(GSPC_Returns ~ Lagged_Music_Sentiment + Lagged_GSPC_Returns, data = data)
summary(model_lag_gspc)

# Model 6: Russell 3000 Returns with Lagged Music Sentiment
model_lag_rua <- glm(RUA_Returns ~ Lagged_Music_Sentiment + Lagged_RUA_Returns, data = data)
summary(model_lag_rua)

# Model 7: S&P 500 Risk-Adjusted Returns with Lagged Music Sentiment
model_lag_gspc_ra <- glm(GSPC_Risk_Adjusted ~ Lagged_Music_Sentiment + Lagged_GSPC_RA, data = data)
summary(model_lag_gspc_ra)

# Model 8: Russell 3000 Risk-Adjusted Returns with Lagged Music Sentiment
model_lag_rua_ra <- glm(RUA_Risk_Adjusted ~ Lagged_Music_Sentiment + Lagged_RUA_RA, data = data)
summary(model_lag_rua_ra)

# Extract standard errors and store them in a list
standard_errors_list <- list(
  model_gspc_se = summary(model_gspc)$coefficients[, "Std. Error"],
  model_rua_se = summary(model_rua)$coefficients[, "Std. Error"],
  model_gspc_ra_se = summary(model_gspc_ra)$coefficients[, "Std. Error"],
  model_rua_ra_se = summary(model_rua_ra)$coefficients[, "Std. Error"],
  model_lag_gspc_se = summary(model_lag_gspc)$coefficients[, "Std. Error"],
  model_lag_rua_se = summary(model_lag_rua)$coefficients[, "Std. Error"],
  model_lag_gspc_ra_se = summary(model_lag_gspc_ra)$coefficients[, "Std. Error"],
  model_lag_rua_ra_se = summary(model_lag_rua_ra)$coefficients[, "Std. Error"]
)

# Print the list of standard errors
print(standard_errors_list)







#Bootstrapping 
library(boot)
library(sandwich)
library(lmtest)
library(parallel)

detectCores()

# Define the model formulas and coefficient names in a list
model_formulas <- list(
  GSPC_Contemp = glm(GSPC_Returns ~ Music_Sentiment + Lagged_GSPC_Returns, data = data),
  RUA_Contemp = glm(RUA_Returns ~ Music_Sentiment + Lagged_RUA_Returns, data = data),
  GSPC_RA_Contemp = glm(GSPC_Risk_Adjusted ~ Music_Sentiment + Lagged_GSPC_RA, data = data),
  RUA_RA_Contemp = glm(RUA_Risk_Adjusted ~ Music_Sentiment + Lagged_RUA_RA, data = data),
  GSPC_Lagged = glm(GSPC_Returns ~ Lagged_Music_Sentiment + Lagged_GSPC_Returns, data = data),
  RUA_Lagged = glm(RUA_Returns ~ Lagged_Music_Sentiment + Lagged_RUA_Returns, data = data),
  GSPC_RA_Lagged = glm(GSPC_Risk_Adjusted ~ Lagged_Music_Sentiment + Lagged_GSPC_RA, data = data),
  RUA_RA_Lagged = glm(RUA_Risk_Adjusted ~ Lagged_Music_Sentiment + Lagged_RUA_RA, data = data)
)

# Function to extract the coefficient
getBeta <- function(data, indices, formula, coef_name){
  model <- glm(formula = formula, data = data[indices, ])
  return(model$coef[coef_name])
}

# Empty list to store bootstrapping results
bootstrap_results <- list()

# Loop through each model and perform bootstrapping
for (name in names(model_formulas)) {
  formula <- model_formulas[[name]]
  coef_name <- if (grepl("Lagged", name)) "Lagged_Music_Sentiment" else "Music_Sentiment"
  
  # Perform the bootstrapping
  bootstrap_results[[name]] <- boot(data, getBeta, R = 2000, formula = formula, coef_name = coef_name, 
                                    parallel = "snow", ncpus = detectCores())
}

# Print the results
print(bootstrap_results)











# HC0

models <- list(
  GSPC_Contemp = model_gspc,
  RUA_Contemp = model_rua,
  GSPC_RA_Contemp = model_gspc_ra,
  RUA_RA_Contemp = model_rua_ra,
  GSPC_Lagged = model_lag_gspc,
  RUA_Lagged = model_lag_rua,
  GSPC_RA_Lagged = model_lag_gspc_ra,
  RUA_RA_Lagged = model_lag_rua_ra
)

# Empty list 
hc_results <- list()

# Loop through each model and compute HC0 standard errors
for (name in names(models)) {
  model <- models[[name]]
  
  # Compute heteroskedasticity-consistent covariance matrix
  VHC <- vcovHC(model, type = "HC0")
  
  # Test coefficients with robust standard errors
  hcstats <- coeftest(model, vcov = VHC)
  
  # Extracting the results for 'Music_Sentiment' or 'Lagged_Music_Sentiment'
  coef_name <- if (grepl("Lagged", name)) "Lagged_Music_Sentiment" else "Music_Sentiment"
  
  # Store rounded results in the list
  hc_results[[name]] <- round(hcstats[coef_name,], 5)
}

# Display results
print(hc_results)









# Lasso


library(gamlr)
library(Matrix)

#1. Same week returns

# GSPC

x_GSPC = model.matrix(GSPC_Returns ~ Music_Sentiment + Lagged_GSPC_Returns, data = data )[,-1]
y_GSPC = data$GSPC_Returns


cv_fit_GSPC <- cv.gamlr(x=x_GSPC, y=y_GSPC,lmr=1e-4, standardize =T)
plot(cv_fit_GSPC)

(lambda_min_GSPC <- cv_fit_GSPC$lambda.min)
(lasso_coef_GSPC <- coef(cv_fit_GSPC, s = "min"))
(lasso_coef_GSPC_1 <- coef(cv_fit_GSPC, s = "1se"))



#RUA
x_RUA = model.matrix(RUA_Returns ~ Music_Sentiment + Lagged_RUA_Returns, data = data)[, -1]
y_RUA = data$RUA_Returns

cv_fit_RUA <- cv.gamlr(x=x_RUA, y=y_RUA, lmr=1e-4, standardize=TRUE)
plot(cv_fit_RUA)

(lambda_min_RUA <- cv_fit_RUA$lambda.min)
(lasso_coef_RUA <- coef(cv_fit_RUA, s = "min"))
(lasso_coef_RUA_1 <- coef(cv_fit_RUA, s = "1se"))



# GSPC Risk adjusted

x_GSPC_RA = model.matrix(GSPC_Risk_Adjusted ~ Music_Sentiment + Lagged_GSPC_RA, data = data)[, -1]
y_GSPC_RA = data$GSPC_Risk_Adjusted

cv_fit_GSPC_RA <- cv.gamlr(x=x_GSPC_RA, y=y_GSPC_RA, lmr=1e-4, standardize=TRUE)
plot(cv_fit_GSPC_RA)

(lambda_min_GSPC_RA <- cv_fit_GSPC_RA$lambda.min)
(lasso_coef_GSPC_RA <- coef(cv_fit_GSPC_RA, s = "min"))
(lasso_coef_GSPC_RA_1 <- coef(cv_fit_GSPC_RA, s = "1se"))


# RUA Risk adjusted

x_RUA_RA = model.matrix(RUA_Risk_Adjusted ~ Music_Sentiment + Lagged_RUA_RA, data = data)[, -1]
y_RUA_RA = data$RUA_Risk_Adjusted

cv_fit_RUA_RA <- cv.gamlr(x=x_RUA_RA, y=y_RUA_RA, lmr=1e-4, standardize=TRUE)
plot(cv_fit_RUA_RA)

(lambda_min_RUA_RA <- cv_fit_RUA_RA$lambda.min)
(lasso_coef_RUA_RA <- coef(cv_fit_RUA_RA, s = "min"))

# GSPC lagged

x_lag_GSPC = model.matrix(GSPC_Returns ~ Lagged_Music_Sentiment + Lagged_GSPC_Returns, data = data)[, -1]
y_lag_GSPC = data$GSPC_Returns

cv_fit_lag_GSPC <- cv.gamlr(x=x_lag_GSPC, y=y_lag_GSPC, lmr=1e-4, standardize=TRUE)
plot(cv_fit_lag_GSPC)

(lambda_min_lag_GSPC <- cv_fit_lag_GSPC$lambda.min)
(lasso_coef_lag_GSPC <- coef(cv_fit_lag_GSPC, s = "min"))
(lasso_coef_lag_GSPC_1 <- coef(cv_fit_lag_GSPC, s = "1se"))


# RUA lagged

x_lag_RUA = model.matrix(RUA_Returns ~ Lagged_Music_Sentiment + Lagged_RUA_Returns, data = data)[, -1]
y_lag_RUA = data$RUA_Returns

cv_fit_lag_RUA <- cv.gamlr(x=x_lag_RUA, y=y_lag_RUA, lmr=1e-4, standardize=TRUE)
plot(cv_fit_lag_RUA)

(lambda_min_lag_RUA <- cv_fit_lag_RUA$lambda.min)
(lasso_coef_lag_RUA <- coef(cv_fit_lag_RUA, s = "min"))
(lasso_coef_lag_RUA_1 <- coef(cv_fit_lag_RUA, s = "1se"))

# GSPC lagged Risk adjusted

x_lag_GSPC_RA = model.matrix(GSPC_Risk_Adjusted ~ Lagged_Music_Sentiment + Lagged_GSPC_RA, data = data)[, -1]
y_lag_GSPC_RA = data$GSPC_Risk_Adjusted

cv_fit_lag_GSPC_RA <- cv.gamlr(x=x_lag_GSPC_RA, y=y_lag_GSPC_RA, lmr=1e-4, standardize=TRUE)
plot(cv_fit_lag_GSPC_RA)

(lambda_min_lag_GSPC_RA <- cv_fit_lag_GSPC_RA$lambda.min)
(lasso_coef_lag_GSPC_RA <- coef(cv_fit_lag_GSPC_RA, s = "min"))
(lasso_coef_lag_GSPC_RA_1 <- coef(cv_fit_lag_GSPC_RA, s = "1se"))

# RUA lagged risk adjusted

x_lag_RUA_RA = model.matrix(RUA_Risk_Adjusted ~ Lagged_Music_Sentiment + Lagged_RUA_RA, data = data)[, -1]
y_lag_RUA_RA = data$RUA_Risk_Adjusted

cv_fit_lag_RUA_RA <- cv.gamlr(x=x_lag_RUA_RA, y=y_lag_RUA_RA, lmr=1e-4, standardize=TRUE)
plot(cv_fit_lag_RUA_RA)

(lambda_min_lag_RUA_RA <- cv_fit_lag_RUA_RA$lambda.min)
(lasso_coef_lag_RUA_RA <- coef(cv_fit_lag_RUA_RA, s = "min"))
(lasso_coef_lag_RUA_RA_1 <- coef(cv_fit_lag_RUA_RA, s = "1se"))












# Robustness checks - Table 4 Panel A - with and without controls

model_GSPC_rc_with_control <- glm(GSPC_Returns ~ Music_Sentiment + Lagged_Music_Sentiment + Lagged_GSPC_Returns, data = data)
summary(model_GSPC_rc_with_control)

model_GSPC_rc_without_control <- glm(GSPC_Returns ~ Music_Sentiment + Lagged_Music_Sentiment, data = data)
summary(model_GSPC_rc_without_control)

model_RUA_rc_with_control <- glm(RUA_Returns ~ Music_Sentiment + Lagged_Music_Sentiment + Lagged_RUA_Returns, data = data)
summary(model_RUA_rc_with_control)

model_RUA_rc_without_control <- glm(RUA_Returns ~ Music_Sentiment + Lagged_Music_Sentiment, data = data)
summary(model_RUA_rc_without_control)

model_GSPC_ra_rc_with_control <- glm(GSPC_Risk_Adjusted ~ Music_Sentiment + Lagged_Music_Sentiment + Lagged_GSPC_RA, data = data)
summary(model_GSPC_ra_rc_with_control)

model_GSPC_ra_rc_without_control <- glm(GSPC_Risk_Adjusted ~ Music_Sentiment + Lagged_Music_Sentiment, data = data)
summary(model_GSPC_ra_rc_without_control)
  
model_RUA_ra_rc_with_control <- glm(RUA_Returns ~ Music_Sentiment + Lagged_Music_Sentiment + Lagged_RUA_RA, data = data)
summary(model_RUA_ra_rc_with_control)

model_RUA_ra_rc_without_control <- glm(RUA_Returns ~ Music_Sentiment + Lagged_Music_Sentiment, data = data)
summary(model_RUA_ra_rc_without_control)




































# Double ML but only controlled for respective lagged returns



# For GSPC returns

outcome_var <- "GSPC_Returns"
treatment_var <- "Music_Sentiment"
covariates <- c("Lagged_GSPC_Returns")
# Preparing the model matrix for doubleML
X <- model.matrix(~ ., data = data[covariates])
Y <- data[[outcome_var]]
D <- data[[treatment_var]]
# Using doubleML to estimate the effects
dml_GSPC_returns_1c <- doubleML(X,D,Y, nfold=10)
summary(dml_GSPC_returns_1c)





# FOR Russell 3000 model

outcome_var <- "RUA_Returns"
treatment_var <- "Music_Sentiment"
covariates <- c("Lagged_RUA_Returns")  
# Preparing the model matrix for doubleML
X <- model.matrix(~ ., data = data[covariates])
Y <- data[[outcome_var]]
D <- data[[treatment_var]]
# Using doubleML to estimate the effects
dml_RU_returns_1c <- doubleML(X,D,Y, nfold=10)
summary(dml_RU_returns_1c)






# FOR GSPC Risk adjusted model

outcome_var <- "GSPC_Risk_Adjusted"
treatment_var <- "Music_Sentiment"
covariates <- c("Lagged_GSPC_RA")  
# Preparing the model matrix for doubleML
X <- model.matrix(~ ., data = data[covariates])
Y <- data[[outcome_var]]
D <- data[[treatment_var]]
# Using doubleML to estimate the effects
dml_GSPC_RA_returns_1c <- doubleML(X,D,Y, nfold=10)
summary(dml_GSPC_RA_returns_1c)







# FOR Russell 3000 risk adjusted model

outcome_var <- "RUA_Risk_Adjusted"
treatment_var <- "Music_Sentiment"
covariates <- c("Lagged_RUA_Returns")  
# Preparing the model matrix for doubleML
X <- model.matrix(~ ., data = data[covariates])
Y <- data[[outcome_var]]
D <- data[[treatment_var]]
# Using doubleML to estimate the effects
dml_RU_RA_returns_1c <- doubleML(X,D,Y, nfold=10)
summary(dml_RU_RA_returns_1c)




# Double ML with autocorrelation (lagged Music Sentiment) & lagged returns

# For GSPC returns

outcome_var <- "GSPC_Returns"
treatment_var <- "Music_Sentiment"
covariates <- c("Lagged_Music_Sentiment", "Lagged_GSPC_Returns")
# Preparing the model matrix for doubleML
X <- model.matrix(~ ., data = data[covariates])
Y <- data[[outcome_var]]
D <- data[[treatment_var]]
# Using doubleML to estimate the effects
dml_GSPC_returns <- doubleML(X,D,Y, nfold=10)
summary(dml_GSPC_returns)





# FOR Russell 3000 model

# Setup for doubleML - choosing an outcome and treatment
outcome_var <- "RUA_Returns"
treatment_var <- "Music_Sentiment"
covariates <- c("Lagged_Music_Sentiment", "Lagged_RUA_Returns")  
# Preparing the model matrix for doubleML
X <- model.matrix(~ ., data = data[covariates])
Y <- data[[outcome_var]]
D <- data[[treatment_var]]
  # Using doubleML to estimate the effects
dml_RU_returns <- doubleML(X,D,Y, nfold=10)
summary(dml_RU_returns)






# FOR GSPC Risk adjusted model

outcome_var <- "GSPC_Risk_Adjusted"
treatment_var <- "Music_Sentiment"
covariates <- c("Lagged_Music_Sentiment", "Lagged_GSPC_RA")  
# Preparing the model matrix for doubleML
X <- model.matrix(~ ., data = data[covariates])
Y <- data[[outcome_var]]
D <- data[[treatment_var]]
# Using doubleML to estimate the effects
dml_GSPC_RA_returns <- doubleML(X,D,Y, nfold=10)
summary(dml_GSPC_RA_returns)







# FOR Russell 3000 risk adjusted model

outcome_var <- "RUA_Risk_Adjusted"
treatment_var <- "Music_Sentiment"
covariates <- c("Lagged_Music_Sentiment", "Lagged_RUA_Returns")  
# Preparing the model matrix for doubleML
X <- model.matrix(~ ., data = data[covariates])
Y <- data[[outcome_var]]
D <- data[[treatment_var]]
# Using doubleML to estimate the effects
dml_RU_RA_returns <- doubleML(X,D,Y, nfold=10)
summary(dml_RU_RA_returns)







# Just some extra analysis to find if there's any relation between lagged Music treatment & weekly returns

outcome_var <- "GSPC_Returns"
treatment_var <- "Lagged_Music_Sentiment"
covariates <- c("Lagged_GSPC_Returns")  
# Preparing the model matrix for doubleML
X <- model.matrix(~ ., data = data[covariates])
Y <- data[[outcome_var]]
D <- data[[treatment_var]]
# Using doubleML to estimate the effects
dml_lag_GSPC_returns <- doubleML(X,D,Y, nfold=10)
# Print the summary of the double machine learning results
summary(dml_lag_GSPC_returns)





# FOR Lagged Rusell 3000 model

# Setup for doubleML - choosing an outcome and treatment
outcome_var <- "RUA_Returns"
treatment_var <- "Lagged_Music_Sentiment"
covariates <- c("Lagged_RUA_Returns")  
# Preparing the model matrix for doubleML
X <- model.matrix(~ ., data = data[covariates])
Y <- data[[outcome_var]]
D <- data[[treatment_var]]
# Using doubleML to estimate the effects
dml_lag_RUA_Returns <- doubleML(X,D,Y, nfold=10)
# Print the summary of the double machine learning results
summary(dml_lag_RUA_Returns)












# FOR Lagged GSPC risk adjusted model

# Setup for doubleML - choosing an outcome and treatment
outcome_var <- "GSPC_Risk_Adjusted"
treatment_var <- "Lagged_Music_Sentiment"
covariates <- c("Lagged_GSPC_RA")  
# Preparing the model matrix for doubleML
X <- model.matrix(~ ., data = data[covariates])
Y <- data[[outcome_var]]
D <- data[[treatment_var]]
# Using doubleML to estimate the effects
dml_lag_GSPC_RA <- doubleML(X,D,Y, nfold=10)
# Print the summary of the double machine learning results
summary(dml_lag_GSPC_RA)





# FOR Lagged Russell risk adjusted model

# Setup for doubleML - choosing an outcome and treatment
outcome_var <- "RUA_Risk_Adjusted"
treatment_var <- "Lagged_Music_Sentiment"
covariates <- c("Lagged_RUA_RA")  
# Preparing the model matrix for doubleML
X <- model.matrix(~ ., data = data[covariates])
Y <- data[[outcome_var]]
D <- data[[treatment_var]]
# Using doubleML to estimate the effects
dml_lag_RUA_RA <- doubleML(X,D,Y, nfold=10)
# Print the summary of the double machine learning results
summary(dml_lag_RUA_RA)

