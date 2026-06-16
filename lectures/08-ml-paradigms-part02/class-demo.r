
#########################
# Using linear regression 
#   Y = body_mass_g (continuous)
#   X = bill_length_mm and species
#########################

# load libraries
library(tidymodels)

# data collection 
library(palmerpenguins)
penguins

# feature engineering 
penguins <- na.omit(penguins)

# model training (linear regression)
linear_model <- 
  linear_reg() %>% 
  set_engine("lm") %>% 
  fit(body_mass_g ~ bill_length_mm + species,
      data = penguins)

# which features seem to matter the most? 
tidy(linear_model)

# model prediction 
linear_preds <- predict(linear_model, penguins) %>%
  bind_cols(penguins)





#########################
# Model evaluation with linear regression models
#   rmse = root mean squared error (more sensitive; penalizes large errors heavily)
#   rsq = coefficient of determination (r-squared)
#   mae = mean absolute error (more robust; treats all error equally)
#########################

linear_preds %>%
  metrics(truth = body_mass_g, estimate = .pred)

  





#########################
# Using linear regression (splitting into train and test data)
#   Y = body_mass_g (continuous)
#   X = bill_length_mm and species
#########################

# split the data into a train and test set 
set.seed(123)
p_split <- initial_split(penguins, prop = 0.8)
p_train <- training(p_split)
p_test  <- testing(p_split)

# sizes of train and test dataset
dim(p_train)
dim(p_test)

# model training (**using only TRAINING dataset**)
linear_model <- 
  linear_reg() %>% 
  set_engine("lm") %>% 
  fit(body_mass_g ~ bill_length_mm + species,
      data = p_train)

# which features seem to matter the most? 
tidy(linear_model)

# model prediction (**on TRAIN dataset**) 
linear_preds_train <- predict(linear_model, p_train) %>%
  bind_cols(p_train)

# model prediction (**on TEST dataset**) 
linear_preds_test <- predict(linear_model, p_test) %>%
  bind_cols(p_test)

# model performance (**on TRAIN dataset**) 
linear_preds_train %>%
  metrics(truth = body_mass_g, estimate = .pred)

# model performance (**on TEST dataset**) 
linear_preds_test %>%
  metrics(truth = body_mass_g, estimate = .pred)




#########################
# Using random forests (splitting into train and test data)
#   Y = body_mass_g (continuous)
#   X = bill_length_mm, flipper_length_mm, sex, species
#########################

library(vip)

rf_spec <- rand_forest(mtry = 3) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("regression") # outcome is continuous
  
rf_fit <- rf_spec %>%
  fit(body_mass_g ~ bill_length_mm + flipper_length_mm + sex + species, 
      data = p_train)

# predict on train dataset
rf_pred_train <- predict(rf_fit, p_train) |> 
  bind_cols(p_train)

# predict on test dataset
rf_pred_test <- predict(rf_fit, p_test) |> 
  bind_cols(p_test)

# train evaluation
rf_pred_train |> 
  metrics(truth = body_mass_g, estimate = .pred)

# test evaluation
rf_pred_test |> 
  metrics(truth = body_mass_g, estimate = .pred)

