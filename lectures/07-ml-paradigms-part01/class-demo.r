#########################
# Example of a data-to-model ML pipeline (using logistic regression)
#########################

# load libraries
library(tidymodels)

# data collection 
library(palmerpenguins)
penguins
dim(penguins)

# feature engineering 
penguins <- na.omit(penguins)
dim(penguins)
glimpse(penguins)

# quick visualization
ggplot(penguins, aes(bill_length_mm, flipper_length_mm, color = sex)) +
  geom_point(size = 5, alpha = 0.9)

# model training (logistic regression)
logistic_model <- 
  logistic_reg() %>% 
  set_engine("glm") %>% 
  fit(sex ~ bill_length_mm + flipper_length_mm, data = penguins)

# which features seem to matter the most? 
tidy(logistic_model)

# model prediction 
logistic_preds <- predict(logistic_model, penguins) %>%
  bind_cols(penguins)

# model evaluation (and how to interpret the output)
logistic_preds %>% 
  conf_mat(truth = sex, estimate = .pred_class)

# let's quantify performance with some metrics
logistic_preds %>% 
  metrics(truth = sex, estimate = .pred_class)
# metric: accurary (0 to 1) = # correct classifications / total classifications
# metric: Kappa (-1 to 1) is a similar measure to accuracy(), but is normalized
#         by the accuracy that would be expected by chance alone and is very 
#         useful when one or more classes have large frequency distributions


#########################
# Using logistic regression, but adding more features
#   - `species` = what type of species is the penguin?
#   - `body_mass_g` = body mass in grams for each penguin
#########################

# model training (logistic regression)
logistic_model <- 
  logistic_reg() %>% 
  set_engine("glm") %>% 
  fit(sex ~ bill_length_mm + flipper_length_mm + body_mass_g + species, 
      data = penguins)

# which features seem to matter the most? 
tidy(logistic_model)

# model prediction 
logistic_preds <- predict(logistic_model, penguins) %>%
  bind_cols(penguins)

# model evaluation (and how to interpret the output)
logistic_preds %>% 
  conf_mat(truth = sex, estimate = .pred_class)

# let's quantify performance with accuracy and Kappa statistics
logistic_preds %>% 
  metrics(truth = sex, estimate = .pred_class)




#########################
# Example of a data-to-model ML pipeline (using random forests)
#   with a bit more about how tidymodels works
#########################

# define or specify the model you want to use including: 
#   - `mtry` = number of predictors to be rand sampled at each split in tree models
#   - `set_engine` = specifies with package you want to fit the model with
#   - `set_mode` = classification or regression?
rf_spec <- rand_forest(mtry = 3) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification") # because outcome is categorical

# actually fit the model using random forests
rf_fit <- rf_spec %>%
  fit(sex ~ bill_length_mm + flipper_length_mm + body_mass_g + species, 
      data = penguins)

# predict sex for each penguin in our dataset 
rf_preds <- predict(rf_fit, penguins) %>% 
  bind_cols(penguins)

rf_preds %>%
  metrics(truth = sex, estimate = .pred_class)
