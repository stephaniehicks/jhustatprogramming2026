library(palmerpenguins)
library(tidyverse)
library(here)

penguins <- na.omit(penguins)

p1 <- ggplot(penguins, aes(bill_length_mm, body_mass_g)) +
  geom_point(size = 5, alpha = 0.9) +
  theme(axis.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20)) 

ggsave(here("lectures", "07-ml-paradigms-part02", "penguins-bodymass.png"), 
       p1, width = 12, height = 8, units = "in", dpi = 300)

# adding a linear model
p2 <- ggplot(penguins, aes(bill_length_mm, body_mass_g)) +
  geom_point(size = 5, alpha = 0.9) +
  geom_smooth(method = "lm", se=FALSE) + 
  theme(axis.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20)) 

ggsave(here("lectures", "07-ml-paradigms-part02", "penguins-bodymass-withlm.png"), 
       p2, width = 12, height = 8, units = "in", dpi = 300)

# colored by species
p3 <- ggplot(penguins, aes(bill_length_mm, body_mass_g, color = species)) +
  geom_point(size = 5, alpha = 0.9) +
  theme(axis.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20)) 

ggsave(here("lectures", "07-ml-paradigms-part02", "penguins-bodymass-by-color.png"), 
       p3, width = 12, height = 8, units = "in", dpi = 300)

# adding a linear model and colored by species
p4 <- ggplot(penguins, aes(bill_length_mm, body_mass_g, color = species)) +
  geom_point(size = 5, alpha = 0.9) +
  geom_smooth(method = "lm", se=FALSE) + 
  theme(axis.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20)) 

ggsave(here("lectures", "07-ml-paradigms-part02", "penguins-bodymass-by-color-withlm.png"), 
       p4, width = 12, height = 8, units = "in", dpi = 300)



#########################
# Using linear regression 
#   Y = body_mass_g (continuous)
#   X = bill_length_mm and species
#########################

# fit a linear regression line with tidymodels
penguins <- na.omit(penguins)
linear_model <- 
  linear_reg() %>% 
  set_engine("lm") %>% 
  fit(body_mass_g ~ bill_length_mm + species, data = penguins)

# model prediction 
linear_preds <- predict(linear_model, penguins) %>%
  bind_cols(penguins)

# model evaluation (and how to interpret the output)
p5 <- linear_preds %>% 
       ggplot(aes(x = body_mass_g, y = .pred, color = species)) + 
       geom_point(size = 5, alpha = 0.9) +
       labs(x="True body mass (g)",
            y="Predicted body mass (g)") + 
       theme(axis.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20))

ggsave(here("lectures", "07-ml-paradigms-part02", "penguins-bodymass-predicted.png"), 
       p5, width = 12, height = 8, units = "in", dpi = 300)



######################### 
# Using linear regression (with train and test)
#   Y = body_mass_g (continuous)
#   X = bill_length_mm and species
#########################

set.seed(123)
p_split <- initial_split(penguins, prop = 0.8)
p_train <- training(p_split)
p_test  <- testing(p_split)

p1_train <- ggplot(p_train, aes(bill_length_mm, body_mass_g, color = species)) +
  geom_point(size = 5, alpha = 0.9) +
  theme(axis.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20)) 

ggsave(here("lectures", "07-ml-paradigms-part02", "penguins-train.png"), 
       p1_train, width = 12, height = 8, units = "in", dpi = 300)

p1_test <- ggplot(p_test, aes(bill_length_mm, body_mass_g, color = species)) +
  geom_point(size = 5, alpha = 0.9) +
  theme(axis.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20)) 

ggsave(here("lectures", "07-ml-paradigms-part02", "penguins-test.png"), 
       p1_test, width = 12, height = 8, units = "in", dpi = 300)


# model training (**using only TRAINING dataset**)
linear_model <- 
  linear_reg() %>% 
  set_engine("lm") %>% 
  fit(body_mass_g ~ bill_length_mm + species,
      data = p_train)

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


# model evaluation 
p2_train <- linear_preds_train %>% 
       ggplot(aes(x = body_mass_g, y = .pred, color = species)) + 
       geom_point(size = 5, alpha = 0.9) +
       labs(x="True body mass (g)",
            y="Predicted body mass (g)") + 
       xlim(2900, 6500) + ylim(2900, 6500) + 
       theme(axis.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20))

ggsave(here("lectures", "07-ml-paradigms-part02", "penguins-train-predicted.png"), 
       p2_train, width = 12, height = 8, units = "in", dpi = 300)


p2_test <- linear_preds_test %>% 
       ggplot(aes(x = body_mass_g, y = .pred, color = species)) + 
       geom_point(size = 5, alpha = 0.9) +
       xlim(2900, 6500) + ylim(2900, 6500) + 
       labs(x="True body mass (g)",
            y="Predicted body mass (g)") + 
       theme(axis.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20))

ggsave(here("lectures", "07-ml-paradigms-part02", "penguins-test-predicted.png"), 
       p2_test, width = 12, height = 8, units = "in", dpi = 300)



######################### 
# Using random forest (with train and test)
#   Y = body_mass_g (continuous)
#   X = bill_length_mm, flipper_length_mm, sex, and species
#########################

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

# model evaluation 
p3_train <- rf_pred_train %>% 
       ggplot(aes(x = body_mass_g, y = .pred, color = species)) + 
       geom_point(size = 5, alpha = 0.9) +
       labs(x="True body mass (g)",
            y="Predicted body mass (g)") + 
       xlim(2900, 6500) + ylim(2900, 6500) + 
       theme(axis.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20))

ggsave(here("lectures", "07-ml-paradigms-part02", "penguins-train-predicted-rf.png"), 
       p3_train, width = 12, height = 8, units = "in", dpi = 300)


p3_test <- rf_pred_test %>% 
       ggplot(aes(x = body_mass_g, y = .pred, color = species)) + 
       geom_point(size = 5, alpha = 0.9) +
       xlim(2900, 6500) + ylim(2900, 6500) + 
       labs(x="True body mass (g)",
            y="Predicted body mass (g)") + 
       theme(axis.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20))

ggsave(here("lectures", "07-ml-paradigms-part02", "penguins-test-predicted-rf.png"), 
       p3_test, width = 12, height = 8, units = "in", dpi = 300)
