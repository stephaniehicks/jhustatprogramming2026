library(tidyverse)
library(here)

p1 <- ggplot(penguins, aes(bill_length_mm, flipper_length_mm)) +
  geom_point(size = 5, alpha = 0.9) +
  theme(axis.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20)) 

ggsave(here("lectures", "06-ml-paradigms-part01", "penguins.png"), 
       p1, width = 12, height = 8, units = "in", dpi = 300)

p2 <- ggplot(penguins, aes(bill_length_mm, flipper_length_mm, color = sex)) +
  geom_point(size = 5, alpha = 0.9) +
  theme(axis.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20)) 

ggsave(here("lectures", "06-ml-paradigms-part01", "penguins-by-sex.png"), 
       p2, width = 12, height = 8, units = "in", dpi = 300)




peng_grid <- expand.grid(
  bill_length_mm = seq(min(penguins$bill_length_mm), max(penguins$bill_length_mm), length.out = 100),
  flipper_length_mm = seq(min(penguins$flipper_length_mm), max(penguins$flipper_length_mm), length.out = 100)
)

peng_grid$pred <- predict(logistic_model, peng_grid)$.pred_class

p3 <- ggplot() +
  geom_tile(data = peng_grid, aes(bill_length_mm, flipper_length_mm, fill = pred), alpha = 0.3) +
  geom_point(data = penguins, aes(bill_length_mm, flipper_length_mm, color = sex), size = 2) +
  theme(axis.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20)) 

ggsave(here("lectures", "06-ml-paradigms-part01", "penguins-decision-boundaries.png"), 
       p3, width = 12, height = 8, units = "in", dpi = 300)



# same plot, but now shapes represent different types of species
p4 <- ggplot(penguins, aes(bill_length_mm, flipper_length_mm, color = sex)) +
  geom_point(aes(shape = species), size = 5, alpha = 0.9) + 
  theme(axis.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20))   

ggsave(here("lectures", "06-ml-paradigms-part01", "penguins-by-species.png"), 
       p4, width = 12, height = 8, units = "in", dpi = 300)


# also body mass could be important?
p5 <- ggplot(penguins, aes(bill_length_mm, flipper_length_mm, color = body_mass_g)) +
  geom_point(aes(shape = species), size = 5, alpha = 0.9) + 
  theme(axis.title = element_text(size = 20, face = "bold"),
        axis.text = element_text(size = 20),
        legend.title = element_text(size = 20, face = "bold"),
        legend.text = element_text(size = 20))  

ggsave(here("lectures", "06-ml-paradigms-part01", "penguins-by-bodymass.png"), 
       p5, width = 12, height = 8, units = "in", dpi = 300)
