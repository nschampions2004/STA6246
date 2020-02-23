library(lme4)
library(PMCMR)
library(agricolae)
library(tidyverse)

# 3.7
# A
tensile_strength <- tibble(levels = gl(n = 4, k = 4, labels = c("1", "2", "3", "4")),
 tensile_strength = c(3129,3000,2865, 2890, 3200, 3300, 
  2975, 3150, 2800, 2900, 2985, 
  3050, 2600, 2700, 2600, 2765))

tensile_anova <- aov(tensile_strength ~ levels, data = tensile_strength)

# B
mes_div_n <- tensile_anova %>% 
  broom::tidy() %>%
  pull(meansq) %>%
  .[[2]] %>%
  map_dbl(.f = function(x) sqrt(x / nrow(tensile_strength))) %>%
  .[[1]] 

tensile_means <- tensile_strength %>%
  group_by(levels) %>%
  summarise(means_ts = mean(tensile_strength, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(means_ts_adj_mse_n = means_ts / mes_div_n) %>% 
  mutate(levels_two =levels)

# C
tidyr::crossing(levels = tensile_means$levels, levels_two = tensile_means$levels_two) %>%
  left_join(tensile_means %>% select(-levels_two, -means_ts_adj_mse_n), 
            by = c("levels" = "levels")) %>%
  left_join(tensile_means %>% 
              select(-levels_two, -means_ts_adj_mse_n), 
            by = c("levels_two" = "levels")) %>%
  filter(!(levels == levels_two)) %>%
  mutate(levels_paired = glue::glue("{levels}_{levels_two}")) %>%
  group_by(levels_paired) %>%
  summarize(diff = means_ts.x - means_ts.y,
    abs_diff = abs(means_ts.x - means_ts.y)) %>%
  ungroup() %>%
  group_by(abs_diff) %>%
  summarize(levels_paired = first(levels_paired)) %>%
  ungroup() %>%
  filter(abs_diff > 174.4761)

LSD.test(tensile_anova, 
         trt = "levels",
         console = TRUE,
         group = F,
         alpha = 0.05)

# D
tensile_anova$model %>%
  bind_cols(residuals = tensile_anova$residuals) %>%
  ggplot(aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line() +
  labs(title = "Normal Probability Plot for Tensile Strength")

# E 
tensile_anova$model %>%
  bind_cols(residuals = tensile_anova$residuals,
            preds = tensile_anova$fitted.values) %>%
  ggplot(aes(x = preds, y = residuals)) +
  geom_point() +
  labs(title = "Residuals plotted against Predicted Values for Tensile Strength")

# 3.11

cotton_data <- tibble(cotton_factor = gl(n = 5, k = 5, label = c("15", "20", "25", "30", "35")),
  tensile_strength = c(7, 7, 15, 11, 9, 12, 17, 12, 18, 18, 14, 19, 19, 18, 
                       18, 19, 25, 22, 19, 23, 7, 10, 11, 15, 11)) %>%
  mutate(cotton_factor = forcats::fct_relevel(cotton_factor, "30"))

cotton_anova <- aov(tensile_strength ~ cotton_factor, data = cotton_data)
dunn.test.control(cotton_data$tensile_strength, cotton_data$cotton_factor)

# 3.14 Golf Problem
# A
golf_data <- tibble(season = as.factor(c(rep("Summer", 10), rep("Shoulder", 7), rep("Winter", 8))),
  scores = c(83, 85, 85, 87, 90, 
             88, 88, 84, 91, 90, 
             91, 87, 84, 87, 85, 
             86, 83, 94, 91, 87, 
             85, 87, 91, 92, 86))
golf_anova <- aov(scores ~ season, golf_data)
summary(golf_anova)  

# B
golf_data %>%
  bind_cols(residuals = golf_anova$residuals) %>%
  ggplot(aes(sample = residuals)) +
    stat_qq() +
    stat_qq_line()

#3.15
# A
contribution_data <- tibble(approach = gl(n = 3, k = 8, labels = c("1", "2", "3")),
  contributions = c(1000, 1500, 1200, 1800, 1600, 1100, 1000, 1250, 1500, 1800,
    2000, 1200, 2000, 1700, 1800, 1900, 900, 1000, 1200, 1500, 
    1200, 1550, 1000, 1100))


contribution_anova <- aov(contributions ~ approach, data = contribution_data)

# B
contribution_data %>%
  bind_cols(residuals = contribution_anova$residuals) %>%
  ggplot(aes(sample = residuals)) +
    stat_qq() +
    stat_qq_line()


calcium_data <- tibble(
  batch = gl(n = 5, k = 5, labels = c("batch_1", "batch_2", "batch_3",
                                             "batch_4", "batch_5")),
  calcium_content = c(23.46, 23.48, 23.56, 23.39, 23.40, 
                      23.59, 23.46, 23.42, 23.49, 23.50, 
                      23.51, 23.64, 23.46, 23.52, 23.49, 
                      23.28, 23.40, 23.37, 23.46, 23.39, 
                      23.29, 23.46, 23.37, 23.32, 23.38)
)

calcium_random_effects <- lmer(calcium_content ~ 1 + (1 | batch), 
                               data = calcium_data)

calcium_aov <- aov(calcium_content ~ batch, data = calcium_data)

confint(calcium_random_effects,
        oldNames = FALSE)


ca_icc <- performance::icc(calcium_random_effects)

# 
# # vacation
# tibble(house = c("Original", "NEW", "Germantown", "Hernando"),
#        whole_price = c(2800, 2100, 1372, 1118)) %>%
#   mutate(total_for_m_and_a = whole_price * (2/9),
#          total_for_kc = whole_price * (4/9),
#          price_per_person_rk = whole_price * (3/9)
#          )
