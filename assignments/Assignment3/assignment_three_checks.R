library(tidyverse)
library(agricolae)
# Problem 1 a)
data_for_one <- data.frame(cotton_weights = gl(n = 5, k = 5, labels = c("15", "20", "25", "30", "35")),
                           data = c(7, 7, 15, 11, 9, 12, 17, 12, 18, 18, 14, 19, 19, 18, 18, 19, 25, 22, 19, 23, 7, 10, 11, 15, 11))
anova_for_one <- aov(data ~ cotton_weights, data_for_one)

summary(anova_for_one)


# Problem 1 b)
sqrt(2 * 8.06 / 5)
2.086*1.79555

LSD.test(anova_for_one, "cotton_weights", console = TRUE, group = FALSE, alpha = 0.05)
# Study: anova_for_one ~ "cotton_weights"
# 
# LSD t Test for data 
# 
# Mean Square Error:  8.06 
# 
# cotton_weights,  means and individual ( 95 %) CI
# 
# data      std r       LCL      UCL Min Max
# 15  9.8 3.346640 5  7.151566 12.44843   7  15
# 20 15.4 3.130495 5 12.751566 18.04843  12  18
# 25 17.6 2.073644 5 14.951566 20.24843  14  19
# 30 21.6 2.607681 5 18.951566 24.24843  19  25
# 35 10.8 2.863564 5  8.151566 13.44843   7  15
# 
# Alpha: 0.05 ; DF Error: 20
# Critical Value of t: 2.085963 
# 
# Comparison between treatments means
# 
# difference pvalue signif.         LCL        UCL
# 15 - 20       -5.6 0.0054      **  -9.3454518 -1.8545482
# 15 - 25       -7.8 0.0003     *** -11.5454518 -4.0545482
# 15 - 30      -11.8 0.0000     *** -15.5454518 -8.0545482
# 15 - 35       -1.0 0.5838          -4.7454518  2.7454518
# 20 - 25       -2.2 0.2347          -5.9454518  1.5454518
# 20 - 30       -6.2 0.0025      **  -9.9454518 -2.4545482
# 20 - 35        4.6 0.0186       *   0.8545482  8.3454518
# 25 - 30       -4.0 0.0375       *  -7.7454518 -0.2545482
# 25 - 35        6.8 0.0012      **   3.0545482 10.5454518
# 30 - 35       10.8 0.0000     ***   7.0545482 14.5454518



# Problem 2
data_for_two <- data.frame(circuit_type = gl(n = 3, k = 5, labels = c("1", "2", "3")),
                           response_time = c(9, 12, 10, 8, 15, 20, 21, 23, 17, 30, 6, 5, 8, 16, 7))
data_for_two %>% 
  group_by(circuit_type) %>%
  summarize(mean_response_time = mean(response_time))

data_for_two %>%
  summarize(mean_response_time = mean(response_time))

data_for_two %>%
  group_by(circuit_type) %>%
  summarize(sum_response_time = sum(response_time)) %>%
  ungroup() %>%
  summarize(sum_squared_sum_response_time = sum(sum_response_time ^ 2))
  

data_for_two %>%
  summarize(sum(response_time) ^ 2 / 25)
17001*0.2 - 1713.96


data_for_two %>%
  summarize(sum(response_time ^ 2))

# pvalue 
pf(271.8/16.9, 2, 12, lower.tail = FALSE)

anova_for_two <- aov(response_time ~ circuit_type, data_for_two)
summary(anova_for_two)


## 2 b)
sqrt(16.9/5)

3.08*1.838478

TukeyHSD(anova_for_two)

# Problem 3
data_for_three <- data.frame(batches = gl(5, 5, labels = c("batch_1", "batch_2", "batch_3", "batch_4", "batch_5")),
                             calcium_content = c(23.46, 23.48, 23.56, 23.39, 23.40, 23.59, 23.46, 23.42, 23.49, 23.50, 
                                                 23.51, 23.64, 23.46, 23.52, 23.49, 23.28, 23.40, 23.37, 23.46, 23.39, 
                                                 23.29, 23.46, 23.37, 23.32, 23.38))
#y_i_dot squared summed
data_for_three %>%
  group_by(batches) %>%
  summarize(squared_sum = sum(calcium_content) ^ 2) %>%
  ungroup() %>%
  summarize(sum_squared_sum = sum(squared_sum))

data_for_three %>%
  summarize(sum(calcium_content) ^ 2)
68701/5
343501.5/25
13740.2-13740.06

# SS_tot
data_for_three %>%
  summarize(sum(calcium_content ^ 2))

data_for_three %>%
  summarize(sum(calcium_content) ^ 2)
343501.5/25
0.04/20
0.035/0.002
pf(17.5, 4, 20, lower.tail = FALSE)
