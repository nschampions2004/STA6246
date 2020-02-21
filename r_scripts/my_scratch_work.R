library(tidyverse)

# car example
LR = c(3, 5, 3, 7, 6, 5, 3, 2, 1, 6, 1, 3, 4, 7, 5, 6, 3, 2, 1, 7, 4, 1 ,3, 5, 7, 1, 2, 
       4, 2, 7, 3, 5, 7, 5, 10, 3, 4, 7, 2, 7)
CT = gl(n = 4, k = 10, labels = c("c1", "c2", "c3", "c4"))

car_frame <- tibble(LR, CT)

car_anova <- aov(sqrt(LR) ~ CT)

summary(car_anova)
confint(car_anova)

par(mfrow = c(1, 1))
plot(car_anova)

shapiro.test(sqrt(LR))

# qq plot
qqnorm(sqrt(LR))
qqline(sqrt(LR))

library(magic)

?magic::rlatin

set.seed(8675309)
latin <- rlatin(n = 5) %>% as.vector()
burn_rate = c(24, 17, 18, 26, 22, 
              20, 24, 38, 31, 30, 
              19, 30, 26, 26, 20, 
              24, 27, 27, 23, 29, 
              24, 36, 21, 22, 31)
bt = gl(n = 5, k = 1, length = 25, labels = c(1:5))
op = gl(n = 5, k = 5, labels = c(1:5))

trts <- c("A", "B", "C", "D", "E")

offset_trts <- vector()
counter <- 1
for (j in 1:length(trts)) {
  for(i in 1:length(trts)) {
    if (i + counter - 1 > length(trts)) {
      offset_trts <- append(offset_trts, trts[(i + counter - 1) %% length(trts)])
    } else {
      offset_trts <- append(offset_trts, trts[(i + counter - 1)])
    }
  }
  counter <- counter + 1
}

offset_trts <- vector()
counter <- 1
for (j in 1:length(trts)) {
  for (i in 1:length(trts)) {
    ifelse(i + counter - 1 > length(trts), 
          offset_trts <- append(offset_trts, trts[(i + counter - 1 %% length(trts))]),
          offset_trts <- append(offset_trts, trts[(i + counter - 1)]))
  }
}

offset_trts

data.frame(bt, op, latin, burn_rate)
