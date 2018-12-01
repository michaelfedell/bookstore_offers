source('../loader.R')
train.logistic <- train.full %>% 
  select(id, avgNetOrder, sumQty, logtargamt,
         recency, frequency, amount, tof, respond,
         oneMonth, threeMonth, sixMonth, oneYear, overYear) %>%
  filter(tof > 0)
logistic.1 = glm(respond ~ avgNetOrder + sumQty + recency + frequency + amount + tof, 
                    data = train.logistic, family = binomial)
# Remove outliers based on cooks distance
train.logistic <- train.logistic %>% 
  filter(cooks.distance(logistic.1) > qf(0.1,6,8218))
vif(logistic.1)

# Balance training data by resampling
prob1 = ifelse(train.logistic$respond == 0,
               0.85/(length(which(train.logistic$respond == 0))),
               0.15/(length(which(train.logistic$respond == 1))))
train.rebalanced = train.logistic[sample(c(1:nrow(train.logistic)), replace = TRUE, prob = prob1),]
length(which(train.rebalanced$respond == 1))

logistic.2 <- glm(respond ~ avgNetOrder + sumQty + recency + frequency + amount + tof,
                  data = train.rebalanced, family = binomial)
summary(logistic.2)
logistic.3 = glm(respond ~ avgNetOrder + recency + frequency + tof,
                 data = train.rebalanced, family = binomial)
summary(logistic.3)

prob2 <- ifelse(train.logistic$respond == 0,
               0.85/(length(which(train.logistic$respond == 0))),
               0.15/(length(which(train.logistic$respond == 1))))
train.rebalanced2 <- train.logistic[sample(c(1:nrow(train.logistic)), replace = TRUE, prob = prob2),]
summary(glm(respond ~ avgNetOrder + recency + frequency  + tof + 
              oneMonth + threeMonth + sixMonth + oneYear + overYear,
            data = train.rebalanced2, family = binomial))
