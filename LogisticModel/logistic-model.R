source('../loader.R', chdir = T)
# Subset training set to data relevant for logistic regression
train.logistic <- train.full %>% 
  select(id, respond, avgNetOrder, sumQty, logtargamt, sumQtyPerTof,
         recency, frequency, amount, amountPer, tof,
         oneMonth, threeMonth, sixMonth, oneYear, overYear) %>%
  filter(tof > 0, sumQty < 1000)

log.baseline = glm(respond ~ avgNetOrder + sumQty + recency + frequency + amount + tof, 
                 data = train.logistic, family = binomial)
# Remove outliers by Cook's Distance
train.logistic <- train.logistic %>% 
  filter(cooks.distance(log.baseline) < qf(0.1,6,8218))

# Oversample responders for training
prob1 <- ifelse(train.logistic$respond == 0,
                0.85/(length(which(train.logistic$respond == 0))),
                0.15/(length(which(train.logistic$respond == 1))))
set.seed(123)
train.rebalanced <- train.logistic[sample(c(1:nrow(train.logistic)), 
                                          replace = TRUE, prob = prob1),]
length(which(train.rebalanced$respond == 1))

# Note number of folds by which responders were oversampled
m <- (nrow(filter(train.rebalanced, respond == 1)) / nrow(train.rebalanced)) / 
  (nrow(filter(train.full, respond == 1)) / nrow(train.full))

# Train best logistic model (result of stepwise regression and iteration)
log.best <- glm(formula = respond ~ avgNetOrder + sumQty + recency + frequency + 
                  tof + amountPer + oneMonth + threeMonth + overYear, family = binomial, 
                data = train.rebalanced)
