source('../loader.R', chdir = T)
# Subset training set to data relevant for logistic regression
train.logistic <- train.full %>% 
  select(id, avgNetOrder, sumQty, logtargamt, sumQtyPerTof,
         recency, frequency, amount, tof, respond, returned,
         oneMonth, threeMonth, sixMonth, oneYear, overYear, amountPer, maxNet) %>%
  filter(tof > 0)

# Oversample responders for training
train.logistic.res <- train.logistic[train.logistic$respond ==1,]
train.logistic.fold <- train.logistic.res[rep(seq(nrow(train.logistic.res)), 3),]
train.rebalanced <- rbind(train.logistic.fold,train.logistic)
length(which(train.rebalanced$respond == 1))

log.baseline <-  glm(respond ~ avgNetOrder + sumQty + recency + frequency  + tof + 
                       amount + amountPer + oneMonth + threeMonth + sixMonth + 
                       oneYear + overYear + sumQtyPerTof + returned + maxNet,
                     data = train.rebalanced, family = binomial)

log.baseline.p <- length(log.baseline$coefficients)

# Remove outliers by Cook's Distance
train.rebalanced <- train.rebalanced %>% 
  filter(cooks.distance(log.baseline) < qf(0.1,
                                           log.baseline.p,
                                           nrow(train.rebalanced) - log.baseline.p + 1))

# Note number of folds by which responders were oversampled
m <- (nrow(filter(train.rebalanced, respond == 1)) / nrow(train.rebalanced)) / 
  (nrow(filter(train.logistic, respond == 1)) / nrow(train.logistic))

# Train best logistic model (result of stepwise regression and iteration)
log.best <- glm(respond ~ recency + frequency  + tof + amount +
                  oneMonth + threeMonth + overYear + maxNet +sumQtyPerTof,
                data = train.rebalanced, family = binomial)
