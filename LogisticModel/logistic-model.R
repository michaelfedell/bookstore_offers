source('../loader.R')
train.logistic <- train.full %>% 
  select(id, avgNetOrder, sumQty, logtargamt, sumQtyPerTof,
         recency, frequency, amount, amountPer, tof, respond,
         oneMonth, threeMonth, sixMonth, oneYear, overYear) %>%
  filter(tof > 0)

log.best <- glm(formula = respond ~ avgNetOrder + sumQty + recency + frequency + 
                  tof + amountPer + oneMonth + threeMonth + overYear, family = binomial, 
                data = train.rebalanced)
