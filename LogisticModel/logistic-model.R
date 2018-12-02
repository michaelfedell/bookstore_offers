source('../loader.R')
train.logistic <- train.full %>% 
  select(id, avgNetOrder, sumQty, logtargamt, sumQtyPerTof,
         recency, frequency, amount, tof, respond,
         oneMonth, threeMonth, sixMonth, oneYear, overYear) %>%
  filter(tof > 0)

log.best <- glm(respond ~ avgNetOrder + sumQty + recency + frequency  + tof + 
                  oneMonth + threeMonth + oneYear + sumQtyPerTof,
                data = train.rebalanced, family = binomial)
