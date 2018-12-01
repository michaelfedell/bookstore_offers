source('./loader.R', chdir = T)
train.full <- train.full %>% select(id, logtargamt, recency, frequency, amount, tof, respond)
train.full <- train.full %>% mutate(ordersPer = frequency / tof, amountPer = amount / tof)
orders <- orders %>% filter(qty < 1000)
orders <- orders %>% mutate(returned = ifelse(price == 0, 1, 0))
train.orders <- merge(train.full, orders, by = 'id')
train.full <- train.full %>% 
  merge(train.orders %>% 
          group_by(id) %>% 
          summarise(sumQty = sum(qty),
                    maxPrice = max(price),
                    maxNet = max(net),
                    returned = max(returned),
                    oneMonth = max(oneMonth),
                    threeMonth = max(threeMonth),
                    sixMonth = max(sixMonth),
                    oneYear = max(oneYear),
                    overYear = max(overYear)),
        by = 'id')
train.responders <- train.full %>% filter(respond == 1, tof > 0)
train.responders <- train.responders[-c(55, 65, 68, 70, 131, 171, 256), ]
ml.baseline <- lm(logtargamt ~ recency + frequency + amount + tof + 
                 ordersPer + amountPer + sumQty + maxPrice + returned + 
                 oneMonth + threeMonth + sixMonth + oneYear + overYear, 
               data=train.responders)
step(baseline, direction = "both")
ml.best <- lm(formula = logtargamt ~ recency + frequency + amount + amountPer + 
             sumQty + oneMonth, data = train.responders)