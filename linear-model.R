source('./loader.R')
train.full <- train.full %>% select(id, logtargamt, recency, frequency, amount, tof, respond)
train.full <- train.full %>% mutate(ordersper = frequency / tof, amountper = amount / tof)
orders <- orders %>% filter(qty < 1000)
orders <- orders %>% mutate(returned = ifelse(price == 0, 1, 0))
train.orders <- merge(train.full, orders, by = 'id')
train.full <- train.full %>% 
  merge(train.orders %>% 
          group_by(id) %>% 
          summarise(sumQty = sum(qty),
                    maxPrice = max(price),
                    maxNet = max(net),
                    oneMonth = max(oneMonth),
                    threeMonth = max(threeMonth),
                    sixMonth = max(sixMonth),
                    oneYear = max(oneYear),
                    overYear = max(overYear)),
        by = 'id')
