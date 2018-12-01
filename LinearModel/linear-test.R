source('./linear-model.R')
test.full <- test.full %>% select(id, logtargamt, recency, frequency, amount, tof, respond)
test.full <- test.full %>% mutate(ordersPer = frequency / tof, amountPer = amount / tof)
test.orders <- merge(test.full, orders, by = 'id')
test.full <- test.full %>% 
  merge(test.orders %>% 
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
test.responders <- test.full %>% filter(logtargamt > 0, tof > 0)
predict(best, newdata = test.responders)
