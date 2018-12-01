source('../loader.R', chdir = T)
train.responders <- train.full %>% filter(respond == 1, tof > 0)
train.responders <- train.responders[-c(55, 65, 68, 70, 131, 171, 256), ]
ml.baseline <- lm(logtargamt ~ recency + frequency + amount + tof + 
                 ordersPer + amountPer + sumQty + maxPrice + returned + 
                 oneMonth + threeMonth + sixMonth + oneYear + overYear, 
               data=train.responders)
# step(ml.baseline, direction = "both")
ml.best <- lm(formula = logtargamt ~ recency + frequency + amount + amountPer + 
             sumQty + oneMonth, data = train.responders)
