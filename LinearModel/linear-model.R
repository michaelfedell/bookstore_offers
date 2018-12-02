source('../loader.R', chdir = T)
# Subset training set to data relevant for multiple regression
train.responders <- train.full %>% filter(respond == 1, tof > 0)
train.responders <- train.responders[-c(55, 65, 68, 70, 131, 171, 256), ]
ml.baseline <- lm(logtargamt ~ recency + frequency + amount + tof + 
                 ordersPer + amountPer + sumQty + maxPrice + returned + 
                 oneMonth + threeMonth + sixMonth + oneYear + overYear, 
               data=train.responders)

# Train best multiple regression model (result of stepwise regression and iteration)
ml.best <- lm(formula = logtargamt ~ recency + frequency + amount + amountPer + 
             sumQty + oneMonth, data = train.responders)
