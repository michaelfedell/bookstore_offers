source('../loader.R', chdir = T, local = T)
# Subset training set to data relevant for multiple regression
train.responders <- train.full %>% 
  select(id, logtargamt, recency, frequency, amount, amountPer, tof, ordersPer, sumQty, 
         maxPrice, returned, oneMonth, threeMonth, sixMonth, oneYear, overYear) %>% 
  filter(logtargamt > 0, tof > 0)
# train.responders <- train.responders[-c(55, 65, 68, 70, 131, 171, 256), ]
ml.baseline <- lm(logtargamt ~ recency + frequency + amount + tof + 
                 ordersPer + amountPer + sumQty + maxPrice + returned + 
                 oneMonth + threeMonth + sixMonth + oneYear + overYear, 
               data=train.responders)

p <- length(ml.baseline$coefficients)
train.responders <- train.responders %>% 
  filter(cooks.distance(ml.baseline) < qf(0.1, p + 1, nrow(train.responders) - p + 1))

# Train best multiple regression model (result of stepwise regression and iteration)
ml.best <- lm(formula = logtargamt ~ frequency + amount + amountPer + sumQty,
              data = train.responders)
