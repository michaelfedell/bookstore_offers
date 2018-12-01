source('./linear-model.R')
test.responders <- test.full %>% filter(logtargamt > 0, tof > 0)
logtargamt.predictions <- predict(ml.best, newdata = test.responders)
sum((logtargamt.predictions - test.responders$logtargamt)^2)
