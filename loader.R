library(dplyr)

# Load in all raw data sets
book <- read.csv('./data/book.csv')
test <- read.csv('./data/booktest.csv')
train <- read.csv('./data/booktrain.csv')
orders <- read.csv('./data/ordersall.csv', stringsAsFactors = F)

# Add respond column to indicate if a customer responded to promotion
book$respond <- ifelse(book$logtargamt > 0, 1, 0)

# Drop empty malformat column
train$X <- NULL
test$X <- NULL

# Orders with orddate as Date and net as total price of order
orders$orddate <- lubridate::dmy(orders$orddate)
orders$net <- orders$qty * orders$price

# Orders of customers who respond to promotion
orders.responders <- train %>% 
  filter(logtargamt > 0) %>%
  merge(orders, by = 'id')

# All information about the customers in training set
train.merge <- merge(train, book, by = 'id')
train.merge$logtargamt.y <- NULL
train.merge <- rename(train.merge, logtargamt = logtargamt.x)

# All information about customers in the test set
test.merge <- merge(test, book, by = 'id')
test.merge$logtargamt.y <- NULL
test.merge <- rename(test.merge, logtargamt = logtargamt.x)

# 
startDate <- as.Date('01-Aug-14', '%d-%B-%y')
orders <- orders %>% mutate(
  dateDiff = difftime(startDate, orddate),
  oneMonth = ifelse(dateDiff < 31, 1, 0),
  threeMonth = ifelse((dateDiff < 63) & (dateDiff > 31), 1, 0),
  sixMonth = ifelse((dateDiff < 183) & (dateDiff > 63), 1, 0),
  oneYear = ifelse(dateDiff < 365 & dateDiff > 183, 1, 0),
  overYear = ifelse(dateDiff > 365, 1, 0)
)

# All information about customers in training set with addition of avgNetOrder
train.full <- orders %>% 
  group_by(id) %>% 
  summarise(avgNetOrder = mean(net)) %>%
  merge(train.merge, by = 'id', all.y = T)

# All information about customers in test set with addition of avgNetOrder
test.full <- orders %>% 
  group_by(id) %>% 
  summarise(avgNetOrder = mean(net)) %>%
  merge(test.merge, by = 'id', all.y = T)
