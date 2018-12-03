library(dplyr)

# Load in all raw data sets
book <- read.csv('./data/book.csv')
test <- read.csv('./data/booktest.csv')
train <- read.csv('./data/booktrain.csv')
orders <- read.csv('./data/ordersall.csv', stringsAsFactors = F)

# Add respond column and interaction terms
book <- book %>% mutate(
  ordersPer = frequency / tof, 
  amountPer = amount / tof,
  respond = as.factor(ifelse(book$logtargamt > 0, 1, 0))
)

# Orders with orddate as Date and net as total price of order
orders$orddate <- lubridate::dmy(orders$orddate)
orders$net <- orders$qty * orders$price
orders <- orders %>% mutate(returned = ifelse(price == 0, 1, 0))
orders <- orders  %>% filter(qty < 1000)

# Add date buckets to orders
startDate <- as.Date('01-Aug-14', '%d-%B-%y')
orders <- orders %>% 
  mutate(dateDiff = difftime(startDate, orddate),
         oneMonth = ifelse(dateDiff < 31, 1, 0),
         threeMonth = ifelse((dateDiff < 63) & (dateDiff > 31), 1, 0),
         sixMonth = ifelse((dateDiff < 183) & (dateDiff > 63), 1, 0),
         oneYear = ifelse(dateDiff < 365 & dateDiff > 183, 1, 0),
         overYear = ifelse(dateDiff > 365, 1, 0))

# Add avgNetOrder feature
book <- orders %>% 
  group_by(id) %>% 
  summarise(avgNetOrder = mean(net),
            sumQty = sum(qty),
            maxPrice = max(price),
            maxNet = max(net),
            returned = max(returned),
            oneMonth = max(oneMonth),
            threeMonth = max(threeMonth),
            sixMonth = max(sixMonth),
            oneYear = max(oneYear),
            overYear = max(overYear)) %>%
  merge(book, by = 'id', all.y = T)

# Add new interaction term for aggregate sumQty books ordered and total time on file
book$sumQtyPerTof <- book$sumQty / book$tof

# recalculate the amount column for customers after removing the orders of qty > 1000
book <- orders %>%
  group_by(id) %>%
  summarise(new.amount = sum(net)) %>%
  select(id, new.amount) %>%
  merge(book, all.y = T)
book$new.amount[is.na(book$new.amount)] <- 0
book$amount <- book$new.amount
book$new.amount <- NULL

# Drop empty malformat column
train$X <- NULL
test$X <- NULL

# All information about the customers in training set
train.full <- merge(train, book, by = 'id')
train.full$logtargamt.y <- NULL
train.full <- rename(train.full, logtargamt = logtargamt.x)

# All information about customers in the test set
test.full <- merge(test, book, by = 'id')
test.full$logtargamt.y <- NULL
test.full <- rename(test.full, logtargamt = logtargamt.x)
