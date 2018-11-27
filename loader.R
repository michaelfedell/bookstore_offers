library(dplyr)

# Load in all raw data sets
book <- read.csv('./data/book.csv')
book.test <- read.csv('./data/booktest.csv')
book.train <- read.csv('./data/booktrain.csv')
orders <- read.csv('./data/ordersall.csv', stringsAsFactors = F)

# Add respond column to indicate if a customer responded to promotion
book$respond <- ifelse((book$logtargamt > 0) && (book$tof > 0), 1, 0)

# Drop empty malformat column
book.train$X <- NULL

# Orders with orddate as Date and net as total price of order
orders$orddate <- lubridate::dmy(orders$orddate)
orders$net <- orders$qty * orders$price

# Orders of customers who respond to promotion
orders.responders <- book.train %>% 
  filter(logtargamt > 0) %>%
  merge(orders, by = 'id')

# All information about the customers in training set
book.train.merge <- merge(book.train, book, by = 'id')
book.train.merge$logtargamt.y <- NULL
book.train.merge <- rename(book.train.merge, logtargamt = logtargamt.x)

# All information about customers in training set with addition of avgNetOrder
book.train.full <- orders %>% 
  group_by(id) %>% 
  summarise(avgNetOrder = mean(net)) %>%
  merge(book.train.merge, by = 'id', all.y = T)