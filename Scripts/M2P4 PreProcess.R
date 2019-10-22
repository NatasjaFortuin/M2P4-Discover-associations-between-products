library(readr)
install.packages("tidyr")
library(tidyr)
library(ggplot2)
library(lattice)
library(mice)
install.packages("mice")
# Import data----
lineitems <- read_csv2(file = "Data/lineitems.csv")
orders_translated <- read_csv2(file = "Data/orders_translated.csv")

# Find NA's in orders----
anyNA(orders_translated)
md.pattern(orders_translated, plot = TRUE, rotate.names = TRUE)

# Filters out----
NAinOrders <- dplyr::filter(orders_translated,is.na(total_paid))
saveRDS(NAinOrders, file = "NAinOrders.rds")

is.na(orders_translated)
sum(is.na(orders_translated))
sum(is.null(orders_translated))
summary(orders_translated)

complete.cases(orders_translated)

#Create subset with only complete cases and no NA's
orders_translated_NoNA <- orders_translated[complete.cases(orders_translated),]
orders_translated_NoNA <- na.omit(orders_translated)

# Find NA's in Line items----
lineitems
anyNA(lineitems)
is.na(lineitems)       
sum(is.na(lineitems))
summary(lineitems)
summary(orders_translated_NoNA)

# Explore line items----
dplyr::count(orders_translated_NoNA)
dplyr::add_count(orders_translated_NoNA, total_paid)

# Explore orders by plots----
ggplot(data = orders_translated_NoNA) + 
  geom_point(mapping = aes(x = state, y = total_paid))
ggsave("PaidPerStateofOrder.png", width = 5, height = 5)

ggplot(data = orders_translated_NoNA, aes(x = state, y = total_paid)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE)

# Detect outliers----
outlier_values <- boxplot.stats(orders_translated_NoNA$total_paid)$out
boxplot(orders_translated_NoNA$total_paid)
outlier_values
summary(outlier_values)

# Look further into state & total paid of orders----
library(dplyr)
dplyr::select(orders_translated_NoNA, total_paid)
orders_translated_NoNA %>% 
group_by(state) %>%
    summarise(avg = mean(total_paid))

orders_translated_NoNA %>% 
count(state, sort = TRUE)

#import transactions of 2 or more items----
library(readr)
trans <- read_csv2(file = "Data/trans1.csv")
View(trans)

# Find NA's in orders----
anyNA(trans)
md.pattern(trans, plot = TRUE, rotate.names = TRUE)

# Explore transaction data----
library(dplyr)
count(trans, items2)
summarise(.data = Test, mean(sleep_total))
trans %>% summarise(mean(items2))
head(trans)
select(trans, items2)

View(orders_translated_NoNA)
summary(orders_translated_NoNA)

View(trans)
nrow(trans)
nrow(orders_translated_NoNA)
nrow(orders_translated_NoNA$state = completed)
nrow(trans$items2)
View(lineitems)
ggplot(data = orders_translated_NoNA)
