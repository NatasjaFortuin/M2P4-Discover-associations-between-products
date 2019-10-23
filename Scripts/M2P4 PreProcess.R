library(readr)
install.packages("tidyr")
library(tidyr)
library(ggplot2)
library(lattice)
library(mice)
library(dplyr)
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

# filter out completed transactions----
View(orders_translated_NoNA)
Ord_Completed <- orders_translated_NoNA %>% 
  filter(state == "Completed")
View(Ord_Completed)

# Join orders with line items
#data=lineitems, product_quantity, sku, unit-price based on product_id
Order_LineItems <- inner_join(x = lineitems %>% 
                                select(id_order, product_quantity), 
                              y = Ord_Completed, by = "id_order")
length(unique(Order_LineItems$id_order))

jt<-Ord_Completed %>% 
  group_by(id_order) %>%
  summarise(n=n_distinct(id_order))
View(jt)

# do I have the same amount of transactions in trans.csv than line items and orders?
nrow(trans)

orders %>% 
  filter(state == "Completed") %>% 
  left_join(lineitems %>% select(id_order, product_quantity)) %>% # find out product quanity
  group_by(id_order) %>% 
  summarise(n = n()) %>% # find out total quantity by order id
  filter(n > 1) %>% # take out all the orders with only 1 product
  nrow() # show me the total quanity of observations

trans_id <- orders %>% 
  filter(state == "Completed") %>% 
  left_join(lineitems %>% select(id_order, product_quantity)) %>% # find out product quanity
  group_by(id_order) %>% 
  summarise(n = n()) %>% # find out total quantity by order id
  filter(n > 1)

Ord_trans_id<-trans_id %>% 
  bind_cols(trans)

View(Ord_trans_id)
View(lineitems)

#add column in line items with product quantity * unit price----
total_product_price_lineitems <- lineitems %>% 
  group_by(id_order) %>% 
  mutate(paid = product_quantity * unit_price)
View(total_product_price_lineitems)

#Creating a table with the aggregated data------------------
#T_from_P <- Products %>%
 # filter(state=="Completed") %>%
  #group_by(id_order) %>%
  #summarise(n=n(),np=sum(product_quantity),
   #         itemsP=paste0(sku,collapse=","),
    #        p_total=sum(product_quantity*unit_price)) %>%
  #filter(n>1)
#Verify the consistency of the prices --------------------
#Orders %>%
 #filter(id_order %in% unlist(T_from_P)) -> O_mt2
#O_mt2 %>%
  #left_join(select(T_from_P,id_order,np,itemsP)) %>%
  #left_join(P_per_o) -> Final_orders
