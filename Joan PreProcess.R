# GOAL: recap data quality process


# libraries ---------------------------------------------------------------
library(tidyverse)

# load data ---------------------------------------------------------------
lineitems_temp <- read_csv2("Data/lineitems.csv")
orders_temp <- read_csv2("Data/orders_translated.csv") 
trans <- read_csv2("Data/trans.csv")
brand <- 
# process -----------------------------------------------------------------

# how can I filter completed orders in orders.csv?
orders_temp %>% 
  filter(state == "Completed")

# check missing values in orders?
anyNA(orders_temp)
orders_temp %>% 
  filter_all(any_vars(is.na(.)))

# find out wich are the duplicated orders_id
unique_orders <- orders_temp %>% 
  distinct(id_order)
View(unique_orders)

line_items_2plus <- lineitems_temp %>% 
  group_by(id_order) %>% 
  summarise(count = n()) %>% 
  filter(count >= 2)
View(line_items_2plus)

orders_completed <- orders_temp %>% 
  filter(state == "Completed") %>% 
  select(id_order)
View(orders_completed)

line_items_completed <- line_items_2plus %>% 
  filter(id_order %in% orders_completed$id_order)
View(line_items_completed)

trans_info <- line_items_completed %>% 
  bind_cols(trans) %>% 
  left_join(y = orders_temp %>% select(total_paid, id_order), by = "id_order") %>% 
  left_join(y = lineitems_temp %>% 
              mutate(total_price = product_quantity * unit_price) %>% 
              select(total_price, id_order), 
            by = "id_order") %>% 
  mutate(difference = total_paid - total_price) 
  # ggplot(aes(x = difference)) +
  #   geom_histogram(bins = 250)

