library(readr)
library(tidyverse)

# load data ---------------------------------------------------------------
lineitems<- read_csv2("Data/lineitems.csv")
orders <- read_csv2("Data/orders_translated.csv") 
trans <- read_csv2("Data/trans.csv")
View(lineitems)

#### PREPROCESS ####

# Select only completed orders----
orders <- orders %>% 
  filter(state == "Completed")

# check missing values in orders
anyNA(orders)
orders %>% 
  filter_all(any_vars(is.na(.)))

#### WERKT NIET!! #####
# find out wich are the duplicated orders_id
orders %>% 
  distinct(id_order)

# keep only items with more than quantity 2
Plus2_lineitems <- lineitems %>% 
  group_by(id_order)
  summarise(count = n()) %>% 
  select(count >= 2)
#View(newlineitems)

completedlineitems <- newlineitems %>% 
  filter(id_order %in% orders$id_order)

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