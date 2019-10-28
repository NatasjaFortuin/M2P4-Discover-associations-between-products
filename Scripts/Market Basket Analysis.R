#load library
library(arules)
install.packages("caTools")
library(arulesViz)
library(readr)
library(tidyverse)

# Load data----
# import Electronidex Transaction Data Basket----
Etrans<-read.transactions(file = "Data/ElectronidexTransactions2017.csv", 
                          format = c("basket"), 
                          header = FALSE, sep = ",", 
                          cols = NULL, rm.duplicates = TRUE, 
                          quote = "\"'", skip = 0, 
                          encoding = "unknown")

inspect (Etrans) # View transactions
length (Etrans) # Number of transactions
size (Etrans) # Number of items per transaction
LIST(Etrans) # Lists the transactions by conversion (LIST must be capitalized)
itemLabels(Etrans)# To see the item labels
View(Etrans)
summary(Etrans)

# import Products with Category from cleaned csv----
products_with_category <- read_delim("Data/products_with_category.csv", 
                                     ";", escape_double = FALSE, trim_ws = TRUE)
View(products_with_category)

#import Original Transaction Data----
trans<-read.transactions(file = "Data/trans.csv", 
                                   format = c("basket"), 
                                   header = FALSE, sep = ",", 
                                   cols = NULL, rm.duplicates = TRUE, 
                                   quote = "\"'", skip = 0, 
                                   encoding = "unknown")  


# import Brand names with sku numbers----
products_with_brands <- read_csv2("Data/products_with_brands.csv")

#import product_brand data----
Productbrand <- read_delim("Data/Productbrand.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE)
view(Productbrand)

#join products with category data with brand data----
products_with_category_brand <- inner_join(products_with_category, 
                                           Productbrand, 
                                           by = "sku", copy = FALSE)


# join category and brand ----

products_with_category_brand <- inner_join(products_with_category, 
                                           products_with_brands, 
                                           by = "sku", copy = FALSE)

# DEFINE WHAT TO ANALYSE WITH TRANS file IN MARKET BASKET ANALYSIS----

temp <- trans@itemInfo %>% 
  rename(sku = labels) %>% 
  left_join(products_with_category_brand, by = "sku", copy = F) %>% 
  distinct()

trans@itemInfo$labels <- temp$brand

trans_brand <- aggregate(trans, by = trans@itemInfo$labels)

trans_brand

View(temp)

# downsized trans cat without row 1 accessories, 4 ex warranty, 6 other---- 
trans_categories
filterAggregate(trans_categories, by = trans_categories@iteminfo$labels, @accessories)

#import original Order Data----
orders <- read_csv2("Data/orders_translated.csv") 
nrow(orders)
orders<-orders %>% 
  filter(state == "Completed") %>% 
  group_by(id_order) %>% 
  distinct(id_order, .keep_all = TRUE)
View(orders)

# check missing values in orders?
anyNA(orders_temp)
orders_temp %>% 
  filter_all(any_vars(is.na(.)))


inspect (Trans) # View transactions
length (Trans) # Number of transactions
size (Trans) # Number of items per transaction
LIST(Trans) # Lists the transactions by conversion (LIST must be capitalized)
itemLabels(Trans)# To see the item labels
View(Trans)
summary(Trans)

#import Electronidex Transaction Data Single----
Trans<-read.transactions(file = "Data/ElectronidexTransactions2017.csv", 
                         format = c("single"), 
                         header = FALSE, sep = ",", 
                         cols = NULL, rm.duplicates = TRUE, 
                         quote = "\"'", skip = 0, 
                         encoding = "unknown")

inspect (Trans) # View transactions
length (Trans) # Number of transactions
size (Trans) # Number of items per transaction
LIST(Trans) # Lists the transactions by conversion (LIST must be capitalized)
itemLabels(Trans)# To see the item labels
View(Trans)
summary(Trans)

#### 1ST ITERATION MARKET BASKET ELECTRONIDEX TRANS DATA BASKET ####

#Plot the Top 10 item frequency within the transactions as a bar chart----
ItemFreqPlot<-itemFrequencyPlot(Etrans, type = c("absolute"), 
                   weighted = FALSE, support = NULL, topN = 10,
                   population = NULL, popCol = "black", popLwd = 1,
                   lift = FALSE, horiz = FALSE, 
                   names = TRUE, cex.names =  graphics::par("cex.axis"), 
                   xlab = NULL, ylab = NULL, mai = NULL)
saveRDS(ItemFreqPlot, file = "ItemFreqPlot.rds")

ItemFreqPlot2<-itemFrequencyPlot(Etrans, type = c("relative"), 
                                weighted = FALSE, support = NULL, topN = 10,
                                population = NULL, popCol = "black", popLwd = 1,
                                lift = FALSE, horiz = FALSE, 
                                names = TRUE, cex.names =  graphics::par("cex.axis"), 
                                xlab = NULL, ylab = NULL, mai = NULL)
saveRDS(ItemFreqPlot, file = "ItemFreqPlot2.rds")

#Plot a sample of the population within the transactions as a bar chart----
image(sample(Etrans, 100))

#run apriori model with different support & confidence parameters----
AssRules_01_6 <- apriori(Etrans, parameter = list(supp = 0.01, conf = 0.6))         
inspect(AssRules_01_6)
AssRules_05_4 <- apriori(Etrans, parameter = list(supp = 0.05, conf = 0.4))
inspect(AssRules_05_4)
AssRules_minlen_2 <- apriori(Etrans, parameter = list(minlen=2))
inspect(AssRules_minlen_2)
AssRules_01_5 <- apriori(Etrans, parameter = list(supp = 0.01, conf = 0.5))
saveRDS(AssRules_01_5, file="AssRules_01_5")

inspect(AssRules_01_5)                            
summary(AssRules_01_5)
# AssRules_01_5 gives 19 rules

inspect(sort(AssRules_01_5, by = "support"))
#Top 5 sorting by support =----
#[1]  {HP Laptop,Lenovo Desktop Computer}                => {iMac}      0.02308083 0.5000000  1.952164 227  
#[2]  {Dell Desktop,Lenovo Desktop Computer}             => {iMac}      0.01860702 0.5069252  1.979202 183  
#[3]  {Acer Desktop,HP Laptop}                           => {iMac}      0.01596340 0.5114007  1.996675 157  
#[4]  {Lenovo Desktop Computer,ViewSonic Monitor}        => {iMac}      0.01576004 0.5555556  2.169071 155  
#[5]  {Dell Desktop,ViewSonic Monitor}                   => {HP Laptop} 0.01525165 0.5747126  2.960869 150  

# AssRules_01_5 gives 19 rules
inspect(sort(AssRules_01_5, by = "confidence"))
#[1]  {Acer Aspire,ViewSonic Monitor}                    => {HP Laptop} 0.01077783 0.6022727  3.102856 106  
#[2]  {ASUS 2 Monitor,Lenovo Desktop Computer}           => {iMac}      0.01087951 0.5911602  2.308083 107  
#[3]  {Apple Magic Keyboard,Dell Desktop}                => {iMac}      0.01016777 0.5847953  2.283232 100  
#[4]  {ASUS Monitor,HP Laptop}                           => {iMac}      0.01179461 0.5829146  2.275889 116  
#[5]  {ASUS 2 Monitor,HP Laptop}                         => {iMac}      0.01108287 0.5828877  2.275784 109 

# AssRules_01_5 gives 19 rules
inspect(sort(AssRules_01_5, by = "lift"))
#[1]  {Acer Aspire,ViewSonic Monitor}                    => {HP Laptop} 0.01077783 0.6022727  3.102856 106  
#[2]  {Dell Desktop,ViewSonic Monitor}                   => {HP Laptop} 0.01525165 0.5747126  2.960869 150  
#[3]  {CYBERPOWER Gamer Desktop,ViewSonic Monitor}       => {HP Laptop} 0.01220132 0.5020921  2.586734 120  
#[4]  {ASUS 2 Monitor,Lenovo Desktop Computer}           => {iMac}      0.01087951 0.5911602  2.308083 107  
#[5]  {Apple Magic Keyboard,Dell Desktop}                => {iMac}      0.01016777 0.5847953  2.283232 100 

# AssRules_01_5 gives 19 rules
inspect(sort(AssRules_01_5, by = "count"))
#[1]  {HP Laptop,Lenovo Desktop Computer}                => {iMac}      0.02308083 0.5000000  1.952164 227  
#[2]  {Dell Desktop,Lenovo Desktop Computer}             => {iMac}      0.01860702 0.5069252  1.979202 183  
#[3]  {Acer Desktop,HP Laptop}                           => {iMac}      0.01596340 0.5114007  1.996675 157  
#[4]  {Lenovo Desktop Computer,ViewSonic Monitor}        => {iMac}      0.01576004 0.5555556  2.169071 155  
#[5]  {Dell Desktop,ViewSonic Monitor}                   => {HP Laptop} 0.01525165 0.5747126  2.960869 150 

ItemRules_HP_Laptop <- subset(AssRules_01_5, items %in% "HP Laptop")
View(ItemRules)
inspect(ItemRules)

ItemRules_iMac <- subset(AssRules_01_5, items %in% "iMac")
inspect(ItemRules_iMac)

is.redundant(AssRules_01_5)
is.redundant(AssRules_01_6)
is.redundant(AssRules_02_7)
is.redundant(AssRules_05_4)

#PLOTS----
plot(AssRules_01_5)
ruleExplorer(AssRules_01_5)

#### 2ND ITERATION APRIORI PRODUCTS WITH CATEGORIES ####

Rules_a <-apriori(trans_categories, parameter = list(supp = 0.2, conf = 0.7))         
inspect(Rules_a)
#for all -> accessories = ass prod. Supp 0.948, Conf 0.986, lift 1, count 9917

Rules_b <-apriori(trans_categories, parameter = list(supp = 0.1, conf = 0.6))         
inspect(Rules_b)
#for all -> accessories = ass prod. Supp 0.948, Conf 0.949, lift 1, count 9917

Rules_c <-apriori(trans_categories, parameter = list(supp = 0.05, conf = 0.5))
inspect(Rules_c)             
#lhs                 rhs           support    confidence lift      count
#[1] {}           => {accessories} 0.94863210 0.9486321  1.0000000 9917 
#[2] {other}      => {accessories} 0.05069830 0.8745875  0.9219459  530 
#[3] {tablet}     => {accessories} 0.05375933 0.8934817  0.9418633  562 
#[4] {laptop}     => {accessories} 0.05777693 0.8753623  0.9227627  604 
#[5] {smartphone} => {accessories} 0.06858619 0.7975528  0.8407399  717

Rules_d <-apriori(trans_categories, parameter = list(supp = 0.03, conf = 0.6))
inspect(Rules_d)  
#lhs                        rhs           support    confidence lift      count
#[1] {}                  => {accessories} 0.94863210 0.9486321  1.0000000 9917 
#[2] {smartwhatch}       => {accessories} 0.03156686 0.8730159  0.9202892  330 
#[3] {display}           => {accessories} 0.04983738 0.9172535  0.9669223  521 
#[4] {other}             => {accessories} 0.05069830 0.8745875  0.9219459  530 
#[5] {pc}                => {accessories} 0.04314138 0.7857143  0.8282603  451 
#[6] {tablet}            => {accessories} 0.05375933 0.8934817  0.9418633  562 
#[7] {laptop}            => {accessories} 0.05777693 0.8753623  0.9227627  604 
#[8] {extended warranty} => {accessories} 0.04534150 0.6657303  0.7017793  474 
#[9] {smartphone}        => {accessories} 0.06858619 0.7975528  0.8407399  717 

Rules_e <-apriori(trans_categories, parameter = list(supp = 0.025, conf = 0.5))
inspect(Rules_e)
#lhs                                    rhs                 support    confidence lift      count
#[1]  {}                              => {accessories}       0.94863210 0.9486321  1.0000000 9917 
#[2]  {service}                       => {accessories}       0.02936675 0.9715190  1.0241262  307 
#[3]  {smartwhatch}                   => {accessories}       0.03156686 0.8730159  0.9202892  330 
#[4]  {display}                       => {accessories}       0.04983738 0.9172535  0.9669223  521 
#[5]  {other}                         => {accessories}       0.05069830 0.8745875  0.9219459  530 
#[6]  {pc}                            => {extended warranty} 0.02841018 0.5174216  7.5970863  297 
#[7]  {pc}                            => {accessories}       0.04314138 0.7857143  0.8282603  451 
#[8]  {tablet}                        => {accessories}       0.05375933 0.8934817  0.9418633  562 
#[9]  {laptop}                        => {accessories}       0.05777693 0.8753623  0.9227627  604 
#[10] {extended warranty}             => {smartphone}        0.03434092 0.5042135  5.8632344  359 
#[11] {extended warranty}             => {accessories}       0.04534150 0.6657303  0.7017793  474 
#[12] {smartphone}                    => {accessories}       0.06858619 0.7975528  0.8407399  717 
#[13] {extended warranty,smartphone}  => {accessories}       0.02592309 0.7548747  0.7957507  271 
#[14] {accessories,extended warranty} => {smartphone}        0.02592309 0.5717300  6.6483481  271 
 
ItemFreqPlot2<-itemFrequencyPlot(trans_categories, type = c("absolute"), 
                                  weighted = FALSE, support = NULL, topN = 10,
                                  population = NULL, popCol = "black", popLwd = 1,
                                  lift = FALSE, horiz = FALSE, 
                                  names = TRUE, cex.names =  graphics::par("cex.axis"), 
                                  xlab = NULL, ylab = NULL, mai = NULL)
ruleExplorer(Rules_d)

ItemRules_laptop <- subset(Rules_d, items %in% "laptop")
inspect(ItemRules_laptop)

ItemRules_accessoiries <- subset(Rules_d, items %in% "accessories")
inspect(ItemRules_accessoiries)

?is.redundant
is.redundant(Rules_a)
is.redundant(Rules_b)
is.redundant(Rules_c)
is.redundant(Rules_d)

#PLOTS----
plot(AssRules_01_5)
ruleExplorer(AssRules_01_5)

#### 3RD ITERATION MARKET BASKET ANALYSIS WITH BRAND ####

rules_brands <- apriori(trans_brand, parameter = list(supp = 0.025, conf = 0.7))
inspect(rules_brands)

ruleExplorer(rules_brands)

View(products_with_category_brand)
class(products_with_brands$brand)

products_with_category_brand
arrange(products_with_category_brand, desc(category))

#subset "display", "smartphone", "pc", "tablet", "laptop", "smartwhatch, "other"
#or all except "accessoiries"
Minimized_products<-products_with_category_brand

relevant_cat <- c("display","smartphone", "pc", "tablet", "laptop", 
                  "smartwhatch", "other")

Minimized_products <- Minimized_products %>% 
  filter(category %in% relevant_cat)

View(Minimized_products)

#### 4TH ITERATION ####
#MBA based on downsized categories, all excluding accessoires

# DEFINE WHAT TO ANALYSE WITH TRANS file IN MARKET BASKET ANALYSIS----

#data with relevant category data: Minimized_products

trans_relevant_cat <- trans@itemInfo %>% 
  rename(sku = labels) %>% 
  right_join(Minimized_products, by = "sku", copy = F) %>% 
  distinct()

str(trans_relevant_cat)

glimpse(Minimized_products)
glimpse(trans_relevant_cat)
glimpse(trans_categories)

#### WAT DOET DIT EIGENLIJK, REVIEW JOAN####
trans@itemInfo$labels <- trans_relevant_cat$category

trans_category_downsized <- aggregate(trans, by = trans@itemInfo$labels)

trans_category_downsized

View(trans_category_downsized)

Rules_downsized_a <-apriori(trans_category_downsized, parameter = list(supp = 0.2, conf = 0.7))         
inspect(Rules_downsized_a)
