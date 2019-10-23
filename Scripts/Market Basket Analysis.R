#load library
library(arules)
install.packages("caTools")
library(arulesViz)
library(readr)

# Load data----
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
