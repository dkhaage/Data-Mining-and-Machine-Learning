# Association Rules 

#install.packages('arules')
library(arules)

Tr_list <- list(
  c("COOKIES", "JUICE"),
  c("BREAD", "BUTTER", "DIAPERS", "MILK"),
  c("DIAPERS", "JUICE"),
  c("BREAD", "BUTTER", "DIAPERS", "MILK"),
  c("BREAD", "BUTTER", "DIAPERS", "MILK"),
  c("COOKIES", "DIAPERS", "JUICE"),
  c("BREAD", "BUTTER", "COOKIES"),
  c("BREAD", "COOKIES", "DIAPERS", "MILK"),
  c("BREAD", "BUTTER", "MILK"),
  c("BREAD", "DIAPERS", "JUICE", "MILK"),
  c("BUTTER", "DIAPERS"),
  c("BREAD", "BUTTER", "DIAPERS")
)

names(Tr_list) <- paste("Tr_ID_",c(1:12),sep="") # Just give names "Tr_ID_1", ...,"Tr_ID_12" to transactions

head(Tr_list, 5) # Shows first 5 transactions


Tr_object <- as(Tr_list, "transactions")

summary(Tr_object)
inspect(Tr_object)
image(Tr_object)

# Extracting Association Rules

# As a transactions object, dataset Tr_object can be given as input to the function apriori() from the package arules, which can be used to extract frequent itemsets and/or association rules from the data, depending on the list of parameters passed as arguments. 
# The parameters list allows the user to set a large variety of attributes. The most common attributes are support (value within [0,1]), which establishes the minimum relative support for itemsets or rules, confidence (value within [0,1]), 
# which establishes the minimum confidence for rules and is therefore used only when extracting rules, minlen and maxlen (positive integers), 
# which establish the minimum and maximum number of items in a frequent itemset or rule to be returned, respectively, target, which determines the type of result to be returned (e.g., "frequent itemsets" or "rules"), and maxtime, which sets the maximal time for subset checking, in seconds. 
# If a parameter is not specified, default values are used (support = 0.1, confidence = 0.8, minlen = 1, maxlen = 10, maxtime = 5).

FI <- apriori(Tr_object, parameter = list(support = 5/12, target = "frequent itemsets"))
inspect(sort(FI, by="support"))

# The following R code extracts from Tr_object the association rules with minimum support of 5/12 and
# confidence 100% (notice that, since we are interested in rules with at least one item on the left-hand 
# side and at least one item on the right-hand side,
# that is, R:I???J where both I and J are non-empty, we set minlen to 2 in this case):

AR <- apriori(Tr_object, parameter = list(support = 5/12, confidence = 1, minlen = 2, target = "rules"))
# as expected, the resulting two rules match exactly those that we had already manually computed.

# This dataset is available in a CSV file 10_Groceries_Transactions.csv. This file has a header with the variable names in the 1st line, the other 10 lines represent the transactions. Each line contains 8 values (columns) separated by comma (','). In this exercise you are asked to:
#   
# Read the file into a data.frame called TR_10_Frame using read.table()
# Remove the first column (Transaction IDs) and represent the remaining 7 columns as a binary matrix called TR_Matrix
# Convert this matrix into an object of class transactions using the function as(), and name this object TR_obj
# Inspect and visualise this object using the functions summary(), inspect() and image()
# Use the functions apriori() and inspect() to generate the rules with minimum relative support of 3/10 and minimum confidence of 9/10.

TR_10_Frame <- read.table("10_Groceries_Transactions.csv", header=TRUE, sep=",")
TR_Matrix <- as.matrix(TR_10_Frame[,2:8])
TR_obj <- as(TR_Matrix, "transactions")
summary(TR_obj)
inspect(TR_obj)
image(TR_obj)

AR <- apriori(TR_obj, parameter = list(support = 3/10, confidence = 9/10, minlen = 2, target = "rules"))
inspect(AR) # Notice that for the sake of compactness only rules with minimum Righ-Hand-Side (RHS) are shown.

# Any additional rule that results from moving items from the LHS to the RHS in one of the reported
# rules can only have the same or even higher confidence, so it also meets the minimum threshold!!!