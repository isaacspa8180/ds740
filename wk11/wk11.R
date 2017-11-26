#1
library(arules)
data(Groceries)
summary(Groceries)

#4
itemFrequencyPlot(Groceries, support=0.05)

#5
rules <- apriori(Groceries, 
                 parameter=list(support=0.001, 
                                confidence=0.5))

#6
sub_rules <- head(rules, n=10, by='lift')
inspect(sub_rules)

#11
#unique_rules <- rules[which(!is.redundant(rules))]
nonRedundant = which(interestMeasure(rules, 
                                     measure='improvement', 
                                     transactions=NULL, 
                                     reuse=TRUE, 
                                     quality_measure='confidence') >= 0)
unique_rules<- rules[nonRedundant]
my_rhs <- rhs(unique_rules)
pastry_index <- which(my_rhs %in% c('pastry'))
inspect(unique_rules[pastry_index])

#14
df <- read.csv('HeartDisease.csv')
df['hasCP'] <- 0
df[which(df$ChestPain == 4), 'hasCP'] <- 1
df$ChestPain <- NULL

#15
df['Age_disc'] <- discretize(df$Age, categories=3, ordered=TRUE)
df$Age <- NULL

#16
#categories <- summary(df$BloodPressure)[c('1st Qu.', '3rd Qu.')]
categories <- summary(df$BloodPressure)[c('Min.', '1st Qu.', '3rd Qu.', 'Max.')]
df['BloodPressure_disc'] <- discretize(df$BloodPressure, 
                                       'fixed', 
                                       categories=categories, 
                                       ordered=TRUE)
df$BloodPressure <- NULL

for (col in colnames(df)) {
    col_values <- df[col][[1]]
    if (is.numeric(col_values)) {
        df[col] <- as.factor(col_values)
    }
}
trans <- as(df, 'transactions')

#18
rules2 <- apriori(trans, 
                  parameter=list(support=.03, confidence=0.5), 
                  appearance=list(rhs=c('hasHD=1'), 
                                  default='lhs'))

#19
female_index <- which(lhs(rules2) %in% c('Sex=0'))
rules3 <- rules2[female_index]
inspect(head(rules3, n=10, by='lift'))
