library(class) #KNN
library(gtools) #combinations
library(randomForest) #bagging and random forest

source('helpers.R')

#Using this to create factor columns for use in colClasses parameter of read.csv
factor_columns <- c('SEX', 'LAB', 'AGEGROUP')
factor_values <- rep('factor', length(factor_columns))
factors <- setNames(factor_values, factor_columns)

df_with_na <- read.csv('calciumgood.csv', colClasses=factors)

#Removing na as my imputation method
df <- na.omit(df_with_na)

#Variable and formula set-up
df_factor <- df
#CAMMOL removing quantitative response
df_factor$CAMMOL <- NULL
#removing AGEGROUP as it is correlated with Age
df_factor$AGEGROUP <- NULL

#Descriptive statistics
dim(df_factor)
str(df_factor)
summary(df_factor)

predictors_all <- c('AGE', 'SEX', 'ALKPHOS', 'LAB', 'PHOSMMOL')
factor_response <- 'CAMLEVEL'

knn_best_predictor <- get_best_predictors(df_factor, 'CAMLEVEL', predictors_all, 'knn')
plot_best_k(df_factor[1:116,], df_factor[117:nrow(df_factor),], 'CAMLEVEL', c('AGE', 'LAB'))

random_forest_best_predictor <- get_best_predictors(df_factor, 'CAMLEVEL', predictors_all, 'random_forest')

#This creates plots of CAMLEVEL vs each of the five predictor variables
par(mfrow=c(1,length(predictors_all)))
for (predictor in predictors_all) {
	plot(df_factor[, 'CAMLEVEL'] ~ df_factor[, predictor],
		 main=sprintf('Spineplot of calcium level versus %s', predictor),
		 xlab=predictor,
		 ylab='CAMLEVEL')
}

#model_random_forest <- randomForest(make_formula('CAMLEVEL', predictors_all), 
									#data=df_factor, 
									#mtry=2)
