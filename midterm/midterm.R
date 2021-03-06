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

predictors_all <- c('AGE', 'SEX', 'ALKPHOS', 'LAB', 'PHOSMMOL')
factor_response <- 'CAMLEVEL'

knn_best_predictor <- get_best_predictors(df_factor, 'CAMLEVEL', predictors_all, 'knn')
plot_best_k(df_factor[1:116,], df_factor[117:nrow(df_factor),], 'CAMLEVEL', c('AGE', 'LAB'))

#Warning this looks at all possible permutations and takes a long time!
random_forest_best_predictor <- get_best_predictors(df_factor, 'CAMLEVEL', predictors_all, 'random_forest')

#This creates plots of CAMLEVEL vs each of the five predictor variables
par(mfrow=c(1,length(predictors_all)))
for (predictor in predictors_all) {
	plot(df_factor[, 'CAMLEVEL'] ~ df_factor[, predictor],
		 main=sprintf('Spineplot of calcium level versus %s', predictor),
		 xlab=predictor,
		 ylab='CAMLEVEL')
}

#Below is the double cross validation section
##### model assessment OUTER CV (with model selection INNER CV as part of model-fitting) #####
fulldata.out = df_factor
k.out = 10 
n.out = dim(fulldata.out)[1]
#define the cross-validation splits 
groups.out = c(rep(1:k.out,floor(n.out/k.out)),1:(n.out%%k.out))  #produces list of group labels
set.seed(8)
cvgroups.out = sample(groups.out,n.out)  #orders randomly, with seed (8) 

allpredictedCV.out = rep(NA,n.out)
for (j in 1:k.out)  {  #be careful not to re-use loop indices
  groupj.out = (cvgroups.out == j)
  traindata.out = df_factor[!groupj.out,]
  testdata.out = df_factor[groupj.out,]
  ### entire model-fitting process ###
    fulldata.in = traindata.out  # only input the data used to fit the model
    x.in = model.matrix(CAMLEVEL ~ AGE + LAB, data=fulldata.in)[,-(1:4)]
    y.in = fulldata.in[, 'CAMLEVEL']
    k.in = 10 
    n.in = dim(fulldata.in)[1]
    groups.in = c(rep(1:k.in,floor(n.in/k.in)),1:(n.in%%k.in))  #produces list of group labels
#    set.seed(8)   # do not reset seed for each internal loop
    cvgroups.in = sample(groups.in,n.in)  #orders randomly, with seed (8) 
    #KNN cross-validation
   all_best_knn <- rep(NA, n.in)
   for (i in 1:k.in) { 
       groupi.in = (cvgroups.in == i)
       traindata.in = fulldata.in[!groupi.in, ]
       testdata.in = fulldata.in[groupi.in, ]
       best_k = get_best_k(traindata.in, testdata.in, 'CAMLEVEL', c('AGE', 'LAB'))
   }
  ### resulting in bestkKNN ###
  KNNtrainfit.out = knn(traindata.out[, c('AGE', 'LAB')], testdata.out[, c('AGE', 'LAB')], traindata.out[, 'CAMLEVEL'], k=best_k)
  allpredictedCV.out[groupj.out] = KNNtrainfit.out
}
#assessment
y.out = fulldata.out$CAMLEVEL
CV.out = get_error_rate(allpredictedCV.out, y.out)
CV.out
