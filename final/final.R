library(caret)
library(leaps)
library(e1071)
library(nnet)
source ('helpers.R')


#------------------------------------------------------------------------------
# Main
#------------------------------------------------------------------------------
col_classes <- c('Medu'='factor', 'Fedu'='factor', 'traveltime'='factor', 
                 'studytime'='factor', 'famrel'='factor', 'freetime'='factor', 
                 'goout'='factor', 'Dalc'='factor', 'Walc'='factor', 
                 'health'='factor')
df <- read.csv('student-por.csv', colClasses=col_classes)
predictors <- colnames(df[, -c(31, 32, 33)])
par(mfrow=c(5,6))
for (predictor in predictors) {
    plot(df[, 'G3'] ~ df[, predictor], main=predictor, xlab='', ylab='G3')
}
#Visual check for correlated terms
#pairs(df)
#PCA analysis
pca_results <- prcomp(data.matrix(df[, -c(31,32,33)]), center=TRUE, scale=TRUE)
plot(summary(pca_results)$importance[3,])

#variable selection
#leaps <- regsubsets(full_formula, data=df, nbest=10)
null_formula <- G3 ~ 1
full_formula <- G3 ~ .-G2-G1-school
null_lm_model <- lm(null_formula, data=df)
full_lm_model <- lm(full_formula, data=df)
step_backward <- step(full_lm_model, trace=FALSE, direction='backward')
step_backward_formula <- step_backward$call$formula
step_forward <- step(null_lm_model, 
					 scope=list(lower=null_lm_model, upper=full_lm_model),
					 trace=FALSE,
					 direction='forward')
step_forward_formula <- step_forward$call$formula
results_full <- tune_models(full_formula, df)
results_step_forward <- tune_models(step_forward_formula, df)
ann_important_variables <- varImp(results_full$ann)
rownames(ann_important_variables)[which(ann_important_variables > 4)]
ann_important_formula <- G3 ~ school + address + failures + schoolsup + internet
#results_backward <- tune_models(step_backward_formula, df)
kernel <- 'radial'
gammas <- c(0.00001, 0.0001, 0.001, 0.01)
costs <- c(0.001, 0.01, 0.1, 1, 5, 10, 100)
#RESULTS
#step_forward, kernel=radial, gamma=0.01, cost=4, cv ~ 7.17
sizes <- 1:5
