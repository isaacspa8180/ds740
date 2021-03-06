library(e1071)
library(nnet)


#==============================================================================
#data loading
#==============================================================================
col_classes <- c('Medu'='factor', 'Fedu'='factor', 'traveltime'='factor', 
                 'studytime'='factor', 'famrel'='factor', 'freetime'='factor', 
                 'goout'='factor', 'Dalc'='factor', 'Walc'='factor', 
                 'health'='factor')
df <- read.csv('student-por.csv', colClasses=col_classes)


#==============================================================================
#dimensionality reduction
#==============================================================================
#------------------------------------------------------------------------------
#pca
#------------------------------------------------------------------------------
pca_results <- prcomp(data.matrix(df[, -c(31,32,33)]), center=TRUE, scale=TRUE)
plot(summary(pca_results)$importance[3,])

#------------------------------------------------------------------------------
#step
#------------------------------------------------------------------------------
full_formula <- G3 ~ .-G2-G1-school
step_backward <- step(lm(full_formula, data=df), trace=FALSE, direction='backward')
step_backward_formula <- step_backward$call$formula


#==============================================================================
#plots
#==============================================================================
#These are plots of the variables indentified in the step backward selection 
#that were deemed important. It is interested to see each compared to the data.
predictors <- c('sex', 'age', 'address', 'Fjob', 'reason', 'studytime', 
                'failures', 'schoolsup', 'higher', 'internet', 'romantic', 
                'famrel', 'goout', 'Dalc', 'health') 
par(mfrow=c(4, 4))
for (predictor in predictors) {
    plot(df[, 'G3'] ~ df[, predictor], main=predictor, xlab='', ylab='G3')
}


#==============================================================================
#tuning parameters
#==============================================================================
#These are used in the tuning of models
#svm
kernel <- 'radial'
gammas <- c(0.0001, 0.001, 0.01, 0.1, 1, 10)
costs <- c(0.001, 0.01, 0.1, 1, 5, 10, 100)

#ann
sizes <- 1:5
decays <- c(.1,.5, 1,5,10,50,100,500)


#==============================================================================
#results
#==============================================================================
#For the results both full variables and the subset explored in the step backward
#selection were used in lm, svm, and ann. Further, tuning of cost and gamma was 
#performed for svm and size and decay for ann.
#------------------------------------------------------------------------------
#lm
#------------------------------------------------------------------------------
#full
lm_full <- tune(lm, full_formula, data=df)
#step_backward
lm_step_backward <- tune(lm, step_backward_formula, data=df)

#------------------------------------------------------------------------------
#svm
#------------------------------------------------------------------------------
#full
svm_full <- tune.svm(full_formula, data=df, kernel=kernel) 
svm_full_tuned <- tune.svm(full_formula, data=df, kernel=kernel, 
                           cost=costs, gamma=gammas)
#step_backward
svm_step_backward <- tune.svm(step_backward_formula, data=df, kernel=kernel) 
#The model below consistently produced the best CV values (CV approx 7.14)
svm_step_backward_tuned <- tune.svm(step_backward_formula, data=df, kernel=kernel, 
                                    cost=costs, gamma=gammas)
#------------------------------------------------------------------------------
#ann
#------------------------------------------------------------------------------
#full
ann_full <- tune.nnet(full_formula, data=df, size=2, linout=TRUE)
ann_full_tuned <- tune.nnet(full_formula, data=df, size=sizes, decay=decays, linout=TRUE)
#step_backward
ann_step_backward <- tune.nnet(step_backward_formula, data=df, size=2, linout=TRUE)
ann_step_backward_tuned <- tune.nnet(step_backward_formula, data=df, size=sizes, decay=decays, linout=TRUE)



