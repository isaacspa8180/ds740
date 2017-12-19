library(caret)
#library(leaps)
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
null_formula <- G3 ~ 1
full_formula <- G3 ~ .-G2-G1-school
null_lm_model <- lm(null_formula, data=df)
full_lm_model <- lm(full_formula, data=df)
step_backward <- step(full_lm_model, trace=FALSE, direction='backward')
step_backward_formula <- step_backward$call$formula
#step_forward <- step(null_lm_model, 
					 #scope=list(lower=null_lm_model, upper=full_lm_model),
					 #trace=FALSE,
					 #direction='forward')
#step_forward_formula <- step_forward$call$formula

#==============================================================================
#tuning parameters
#==============================================================================
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
ann_important_formula <- G3 ~ school + address + failures + schoolsup + internet

#------------------------------------------------------------------------------
#lm
#------------------------------------------------------------------------------
#full
lm_full <- tune(lm, full_formula, data=df)
#cv_lm_full <- get_error_rate(perform_cross_validation(full_formula, df, 'lm'), df$G3)
#step_backward
#cv_lm_step_backward <- get_error_rate(perform_cross_validation(step_backward_formula, df, 'lm'), df$G3)
lm_step_backward <- tune(lm, step_backward_formula, data=df)

#------------------------------------------------------------------------------
#svm
#------------------------------------------------------------------------------
#full
#cv_svm_full <- get_error_rate(perform_cross_validation(full_formula, df, 'svm'), df$G3)
svm_full <- tune.svm(full_formula, data=df, kernel='radial') 
svm_full_tuned <- tune.svm(full_formula, data=df, kernel='radial', 
                           cost=costs, gamma=gammas)
#svm_full_tuned_cost <- svm_full_tuned$best.parameters$cost
#svm_full_tuned_gamma <- svm_full_tuned$best.parameters$gamma
#cv_svm_full_tuned <- get_error_rate(perform_cross_validation(full_formula, df, 
                                                             #'svm', cost=svm_full_tuned_cost, 
                                                             #gamma=svm_full_tuned_gamma), df$G3)
#step_backward
#cv_svm_step_backward <- get_error_rate(perform_cross_validation(step_backward_formula, df, 'svm'), df$G3)
svm_step_backward <- tune.svm(step_backward_formula, data=df, kernel='radial') 
svm_step_backward_tuned <- tune.svm(step_backward_formula, data=df, kernel='radial', 
                                    cost=costs, gamma=gammas)
#svm_step_backward_tuned_cost <- svm_step_backward_tuned$best.parameters$cost
#svm_step_backward_tuned_gamma <- svm_step_backward_tuned$best.parameters$gamma
#cv_svm_step_backward_tuned <- get_error_rate(perform_cross_validation(step_backward_formula, df, 
                                                                      #'svm', cost=svm_step_backward_tuned_cost, 
                                                                       #gamma=svm_step_backward_tuned_gamma), df$G3)
#------------------------------------------------------------------------------
#ann
#------------------------------------------------------------------------------
#full
#cv_ann_full <- get_error_rate(perform_cross_validation(full_formula, df, 'ann'), df$G3)
ann_full <- tune.nnet(full_formula, df, size=2)
ann_full_tuned <- tune.nnet(full_formula, data=df, size=sizes, decay=decays)
#ann_full_tuned_size <- ann_full_tuned$best.parameters$size
#ann_full_tuned_decay <- ann_full_tuned$best.parameters$decay
#cv_ann_full_tuned <- get_error_rate(perform_cross_validation(full_formula, df, 
                                                             #'ann', size=ann_full_tuned_size, 
                                                             #decay=ann_full_tuned_decay), df$G3)
#step_backward
#cv_ann_step_backward <- get_error_rate(perform_cross_validation(step_backward_formula, df, 'ann'), df$G3)
ann_step_backward <- tune.nnet(step_backward_formula, data=df, size=2)
ann_step_backward_tuned <- tune.nnet(step_backward_formula, data=df, size=sizes, decay=decays)
#ann_step_backward_tuned_size <- ann_step_backward_tuned$best.parameters$size
#ann_step_backward_tuned_decay <- ann_step_backward_tuned$best.parameters$decay
#cv_ann_step_backward_tuned <- get_error_rate(perform_cross_validation(step_backward_formula, df, 
                                                             #'ann', size=ann_step_backward_tuned_size, 
                                                             #decay=ann_step_backward_tuned_decay), df$G3)
