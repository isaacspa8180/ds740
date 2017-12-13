library(e1071)
library(nnet)

#Used in the lm cross-validation.
add_all_factors <- function(data, model){
    for (col in colnames(data)) {
        #column_value <- data[col][[1]]
        column_value <- data[, col]
        if (is.factor(column_value)){
            model$xlevels[[col]] <- levels(column_value)
        }
    }    
    return(model)
}


get_error_rate <- function (y_hat, y) {
    r <- y_hat - y
    r_squared <- r ^ 2
    rmse <- mean(r_squared)
    return (rmse)
}


#Used in the perform_cross_validation function.
get_cv_groups <- function(n, k, seed) {
    if (n %% k == 0) {
        groups <- rep(1:k, floor(n/k))
    } else {
        groups <- c(rep(1:k, floor(n/k)), 1:(n%%k))
    }
    if (seed) {set.seed(seed)}
    cv_groups <- sample(groups, n)
    return(cv_groups)
}


perform_cross_validation <- function (formula, data, type, k=10, seed=13, ...) {
    args <- list(...)
    cv_groups <- get_cv_groups(nrow(data), k, seed)
    all_predicted_cv <- rep(NA, nrow(data))
    for (i in 1:k) {
        training_data <- data[which(cv_groups != i), ]
        validation_data <- data[which(cv_groups == i), ]
        if (type == 'lm'){
            model_bad_factors <- lm(formula, data=training_data)
            model <- add_all_factors(data, model_bad_factors)
        }
        if (type == 'svm'){
            #must supply kernel, gamma, and cost 
            model <- svm(formula=formula, data=training_data, 
                         kernel=args$kernel, gamma=args$gamma, cost=args$cost)
        } else if (type == 'ann'){
            #must supply size
            model <- nnet(formula=formula, data=training_data, size=args$size, 
                          linout=TRUE, trace=FALSE, maxit=1000)
        }
        predicted_cv <- predict(model, validation_data)
        all_predicted_cv[cv_groups == i] <- predicted_cv
    }
    return(all_predicted_cv)
}

tune_models <- function (formula, data, 
                         included_models=c('lm', 'svm', 'ann'), ...){
    args <- list(...)
    tuned_models <- list()
    if ('lm' %in% included_models){
        lm_tune_results <-tune(lm, formula, data=data)
        tuned_models <- append(tuned_models, list('lm'=lm_tune_results))
    }
    if ('svm' %in% included_models){
        svm_tune_results <- tune.svm(formula, data=df, kernel=args$kernel,
                                     cost=args$costs, gamma=args$gammas)
        tuned_models <- append(tuned_models, list('svm'=svm_tune_results))
    }
    if ('ann' %in% included_models){
        ann_tune_results <- tune.nnet(formula, data=df, size=args$sizes, 
                                      linout=TRUE, trace=FALSE, maxit=1000)
        tuned_models <- append(tuned_models, list('ann'=ann_tune_results))
    }
    return (tuned_models)
}

