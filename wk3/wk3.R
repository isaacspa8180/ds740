#1
wi <- read.csv('Wisconsin_income.csv', 
               colClasses = c('CIT2' = 'factor',
                              'COW' = 'factor',
                              'LANX' = 'factor',
                              'MAR' = 'factor',
                              'SEX' = 'factor',
                              'DIS' = 'factor',
                              'RAC' = 'factor',
                              'Hispanic' = 'factor'))

#2
par(mfrow = c(3, 1))
with(wi, hist(PERNP, main = "Total person's earnings"))
with(wi, hist(WKHP, main = 'Usual hours worked per week'))
with(wi, hist(JWMNP, main = 'Travel time to work'))

wi['JWMNP_log'] <- log(wi$JWMNP)
wi['PERNP_log'] <- log(wi$PERNP)

#3
wi_best_subset <- regsubsets(PERNP_log ~ .-JWMNP-PERNP, 
                             data = wi, 
                             nvmax = 41)

plot(wi_best_subset)

#4
wi_best_subset_summary <- summary(wi_best_subset)

plot(wi_best_subset_summary$adjr2, 
     xlab = 'Number of variables', 
     ylab = 'Adjusted R^2', 
     type = 'l')
#5
which.max(wi_best_subset_summary$adjr2)

#6
which.min(wi_best_subset_summary$bic)

#7
predict.regsubsets <- function(object, newdata, id, ...){
    form <- as.formula(object$call[[2]])
    mat <- model.matrix(form, newdata)
    coefi <- coef(object, id = id)
    xvars <- names(coefi)
    mat[ , xvars] %*% coefi
}

set.seed(3)
k <- 10
n <- nrow(wi)
max_vars <- 41
groups <- c(rep(1:k, floor(n / k)), 1:(n %% k))
cv_groups <- sample(groups, n)
group_error <- matrix( ,nr = max_vars, nc = k)

for (i in 1:k) {
    group_i <- (cv_groups == i)

    cv_fit <- regsubsets(PERNP_log ~ .-JWMNP-PERNP, 
                         data = wi[!group_i, ], 
                         nvmax = max_vars)

    for (j in 1:max_vars) {
        y_pred <- predict(cv_fit, newdata = wi[group_i, ], id = j)
        group_error[j, i] <- mean((wi$PERNP_log[group_i] - y_pred) ^ 2)
    }
}

mse <- apply(group_error, 1, mean)
plot(mse)
which.min(mse)

#9
se <- apply(group_error, 1, sd) / sqrt(k)
