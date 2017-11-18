#1
library(ISLR)
library(nnet)
set.seed(10)
fit <- nnet(Purchase ~ LoyalCH + SalePriceMM + PriceDiff, data=OJ, size=1)

#2
library(NeuralNetTools)
plotnet(fit)

#4
fit$fitted.values[1]

#5
zH1 <- fit$wts[1] + sum(fit$wts[2:length(fit$wts)] + OJ[1, 2:length(fit$wts)])
sigmaH1 <- 1/(1+exp(-zH1))

#6
OJClass <- predict(fit, OJ, type='class')
confusion <- table(predicted=OJClass, actual=OJ$Purchase)
(confusion[1,2] + confusion[2,1])/sum(confusion)

#7
OJClass.9 <- rep(NA, length(OJ$Purchase))
OJClass.9[which(fit$fitted.values > .9)] <- "Yes"
OJClass.9[which(fit$fitted.values < .1)] <- "No"

confusion <- table(predvals=OJClass.9, truth=OJ$Purchase)
(confusion[1,2] + confusion[2,1])/sum(confusion)

#9
length(which(is.na(OJClass.9)))

#10
lekprofile(fit)

#11
library(ISLR)
df <- na.omit(Hitters)
df['League01'] <- 0
df[which(df$League == 'N'), 'League01'] <- 1
df$League <- NULL

df['Division01'] <- 0
df[which(df$Division == 'W'), 'Division01'] <- 1
df$Division <- NULL

df['NewLeague01'] <- 0
df[which(df$NewLeague == 'N'), 'NewLeague01'] <- 1
df$NewLeague <- NULL

#12
library(nnet)
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
k <- 10
cv_groups <- get_cv_groups(n=nrow(df), k=k, seed=10)
decay_rate <- seq(0.1, 3, by=0.1)
total_misclass_error <- matrix( , nr=k, nc=length(decay_rate))
for (i in 1:k) {
    training_data <- df[which(cv_groups != i), ]
    validation_data <- df[which(cv_groups == i), ]
    training_data_std <- scale(training_data)
    validation_data_std <- scale(validation_data, 
                                 center=attr(training_data_std, 'scaled:center'), 
                                 scale=attr(training_data_std, 'scaled:scale'))
    for (j in 1:length(decay_rate)) {
        fit <- nnet(Salary ~ ., 
                    data=training_data_std, 
                    size=10,
                    decay=decay_rate[j],
                    linout=TRUE, 
                    trace=FALSE, 
                    maxit=1000) 
        predictions <- predict(fit, validation_data_std)
        misclass_error <- mean((predictions - validation_data_std[, 17]) ^ 2)
        total_misclass_error[i,j] <- misclass_error
    }
}
error <- apply(total_misclass_error, 2, mean)
plot(decay_rate, error, type = "l", lwd = 2, las = 1)
min(error)
which(error == min(error))

#12b
#11
library(ISLR)
df <- na.omit(Hitters)
df['League01'] <- 0
df[which(df$League == 'N'), 'League01'] <- 1
df$League <- NULL

df['Division01'] <- 0
df[which(df$Division == 'W'), 'Division01'] <- 1
df$Division <- NULL

df['NewLeague01'] <- 0
df[which(df$NewLeague == 'N'), 'NewLeague01'] <- 1
df$NewLeague <- NULL

#12
library(nnet)
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
k <- 10
n <- nrow(df)
cv_groups <- get_cv_groups(n=n, k=k, seed=10)
decay_rate <- seq(0.1, 3, by=0.1)
total_square_error <- matrix( , nr=n, nc=length(decay_rate))
for (i in 1:k) {
    group_i = (cv_groups == i) 
    training_data <- df[which(cv_groups != i), ]
    validation_data <- df[which(cv_groups == i), ]
    training_data_std <- scale(training_data)
    validation_data_std <- scale(validation_data, 
                                 center=attr(training_data_std, 'scaled:center'), 
                                 scale=attr(training_data_std, 'scaled:scale'))
    for (j in 1:length(decay_rate)) {
        fit <- nnet(Salary ~ ., 
                    data=training_data_std, 
                    decay=decay_rate[j],
                    size=10,
                    linout=TRUE, 
                    trace=FALSE, 
                    maxit=1000) 
        predictions <- predict(fit, validation_data_std)
        square_error <- (predictions - validation_data_std[, 17]) ^ 2
        total_square_error[group_i,j] <- square_error
    }
}
error <- apply(total_square_error, 2, mean)
plot(decay_rate, error, type = "l", lwd = 2, las = 1)
min(error)
which(error == min(error))

#15
set.seed(10)
df_std <- scale(df)
fit <- nnet(Salary ~ ., 
            data=df_std,
            size=10,
            decay=1.5,
            linout=TRUE, 
            trace=FALSE, 
            maxit=300) 
library(NeuralNetTools)
garson(fit)
