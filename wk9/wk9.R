df <- read.csv('Oak_log.csv')
plot(scale(df$logSize), 
     scale(df$logRange), 
     pch=c(21,22)[df$Region], 
     col=c('red','blue')[df$Region],
     main='Scatterplot log(range) vs log(accorn size)',
     xlab='log(accorn size)',
     ylab='log(range)')

legend('topleft', 
       legend=c('Atlantic', 'California'), 
       pch=c(21,22), 
       col=c('red', 'blue'))

b <- fit_svm$rho
w <- colSums(fit_svm$coefs[ ,1] * fit_svm$SV) #beta_1, ..., beta_p

abline(b / w[2], -w[1] / w[2]) #support vector classifier
abline((b + 1) / w[2], -w[1] / w[2], lty=2)
abline((b - 1) / w[2], -w[1] / w[2], lty=2)

#3
library(e1071) #svm
fit_svm <- svm(Region ~ logSize + logRange, 
               data=df,
               kernel='linear',
               cost=1)
table(fit_svm$fitted, df$Region)


#7
library(e1071)
library(ISLR)
Auto['high_gas'] <- 0
high_gas_index <- with(Auto, which(mpg > median(mpg)))
Auto[high_gas_index, 'high_gas'] <- 1
Auto$high_gas <- as.factor(Auto$high_gas)
Auto$origin <- as.factor(Auto$origin)
Auto$mpg <- NULL
Auto$name <- NULL
#8
fit_svm <- svm(high_gas ~ ., data=Auto, kernel='linear', cost=1)
#9
set.seed(9)
tune_out <- tune(svm,
                 high_gas ~ ., 
                 data=Auto,
                 kernel='linear', 
                 type='C-classification', 
                 ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100)))
#10
#set.seed(9)
tune_out2 <- tune(svm,
                  high_gas ~ .,
                  data=Auto,
                  kernel='radial',
                  type='C-classification',
                  ranges=list(cost=c(0.001, 0.01, 0.1, 1, 5, 10, 100),
                              gamma=c(0.5, 1, 2, 3, 4)))

#16
fit_best_svm <- tune_out2$best.model
new_data <- data.frame(cylinders=4, 
                       displacement=132.5, 
                       horsepower=155, 
                       weight=2910, 
                       acceleration=8.3, 
                       year=77,
                       origin=factor(1, levels=c(1, 2, 3)))
predict(fit_best_svm, new_data)
