library(ISLR)
library(MASS)
library(pROC)


#1
Auto$Domestic <- rep(0, nrow(Auto))
Auto$Domestic[which(Auto$origin==1)] <- 1
table(Auto$Domestic)

#2
boxplot(Auto$mpg ~ Auto$Domestic)

#3
aggregate(Auto$mpg, by=list(Auto$Domestic), mean)

#6
lda_fit_1 <- lda(Domestic ~ mpg, data=Auto)
fitted_class_lda_1 <- predict(lda_fit_1, data=Auto)$class
tbl_fit_1 <- table(Auto$Domestic, fitted_class_lda_1))
tbl_fit_1[2,2]/sum(tbl_fit_1[2,])
tbl_fit_1[1,1]/sum(tbl_fit_1[1,])

#9
others <- with(Auto, data.frame(Auto[ ,c('cylinders', 'displacement', 'horsepower', 'weight', 'acceleration', 'year')])
par(mfrow=c(3,3))
for (name in colnames(others)) {
    boxplot(others[, name] ~ Auto$Domestic, main=name)
}

#11
my_roc <- with(Auto, roc(response=Domestic, predictor=displacement))
plot(my_roc)

#12
with(Auto, plot(mpg ~ displacement, col=origin))


#14
lda_formula_2 <- (origin ~ mpg + cylinders + displacement + horsepower + weight)
lda_fit_2 <- lda(lda_formula_2, data=Auto)

#17
fitted_class_lda_2 <- predict(lda_fit_2, data=Auto)$class
tbl_fit_2 <- table(Auto$origin, fitted_class_lda_2)
diag(tbl_fit_2)

#20
new_data <- data.frame(mpg=20, cylinders=8, displacement=320, horsepower=280, weight=3600)
predict(lda_fit_2, new_data)
