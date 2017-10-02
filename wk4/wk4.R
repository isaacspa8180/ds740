library(ISLR)
library(MASS)
library(pROC)
library(MVN)


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


#24
qda_formula_1 <- (origin ~ mpg + cylinders + displacement + horsepower + weight)
qda_fit_1 <- qda(qda_formula_1, data=Auto)

fitted_class_qda_1 <- predict(qda_fit_1, data=Auto)$class
tbl_fit_1 <- table(Auto$origin, fitted_class_qda_1)
diag(tbl_fit_1)

new_data <- data.frame(mpg=20, cylinders=8, displacement=320, horsepower=280, weight=3600)
predict(qda_fit_1, new_data)

#26

lda_parameters <- function (response_levels, predictor_variables){
	response_levels + 
		response_levels * predictor_variables + 
		predictor_variables * (predictor_variables + 1) /2
}

qda_parameters <- function (response_levels, predictor_variables){
	response_levels + 
		response_levels * predictor_variables + 
		response_levels * predictor_variables * (predictor_variables + 1) /2
}

#32
n = nrow(Auto)
m = 10
# i am initializing i here because R was complaining if I didn't. Need to figure out why. 
i = 1
groups = c(rep(1:m,floor(n/m)),1:(n%%m))
#groups = c(rep(1:10,39),1,2)
set.seed(4)
cvgroups = sample(groups,n)

formula_1 <- (origin ~ displacement)
formula_2 <- (origin ~ displacement + mpg)
formula_3 <- (origin ~ displacement + mpg + cylinders + horsepower + weight)

perform_cross_validation <- function (formula, lda_or_qda) {
	all_predicted_cv = factor(rep(NA, n), levels=c('1', '2', '3'))
	for (i in 1:m) {
		# match.fun takes a string and finds a function with same name
		model <- match.fun(lda_or_qda)(formula, data=Auto, subset=(cvgroups!=i))
		# I am using the term.label attribute of the model to specify the columns for new data.
		new_data <- data.frame(Auto[cvgroups==i, attr(model$terms, 'term.labels')])
		colnames(new_data) <- attr(model$terms, 'term.labels')
		all_predicted_cv[cvgroups==i] = predict(model, new_data)$class
	}
	return(all_predicted_cv)
}

all_predicted_cv_model_1 <- perform_cross_validation(formula_1, 'lda')
all_predicted_cv_model_2 <- perform_cross_validation(formula_2, 'lda')
all_predicted_cv_model_3 <- perform_cross_validation(formula_3, 'lda')
all_predicted_cv_model_4 <- perform_cross_validation(formula_1, 'qda')
all_predicted_cv_model_5 <- perform_cross_validation(formula_2, 'qda')
all_predicted_cv_model_6 <- perform_cross_validation(formula_3, 'qda')

cv_model_1 = sum(all_predicted_cv_model_1 != Auto$origin) / n; cv_model_1
cv_model_2 = sum(all_predicted_cv_model_2 != Auto$origin) / n; cv_model_2
cv_model_3 = sum(all_predicted_cv_model_3 != Auto$origin) / n; cv_model_3
cv_model_4 = sum(all_predicted_cv_model_4 != Auto$origin) / n; cv_model_4
cv_model_5 = sum(all_predicted_cv_model_5 != Auto$origin) / n; cv_model_5
cv_model_1 = sum(all_predicted_cv_model_1 != Auto$origin) / n; cv_model_1
cv_model_6 = sum(all_predicted_cv_model_6 != Auto$origin) / n; cv_model_6


#42
columns =  c('displacement', 'mpg', 'cylinders', 'horsepower', 'weight')
origin_1 <- with(Auto, Auto[which(origin == 1), columns])
origin_2 <- with(Auto, Auto[which(origin == 2), columns])
origin_3 <- with(Auto, Auto[which(origin == 3), columns])

hzTest(origin_1)
hzTest(origin_2)
hzTest(origin_3)


