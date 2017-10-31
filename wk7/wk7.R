library(ISLR)


set.seed(7)
oj_training_set_index <- sample(x=1:nrow(OJ), size=800)
oj_training_set <- OJ[oj_training_set_index, ]
oj_validation_set <- OJ[-oj_training_set_index, ]

#2
library(tree)
oj_tree <- tree(as.factor(Purchase) ~ ., data=oj_training_set)

#4
plot(oj_tree)
text(oj_tree, pretty=FALSE)

#5
oj_actual <- oj_validation_set$Purchase
oj_predict <- predict(oj_tree, oj_validation_set, type='class')

oj_confusion_matrix <- table(oj_actual, oj_predict)

oj_correct <- sum(diag(oj_confusion_matrix))
oj_total <- sum(oj_confusion_matrix)
oj_wrong <- oj_total - oj_correct
oj_error_rate <- oj_wrong / oj_total

#6
oj_cross_validation <- cv.tree(oj_tree, FUN=prune.misclass)

#7
oj_prune <- prune.misclass(oj_tree, best=5)

#8
library(ISLR)

hitters_no_na <- Hitters[complete.cases(Hitters), ]
hitters_no_na['hitters_log_salary'] <- with(hitters_no_na, log(Salary))
hitters_no_na$Salary <- NULL

#9
library(gbm)

hitters_no_na_boost <- gbm(hitters_log_salary ~ .,
						   data=hitters_no_na,
						   distribution='gaussian',
						   n.trees=5000,
						   shrinkage=0.001,
						   interaction.depth=4)

#10
n <- nrow(hitters_no_na)
k <- 10 
groups <- c(rep(1:k, floor(n / k)), 1:(n - floor(n / k) * k))
set.seed(7)
cv_groups <- sample(groups, n)
boost_predict <- rep(-1, n)
lm_predict <- rep(0, n)

for (i in 1:k) {
	group_i <- (cv_groups == i)
	boost <- gbm(hitters_log_salary ~ ., 
				 data=hitters_no_na[!group_i, ],
				 distribution='gaussian',
				 n.trees=5000,
				 shrinkage=0.001,
				 interaction.depth=4)
	lm_fit <- lm(hitters_log_salary ~ .,
				 data=hitters_no_na[!group_i, ])
	boost_predict[group_i] <- predict(boost,
									  newdata=hitters_no_na[group_i, ],
									  n.trees=5000,)
	lm_predict[group_i] <- predict(lm_fit,
								   newdata=hitters_no_na[group_i, ])
}
 mean((boost_predict - hitters_no_na$hitters_log_salary) ^ 2)
 mean((lm_predict - hitters_no_na$hitters_log_salary) ^ 2)

#14
library(ISLR)
library(randomForest)

hitters_no_na <- Hitters[complete.cases(Hitters), ]
hitters_no_na['hitters_log_salary'] <- with(hitters_no_na, log(Salary))
hitters_no_na$Salary <- NULL

predictors <-  ncol(subset(hitters_no_na, 
						   select=-c(hitters_log_salary)))
predictors_regression_random_forest <- floor(predictors / 3)
predictors_classification_random_forest  <- floor(sqrt(predictors))

hitters_no_na_bagging <- randomForest(hitters_log_salary ~ .,
									  data=hitters_no_na,
									  mtry=predictors,
									  importance=TRUE)

hitters_no_na_random_forest <- randomForest(hitters_log_salary ~ .,
											data=hitters_no_na,
											mtry=predictors_regression_random_forest,
											importance=TRUE)
hitters_lm <- lm(hitters_log_salary ~ .,
				 data=hitters_no_na)

#17
hitters_no_na_bagging$importance

#18
set.seed(7)
hitters_no_na_bagging <- randomForest(hitters_log_salary ~ .,
									  data=hitters_no_na,
									  mtry=predictors,
									  importance=TRUE)

mean((hitters_no_na_bagging$predicted - hitters_no_na$hitters_log_salary) ^ 2)
set.seed(7)
hitters_no_na_random_forest_2 <- randomForest(hitters_log_salary ~ .,
											data=hitters_no_na,
											mtry=6,
											importance=TRUE)
mean((hitters_no_na_random_forest_2$predicted - hitters_no_na$hitters_log_salary) ^ 2)

n <- nrow(hitters_no_na)
k <- 10 
groups <- c(rep(1:k, floor(n / k)), 1:(n - floor(n / k) * k))
set.seed(7)
cv_groups <- sample(groups, n)
boost_predict <- rep(-1, n)

for (i in 1:k) {
	group_i <- (cv_groups == i)
	boost <- gbm(hitters_log_salary ~ ., 
				 data=hitters_no_na[!group_i, ],
				 distribution='gaussian',
				 n.trees=5000,
				 shrinkage=0.001,
				 interaction.depth=4)
	boost_predict[group_i] <- predict(boost,
									  newdata=hitters_no_na[group_i, ],
									  n.trees=5000,)
}
mean((boost_predict - hitters_no_na$hitters_log_salary) ^ 2)

