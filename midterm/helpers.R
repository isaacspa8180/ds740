#This is perhaps a little too 'clever'. However, it was 
#beneficial for me to supply response and predictors to a
#formula that then pastes it together as a formula. 
make_formula <- function (response, predictors) {
	predictors_head <- predictors[1]
	predictors_tail <- predictors[2:length(predictors)]

	if (length(predictors) > 1) {

		for (predictor in predictors_tail) {
			predictors_head <- paste(predictors_head, predictor, sep=' + ')	
		}
	}
	formula <- as.formula(paste(response, 
								predictors_head, 
								sep=' ~ ')) 
	return(formula)
}


#Here I am summing all non-diagonal (misclassifications) and
#diving by the total of the validation set in order to get 
# the error rate. 
get_error_rate <- function (predictions, validation_response) {
	confusion_matrix <- table(predictions, validation_response)
	#print(confusion_matrix)
	misclassifications <- 0
	for (row in 1:nrow(confusion_matrix)) {
		for (col in 1:ncol(confusion_matrix)) {
			if(row != col) {
				misclassifications <- misclassifications + confusion_matrix[row, col]
			}
		}
	}
	error_rate <- misclassifications / length(validation_response)
	return(error_rate)
}

#Plots k to find the best k. This is based on week 1 code
plot_best_k <- function (training_data, 
						 validation_data, 
						 response_term, 
						 predictor_terms) {
	K <- seq(1, nrow(validation_data), by=2)
	n = length(K)
	all_error_rate <- numeric(n)
	for (i in 1:n) {
		predictions <- knn(training_data[, predictor_terms],
						   data.frame(validation_data[, predictor_terms]),
						   training_data[, response_term], 
						   k=K[i])
		error_rate <- get_error_rate(predictions, 
									 validation_data[, response_term])
		all_error_rate[i] <- error_rate
	}
	plot(all_error_rate ~ K, main='Error rate vs. K', ylab='Error rate', xlab='K')
}

get_best_k <- function (training_data, 
						 validation_data, 
						 response_term, 
						 predictor_terms) {
	K <- seq(1, nrow(validation_data), by=2)
	n = length(K)
	all_error_rate <- numeric(n)
	for (i in 1:n) {
		predictions <- knn(training_data[, predictor_terms],
						   data.frame(validation_data[, predictor_terms]),
						   training_data[, response_term], 
						   k=K[i])
		error_rate <- get_error_rate(predictions, 
									 validation_data[, response_term])
		all_error_rate[i] <- error_rate
	}
        best_k <- min(K[which(all_error_rate == min(all_error_rate))])
	return(best_k)
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


#This is a function that wraps the cv function we have performed. 
#Additionally, I use an if statement so one can choose between
#knn and random forests.
perform_cross_validation <- function (data,
									  response_term,
									  predictor_terms,
									  model_type,
									  k=10, 
									  seed=8) {

	#make_formula is defined about and pastes the response
	#and predictor terms together.
	formula <- make_formula(response_term, predictor_terms)

	cv_groups <- get_cv_groups(nrow(data), k, seed)

	response <- data[ , response_term]
	factor_levels <- levels(response)
	all_predicted_cv <- factor(rep(NA, nrow(data)), levels=factor_levels)


	for (i in 1:k) {
		training_data <- data[which(cv_groups != i), ]
		validation_data <- data[which(cv_groups == i), ]

		if (model_type == 'knn') {
			model <- knn(training_data[, predictor_terms],
						 data.frame(validation_data[, predictor_terms]),
						 cl=training_data[, response_term],
						 k=7) #7 looked optimal based on the plot_best_k function
			predicted_cv <- model
		}
		if (model_type == 'random_forest') {
			p <- floor(sqrt(length(predictor_terms)))
			model <- randomForest(formula, 
								  data=training_data, 
								  mtry=p,
								  importance=TRUE)
			predicted_cv <- predict(model, validation_data, type = 'class') 
		}
		all_predicted_cv[cv_groups == i] <- predicted_cv
	}
	return(all_predicted_cv)
}


#I found a function called permutations in the gtools. I use this
#to find all permutations of a predictor variables. I then perfom
#cross validation on all of the results to find the best one. 
get_best_predictors <- function(data, 
								response_term, 
								predictor_terms, 
								model_type) {
	best_cv <- Inf
	best_predictors <- NA
	n <- length(predictor_terms)
	for (i in 1:n) {
		#This takes awhile to run!
		predictor_matrix <- permutations(n, i, predictor_terms)
		for (i in 1:nrow(predictor_matrix)) {
			predictor_row <- predictor_matrix[i, ]
			cv_predictions <- perform_cross_validation(data, 
													   response_term, 
													   predictor_row, 
													   model_type)
			cv <- get_error_rate(cv_predictions, data[, response_term])
			if (cv < best_cv){
				best_cv <- cv
				best_predictors <- predictor_row
			}
		}
	}
	return(list(best_predictors, best_cv))
}
