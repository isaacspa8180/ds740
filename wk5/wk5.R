library(glmnet)


df <- read.csv('Trees.csv')
x <- model.matrix(Volume ~ . - 1, data=df)[, -1]
y <- df$Volume
lambdalist = 1:1000/1000
rr_fit <- glmnet(x, y, lambda=lambdalist, alpha=0)

#fit <- lm(Volume ~ ., data=df)
fit <- lm(y ~ x)

lamdbalist_2 <- 1:100/100
lasso_fit <- glmnet(x, y, lambda=lamdbalist_2, alpha=1)
coef(lasso_fit, s=0.10)
