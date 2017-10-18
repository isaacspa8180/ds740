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

#12
library(ISLR)
log_predictors <- with(College, data.frame(log.Enroll=log(Enroll), 
                                           log.Apps=log(Apps), 
                                           log.Accept=log(Accept), 
                                           log.F.Undergrad=log(F.Undergrad), 
                                           log.P.Undergrad=log(P.Undergrad)))

college_transformed <- cbind(College, log_predictors)
college_transformed[, c('Enroll', 
                        'Apps', 
                        'Accept', 
                        'F.Undergrad', 
                        'P.Undergrad')] <- NULL

with(college_transformed, hist(log.Enroll))

cor_cols <- c('Expend', 
              'log.Accept', 
              'log.P.Undergrad', 
              'perc.alumni', 
              'Personal')
cors <- with(college_transformed, cor(log.Enroll, college_transformed[, cor_cols]))

#16
fit <- lm(log.Enroll ~ ., data=college_transformed)
vif(fit)

#17
library(glmnet)

 
lambdalist = 1:1000/1000
x = model.matrix(log.Enroll ~ ., data=college_transformed)[,-1]
y = college_transformed$log.Enroll

lasso_fit <-  glmnet(x, y, lambda=lambdalist, alpha=1)
coef(lasso_fit, s=0.02)
coef(lasso_fit, s=0.03)
coef(lasso_fit, s=0.05)
coef(lasso_fit, s=0.8)

#22
n = nrow(college_transformed)
ncv = 10
groups = c(rep(1:10,77),(1:7))
set.seed(5)
cvgroups = sample(groups,777)

lambdalist = 1:1000/1000
x = model.matrix(log.Enroll ~ ., data=college_transformed)[,-1]
y = college_transformed$log.Enroll

cv_enet <- cv.glmnet(x, 
                     y, 
                     lambda=lambdalist, 
                     alpha=0.75, 
                     nfolds=ncv, 
                     foldid=cvgroups)
min(cv_enet$cvm)
order(cv_enet$cvm)[1]
cv_enet$lambda[order(cv_enet$cvm)[1]]
plot(cv_enet$lambda, 
     cv_enet$cvm, 
     type='l', 
     lwd=2, 
     col='red', 
     xlab='lambda', 
     ylab='CV(10)')
