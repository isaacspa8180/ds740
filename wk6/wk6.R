library(smss)


data(crime2005)

#1
fit <- lm(VI2 ~ ME + PO, data=crime2005)

#2
plot(fit)
crime2005[c(8,40, 51),]

#3
fit.w <- lm(VI2 ~ ME + PO, data=crime2005)
oldcoef <- rep(0, length(fit.w$coef))
newcoef <- fit.w$coef
iter <- 0
while(sum(abs(oldcoef - newcoef)) > .0001 & iter < 100){
    MAR = median(abs(fit.w$residuals))
    k <- 1.345 * MAR / 0.6745
    w <- pmax(1 - (k / abs(fit.w$residuals)), 0)
    fit.w <- lm(VI2 ~ ME + PO, data=crime2005, weights=w)

    iter <- iter + 1
    oldcoef <- newcoef
    newcoef <- fit.w$coef
}

#5
library(MASS)
fit_tukey <- rlm(VI2 ~ ME + PO, data=crime2005, psi = psi.bisquare)

#8
plot(fit_tukey$w)

#9
df_with_na <- read.csv('elnino.csv')
df <- na.omit(df_with_na)

formula <- (air.temp ~ zon.winds + mer.winds + humidity + s.s.temp)
fit_lm <- lm(formula, data=df)

#10
jpeg('appropriateness_linear_model.jpg')
par(mfrow=c(2, 1))
plot(fit_lm, which=c(1, 2))
dev.off()

#11
library(nlme)

fit_gls <- gls(formula, data=df)
buoy_3_gls_residuals <- fit_gls$residuals[which(df$buoy == 3)]

plot(buoy_3_gls_residuals[-length(buoy_3_gls_residuals)], 
     buoy_3_gls_residuals[-1])

fit_gls_2 <- gls(formula,
                 data=df,
                 correlation=corAR1(form=~1|buoy))
