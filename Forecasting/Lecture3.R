library(fpp2)

beer <- window(ausbeer, start = 1992)
fc <- snaive(beer)
autoplot(fc)

checkresiduals(fc)

# mbeer <- fc$mean
# autoplot(mbeer)
# NOTE: 1. mean is around 0 
summary(residuals(fc))
# 2. Very correlated and autocorrealted due to ACF 
