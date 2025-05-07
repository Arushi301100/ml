
women

model1 <- lm(weight ~ height, data = women)
?summary(model1)
residuals(model1)
newdata1 = data.frame(height = 66.5)
predict(model1, newdata = newdata1)
plot(x=women$height, y=women$weight)
plot(model1)
mtcars

model2 = lm(mpg ~ wt + hp, data = mtcars)
summary(model2)
#y = mx + c
#mpg = -3.8 * wt +  -.03 * hp + 37.22
predict(model2, newdata = data.frame(wt=2.9 , hp=120))


airquality
