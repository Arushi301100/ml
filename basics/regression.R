#linear regression

mtcars
plot(mtcars$wt, mtcars$mpg)
abline(mtcars$wt, mtcars$mpg)
?abline
model = lm(mpg ~ wt, data = mtcars)
summary(model)
plot(mtcars$wt, mtcars$mpg)
abline(model)

predict(model, newdata = data.frame(wt=2.5))
