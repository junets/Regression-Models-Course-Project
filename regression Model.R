data(mtcars)
names(mtcars)
head(mtcars)
?mtcars

# compare the difference 
## t test
t.test(mtcars[mtcars$am == 0,]$mpg, mtcars[mtcars$am==1,]$mpg)

## boxplot
library(ggplot2)
mtcars$trans = mtcars$am
mtcars[mtcars$am == 0,]$trans = "Auto"
mtcars[mtcars$am == 1,]$trans = "Manual"
ggplot(data = mtcars, aes(factor(trans),mpg)) + 
  geom_boxplot() + 
  labs(x = 'Transmission', y = "MPG", title = "MPG based on Auto and Manual")

mtcars$trans = NULL

pairs(mtcars, panel = panel.smooth)
library(corrgram)
corrgram(mtcars, lower.panel=panel.shade,
         upper.panel=panel.pie, text.panel=panel.txt,
         main="Car Milage Data in PC2/PC1 Order")

library(corrplot)
corrplot(cor(mtcars), method = "ellipse")
for (i in c('cyl', 'vs', 'am', 'gear', 'carb')){
  mtcars[,i] = factor(mtcars[,i])
}

allin = lm(data = mtcars, mpg ~ .)
summary(allin)
library(car)
vif(allin)

modified1 = update(allin, mpg ~ cyl+disp+hp+drat+wt+qsec+vs+am+gear)
summary(modified1)
vif(modified1)
modified2 = update(allin, mpg ~ disp+hp+drat+wt+qsec+vs+am+gear)
summary(modified2)
vif(modified2)
modified3 = update(allin, mpg ~ hp+drat+wt+qsec+vs+am+gear)
summary(modified3)
vif(modified3)
modified4 = update(allin, mpg ~ hp+drat+wt+qsec+vs+am)
summary(modified4)
modified5 = update(allin, mpg ~ hp+drat+wt+qsec+am)
summary(modified5)
modified6 = update(allin, mpg ~ hp+wt+qsec+am)
summary(modified6)
modified7 = update(allin, mpg ~ (wt+qsec)*am)
summary(modified7)
modified8 = update(allin, mpg ~ wt*am+qsec)
summary(modified8)
library(leaps)
check <- regsubsets(mpg ~ ., data = mtcars)
summary(check)
plot(check, scale = "Cp")
abline(h = 8, col = "red", lwd = 3)

ggplot(data = mtcars, aes(wt,mpg,  color=am)) + 
  geom_point()
ggplot(data = mtcars, aes(qsec,mpg,  color=am)) + 
  geom_point()
mean(modified8$residuals)
var(modified8$residuals)
par(mfrow = c(2, 2))
plot(modified8)