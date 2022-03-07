rm(list = ls())
student=import("Regression Project Data.xlsx",sheet="Sheet1")
attach(student)
library(rio)
names(student)
str(student)
student$gender=as.factor(student$gender)
str(student)

## Used as factor command to change the characters in gender into 2 factors
## of 1s and 2s. 1s are for females and 2s are for males. Alphabetical order.

plot(student)

## We can see the relation between our different variables in the plots
## for students. Very similar at the first glance.


student.out.reading=lm(`math score`~`reading score`, data = student)
summary(student.out.reading)
## The summary of the relationship between our dependent variable y (math score)
## and one of our independent variables x1 (reading score), gives us a beta coefficient
## of 0.91 which our p-value indicates that it is in fact significant (This is the slope
## of our line). However, our residual standard error could be better but we have
## to find out if there are models that are better than this one in particular.
## Lastly, even though the R-squared for this model indicates high levels of correlation,
## we have to see if there are other models that can give us higher correlation
## between the data variables.
plot(`reading score`,`math score`,pch=19,main = 'Math vs Reading')
abline(student.out.reading,col='red',lwd=3)
qqnorm(student.out.reading$residuals,pch=19,main="Normality Plot")
qqline(student.out.reading$residuals,lwd=3,col="red")
plot(reading.score,student.out.reading$residuals,
     pch=19,main="Equality of Variances Plot")
abline(0,0,col='red',lwd=3)
## plot, qqnorm, and the residual plot for visual presentation of our analysis.


student.out.writing=lm(`math score`~`writing score`, data = student)
summary(student.out.writing)
## Very similar result to our regression analysis of our dependent variable with
## our reading independent variable. However, based off of numbers of our we can see
## that our slope with writing variable is slightly lower (Beta coefficient) with 
## a very low p-value indicating the significance of the writing variable to math
## variable. Our R-squared number is higher, which tells us there is a higher
## level of correlation between writing and math, but not significantly higher.
## Our residual standard error is also lower than of what we got in the previous one.
plot(`writing score`,`math score`,pch=19,main = 'Math vs Writing')
abline(student.out.writing,col='green',lwd=3)
qqnorm(student.out.writing$residuals,pch=19,main="Normality Plot")
qqline(student.out.writing$residuals,lwd=3,col="green")
plot(writing.score,student.out.reading$residuals,
     pch=19,main="Equality of Variances Plot")
abline(0,0,col='green',lwd=3)
## plot, qqnorm, and the residual plot for visual presentation of our analysis.


student.out.gender=lm(`math score`~`gender`, data = student)
summary(student.out.gender)
## Not a good or our best fit of the model at all. The beta coefficient is greater
## than 1, 1.907. This could be due to the fact that the data for our gender consists
## of 2 levels of 1s and 2s. 2s for the male. With the beta coefficient of 1.907,
## we can assume that for every male gender, the slope of our line is in the positives,
## and maybe indicating that male gender scores higher than female gender in math.
qqnorm(student.out.gender$residuals,pch=19,main="Normality Plot")
qqline(student.out.gender$residuals,lwd=3,col="purple")




student$reading2=reading.score^2
student.out.reading2=lm(`math score`~`reading score`+ reading2, data = student)
summary(student.out.reading2)
## This model and the linear model of reading earlier, indicate that the scores,
## and the best fitted line is more so linear than it is curved. However, this tells
## us that there is a slight curve to the line but nothing significant with the
## support of the p-values from this model.
## The reading score is still of significance when it comes to the possibility
## of correlation between the 2 data. This model could be said is better than the
## linear model since our r-squared value is slightly higher, however, both models
## are not far off from each other and most likely the best model that we can find.
plot(`reading score`,`math score`,pch=19,main = 'Math vs Reading')
points(reading.score,student.out.reading2$fitted.values,col='red',pch=19)
plot(student.out.reading2$fitted.values,student.out.reading2$residuals,pch=19,
     main = 'Equality of Variances')
abline(0,0,col='red',lwd=3)

student$writing2=writing.score^2
student.out.writing2=lm(math.score ~ writing.score + writing2, data = student)
summary(student.out.writing2)
## Very similar outcome for this model as well compared to our previous model.
## The numbers tell us that the line is closer to a linear line in the data, meaning
## that it is very close to a slope (beta coefficient) of 1 which would mean that
## the writing score in relation to the math score, increase at almost the same rate.
## Slightly a better model compared to the previous model but nothing of significance.
## We should look more in detail to find a better model.
## Higher R-squared but not that much higher at all. P-value is lower compared
## to reading+reading2, which could be telling us that increasing or decreasing 
## writing scores have more impact on the math scores.
plot(writing.score, math.score, pch=19, main = 'Math vs Writing')
points(writing.score,student.out.writing2$fitted.values,col='green',pch=19)
plot(student.out.writing2$fitted.values,student.out.writing2$residuals,pch=19,
     main = 'Equality of Variances')
abline(0,0,col='green',lwd=3)

s.out.read.write=lm(math.score~reading.score+writing.score,data = student)
summary(s.out.read.write)
## This model clarifies a lot of our earlier assumptions; writing scores do in
## fact have a higher impact on our math scores. As it can be seen in this model
## regression analysis, it can be seen that the p-value of writing scores is telling
## us that there is a significant impact on the math score. The slope (beta coefficient)
## also clearly shows us that math scores change more with an increase or decrease
## in relation to writing scores.
## This model is better than our previous models since the R-squared value is greater
## than all of the previous ones. Although it is not significantly greater, it is
## still greater with lower residual standard error which indicates that this model
## is better than the previous ones.

s.out.read.gender=lm(math.score~reading.score+gender,data = student)
summary(s.out.read.gender)
## Both our independent and binary variable have a very great impact on the math
## score results. The p-value for both indicate that both are significantly impacting
## the math scores. R-squared value is the highest we have found so far compared
## to the previous models. We also can see the residual standard error also has
## decreased in this model.
## This model is the best one so far, however we need to do more analysis to make sure
## if this model is the best one we can find. There are still more combinations
## that could change this hypothesis.
plot(s.out.read.gender$fitted.values,s.out.read.gender$residuals,pch=19,
     main = 'Equality of Variances')
abline(0,0,col = 'purple', lwd=3)
## The dots in this plot can be seen that are not too widely apart from the 0 line,
## which confirms why our residual standard error is lower than the previous models.

s.out.write.gender=lm(math.score~writing.score+gender,data = student)
summary(s.out.write.gender)
## This model is better than the previous model right off the bat. You can see
## higher beta coefficients with p-values indicating that they are significant.
## The residual standard error is also lower than the previous model, and also the
## R-squared value is greater than the previous model.
## This also gets us closer to our final prediction of which variables greatly affect
## math scores in this data set.
plot(s.out.write.gender$fitted.values,s.out.write.gender$residuals,pch=19,
     main = 'Equality of Variances')
abline(0,0,col = 'purple', lwd=3)
## Lower errors can be seen in the plot. Min and Max of the residuals from the regression
## model can be seen to be the lowest compared to all of them so far.


#### So far we are understanding more that writing scores and male gender have the
#### highest impact on math scores. But we need to run more regression analysis to
#### figure that out more accurately.

s.out.read.write.gender=lm(math.score~reading.score+writing.score+gender,data = student)
summary(s.out.read.write.gender)
## This is also very similar to our previous regression model. Very similar in the numbers as well.
## This model also helps us more to confirm that writing scores and male gender are more of
## significance to the changes in math scores. This model based off numbers is just
## as good as the previous model; however, the fact that reading scores are also
## included in this regression model, this model is definitely of more value to us
## to understand what variables have more impact on math scores.
plot(s.out.read.write.gender$fitted.values,s.out.read.write.gender$residuals,pch=19,
     main = 'Equality of Variances')
abline(0,0,col = 'brown', lwd=3)
## Very similar to the last model. Low Max and High Min compared to the other
## residual data.

s.out.read.write.readwrite=lm(math.score~reading.score+writing.score+I(reading.score*writing.score),data = student)
summary(s.out.read.write.readwrite)
## The interaction of our 2 independent variables and our 2 independent variables separately.
## This model also helps us with understanding of how writing scores impact math scores
## more than reading scores. However, the interaction of the two independent variables
## do not seem to have any significant impact. Which could possibly mean that people
## who score higher on writing and math who are more predominantly of male gender,
## do not score as well in reading.
## R-squared value is not as good as the last 2 models. However, the information
## received from this model is far more important than whether this model is a great
## reperesentation of our data set regression analysis.
plot(s.out.read.write.readwrite$fitted.values,s.out.read.write.readwrite$residuals,pch=19,
     main = 'Equality of Variances')
abline(0,0,col = 'blue', lwd=3)
## Higher Max and Lower Min in the plot.

s.out.no.gender=lm(`math score`~reading.score+writing.score+I(reading.score*writing.score)+reading2+writing2, data = student)
summary(s.out.no.gender)
## Not a bad model, however, it does not really tell us anything in specific with
## any statistical significance. Writing scores still have the lowest p-value and
## the highest beta coefficient in this model. That is good to know.

full.out=lm(math.score~ reading.score+writing.score+gender+reading2+writing2+I(reading.score*writing.score),data = student)
summary(full.out)
## Full out analysis of all our different variables and the interaction between our
## 2 independent variable. Great R-squared value and low residual error compared to 
## the other ones.
## This is our best model for many reasons.
## It can be seen that how math scores are affected by all of our variables (Except itself, duh)
## and the interaction between our 2 independent variable.
## Writing scores and gender male are the ones that are of most significant to the changes
## in math scores. More so gender male.
## R-squared value is also the highest out of all the other models and is very close to
## 0.9 which is what we would like to see in a regression analysis. The residual
## standard error is also the lowest we have seen between all the models.
## This model tells us that our results and take from the simple and multiple regression
## analyses were true.
## People are more likely to score higher in math if they are mainly of male gender,
## and also if they score high in writing. being a male has more impact on the math
## scores achieved by the students (According to the beta coefficients and p-values).
plot(full.out$fitted.values,full.out$residuals,pch=19,
     main = 'Equality of Variances')
abline(0,0,col = 'blue', lwd=3)
## The data points are much closer to our (0,0) horizontal line. Thus, lower residual
## standard error.


# 4-in-1 Plot
par(mfrow=c(2,2))
plot(math.score,full.out$fitted.values,pch=19,
     main="Full, Linearity",
     xlab="Actuals",ylab="Fitted Values")
abline(0,1,col="red",lwd=3)
qqnorm(full.out$residuals,pch=19,
       main="Full, Normality")
qqline(full.out$residuals,lwd=3,col="red")
hist(full.out$residuals,col="black",
     main="Full, Residuals",
     xlab="Residuals",probability=TRUE)
curve(dnorm(x,0,sd(full.out$residuals)),
      from=min(full.out$residuals),
      to=max(full.out$residuals),
      lwd=3,col="red",add=TRUE)
plot(math.score,rstandard(full.out),pch=19,
     main="Full, Equality of Variances",
     xlab="Actuals",ylab="Stdized Residuals")
abline(0,0,col="red",lwd=3)
par(mfrow=c(1,1))
## These are the plots of our best fitted model. Right skewed. 


predict(full.out, interval = "confidence")










