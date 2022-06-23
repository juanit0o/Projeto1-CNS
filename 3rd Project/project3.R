x = c(1981:1993); length(x)
y = c(12, 14, 33, 50, 67, 74, 123, 141, 165, 204, 253, 246, 240); length(y)

#https://www.datascienceblog.net/post/machine-learning/interpreting_generalized_linear_models/
#https://towardsdatascience.com/diagnose-the-generalized-linear-models-66ad01128261
#https://www.theanalysisfactor.com/r-glm-model-fit/
dataMatrix = as.data.frame(matrix(c(x,y), 13,2))
fit1 = glm(y~x, family=poisson(link = "log"), data=dataMatrix)
summary(fit1)

#report model fit
#residual deviance
#aic
#plot de the residual plots for the fitted model
res = resid(fit1)
plot(fitted(fit1), res)
abline(0,0)
#como os pontos nao estao nada proximos da reta de 0 e variam muito no grafico
#isto representa que o fit do modelo nao � apropriado
#comment on model fit adequacy

##############
fit2 = glm(y~x+I(x^2), family=poisson, data = dataMatrix)
summary(fit2)
res2 = resid(fit2)
plot(fitted(fit2), res2)
abline(0,0)

anova(fit1,fit2)
#  Resid. Df Resid. Dev Df Deviance
#1        11     80.686
#2        11     81.044  0 -0.35735

#o modelo que tem menor variancia é o primeiro por isso deve ser escolhido yadda yadda
#faz melhor fit


summary(fit2)
confint(fit2)
plot(x,y)

#predicts the future values
#https://statisticsglobe.com/plot-predicted-vs-actual-values-in-r
#next 100 years

###### pequeno prazo
#allYears = c(1981:2000)
#next100years = data.frame(x=seq(1993,1999, by=1))
#newPredictions = predict(fit2, type="response", newdata = next100years)
#allPredictions = c(y, newPredictions)
#plot(allYears, allPredictions )


###### 100 anos
allYears = c(1981:2094)
next100years = data.frame(x=seq(1993,2093, by=1))
next100yearsVec = c(1994:2094)

newPredictions = predict(fit2, type="response", newdata = next100years)
allPredictions = c(y, newPredictions)
plot(allYears, allPredictions)

lines(next100years$x,newPredictions )


#allYears = allYears, allPredictions = allPredictions
dataToPlot = data.frame(allYears = allYears, allPredictions = allPredictions)
library(ggplot2)

#x =allYears, y = allPredictions
ggplot(dataToPlot, aes(x =allYears, y = allPredictions)) +
  geom_smooth(method = glm, color = "black", se = FALSE, formula = y~x+I(x^2)) +
  stat_smooth(aes(ymin = after_stat(y - 2 * se), ymax = after_stat(y + 2 * se)), geom = "ribbon", method = glm, formula = y~x+I(x^2), fill = "grey60", alpha = .4) +
  theme_bw() + geom_point()


############
###### preexisting values
###TODO ver se é para fazer isto ou prever 100 anos para a frente
#https://pdfs.semanticscholar.org/d510/26c6e9fac622eaacd79a222d73cbd7ba398e.pdf

year = seq(1,13,length=100)
belg.aids <- data.frame(cases=c(12,14,33,50,67,74,123,
                                141,165,204,253,246,240),year=1:13)
fit2Aux = glm(cases~year+I(year^2), family=poisson(link=log), data = belg.aids)
predsAux = predict(fit2Aux, newdata=data.frame(year=year), se=TRUE, type = "response")
plot(belg.aids$year+1980, belg.aids$cases)
lines(year+1980, (predsAux$fit), col=4)
lines(year+1980, (predsAux$fit+2*predsAux$se.fit), col=1)
lines(year+1980, (predsAux$fit-2*predsAux$se.fit), col=1)



