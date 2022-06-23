set.seed(1234)

#Exc 2


############### a) ###########

B= 10000 # samples
x=c(1766,884,2420,695,1825,1014,2183,2586,627,965,1577,2195,1354,1325,1552,1299,71,3725,1354,159)
#Teste á normalidade dos dados
hist(x)

obsvOrdered = x[order(x)]
obsvOrdered

probsI = (1:length(obsvOrdered)-0.5)/length(obsvOrdered)
quantisI = qnorm(probsI)
par(pty="s")
library(car)
qqp(obsvOrdered, distribution="norm")

shapiro.test(x)$p.value
#Não rejeitamos a hípotese nula de que a população segue uma distribuição normalidade

#Teste Exacto
t.test(x,mu=1020,alternative='greater',conf=0.90)

mean(x)

n=length(x)
#computing the observed value of the test statistic
mu0 = 1020; sd0 = sd(x); t.obs = (mean(x)-mu0)/(sd0/sqrt(n))
# non-parametric bootstrap
B=5000; t.star=numeric(B)
z=x-mean(x)+mu0 # this transformation imposes H0 on Fhat
for(i in 1:B){
  z.star = sample(z,n,replace=T)
  sd.z.star = sd(z.star)
  t.star[i] = (mean(z.star)-mu0)/(sd.z.star/sqrt(n))
}
# computing the p.value of the test
p.value <- sum(t.star>t.obs)/B; p.value

############### b) ###########


t=mean(x)
t.star = numeric(B)
for(i in 1:B){ x.boot = sample(x,length(x),replace = T)
t.star[i] = mean(x.boot)}
# the bootstrap estimate of the mean is
t.boot = mean(t.star);
# computing the basic/pivotal 90% bootstrap CI
deltastar = t.star - t
d = quantile(deltastar, c(0.05,0.95))
ci = t - c(d[2], d[1])
names(ci) <- c("5%","95%");ci

# computing the percentile 90% bootstrap CI
d = quantile(t.star, c(0.05,0.95));d

hist(t.star)#Histograma de as medias obtidas pelo Bootrstaping para a população

############### c) ###########

library(boot);

TT <- function(data,indices){return(mean(data[indices,]))}
boot.mean <- boot(data=as.data.frame(x),statistic = TT,R=B)
boot.ci(boot.out = boot.mean,conf=0.90, type = "bca",)#Função incorporada no Rstudio

z0 <- qnorm(mean(t.star<t.boot))
alpha=0.1
u <- c(alpha/2, 1-alpha/2)
zu <- qnorm(u)
n = length(x)
# preparing for jackknifing
t1 = mean(x); t.star1 = numeric(n)
for(i in 1:n){ t.star1[i] = mean(x[-i]) }
# computing the estimates
t.jack = mean(t.star1)
uu<-(n-1)*t.jack-t.star1
acc<-sum(uu*uu*uu)/(6*(sum(uu*uu))^1.5)
tt <- pnorm(z0+ (z0+zu)/(1-acc*(z0+zu)))
confpoints <- quantile(x=t.star,probs=tt,type=1);confpoints

############### d) ###########

estimador_P=mean(x>1100);estimador_P

#standard erro
var_p<-var(x>1100)/length(x)
Standard_error=sqrt(var_p);Standard_error


############### e) ###########


# fixing the seed and number of bootstrap samples
set.seed(1234); B=10000
# bootstraping B samples from the observed sample and computing the bootstrap mean for each
t.star = numeric(B)
for(i in 1:B){ x.boot = sample(x,length(x),replace = T)
t.star[i] = mean(x.boot>1100) }
# the bootstrap estimate of the mean is
t.boot = mean(t.star)
# estimating the SE of the sample mean estimator
se.T.boot = sqrt(var(t.star))
# an estimate of the bias of the estimator is computed as
bias.T <- t.boot-estimador_P
#variance of the estimate of P
var.T.boot = var(t.star)
var.T.boot1=(1/(B-1))*sum((t.star-t.boot)^2)

boot.res = cbind(t.boot,bias.T,se.T.boot)

######################## jackknife ######################

# inputing the data
n = length(x)
# preparing for jackknifing
t.star_jack = numeric(n)
for(i in 1:n){ t.star_jack[i] = mean(x[-i]>1100) }
# computing the estimates
t.jack = mean(t.star_jack)
se.jack = sqrt((n-1)*mean((t.star_jack-t.jack)^2))
bias.jack = (n-1)*(t.jack-estimador_P)
# displaying the results
jack.res=cbind(t.jack,bias.jack,se.jack)

results <- rbind(jack.res,boot.res)
colnames(results) <- c("estimate","bias","SE")
rownames(results) <- c("jackknife","bootstrap"); results

############### f) ###########

B=10000
n<-20
t.star2 = numeric(B)
for(i in 1:B){ x.boot2 = sample(x,length(x),replace = T)
t.star2[i] = mean(x.boot2>1100)}

t.star_jack = numeric(n)
for(i in 1:n){ t.star_jack[i] = sqrt(var(t.star2[-i]))}
sd(t.star2)

sqrt((n-1)*mean((t.star_jack-mean(t.star_jack))^2))


#Exc 3
############### a) ###########
#Criacao de dataframe com dados
x = c(34.00, 28.00, 31.00, 28.00, 30.0, 27.0, 32.0, 25.0, 34.0, 34.00, 29.0, 26.00, 24, 33.00)
y = c(23.44, 7.95, 17.04, 9.57, 16.9, 9.3, 16.2, 3.2, 24, 19.02, 11.2, 7.32, 3, 18.63)
dataframe <- data.frame(x, y)
#validacao de que existe linearidade

#representar dados graficamente
plot(x,y)

#correlacao entre variaveis
cor(x, y)

#ajustar o modelo de reg linear aos nossos dados
fit = lm(y~x, dataframe)

# linha de regress ajustada
abline(fit,col=4,lwd=1.5)

#resumo de dados deste fit
summary(fit)

#parametros fixos beta0 e beta1
betas = fit$coefficients;betas

sigma = summary(fit)$sigma; sigma

r2ajustado = summary(fit)$adj.r.square; r2ajustado

#IC a 90% para o declive
confint(fit, level = 0.90)[2,]


############### b) ###########
# Validação da normalidade dos resíduos
# Gráfico QQ (quantis emprícos vs quantis teóricos)
qqnorm(fit$residuals, main="Gráfico QQ", xlab="Quantis teóricos", 
       ylab="Quantis empíricos",cex.lab=1.5)
qqline(fit$residuals)

# Teste Shapiro wilk (hipotese nula corresponde as amostras serem de uma pop normal)
shapiro.test(fit$residuals)$p.value
# 0.6804204
# Como pvalue > alpha (0.05) Não temos evidência para rejeitar a normalidade dos resíduos

# Validação da homocedasticidade do erro
# Teste de Breush-Pagan (hipotese nula corresponde as variâncias do erros são iguais)
library(lmtest)
bptest(fit)$p.value

# Validação da independência das observações
# Gráfico dos resíduos normalizados vs. valores ajustados
library(MASS)
plot(fit$fitted.values,studres(fit), xlab="Valores ajustados", 
     ylab="Resíduos normalizados",cex.lab=1.5)
abline(h=0,lty=3)


############### c) ###########
#bootrstrap de pares

beta1 = fit$coefficients[2]
B = 10000
n = nrow(dataframe)
#guardar coeficientes beta1 (declive) p cada bootstrap
betaDecs = numeric(B)


for(i in 1:B){
  #amostrar aleatoriamente c replacement n amostras de um vetor com tamanho N
  idx = sample(1:n, n, replace = TRUE)
  
  #selecionar vals de x e y para os indexes obtidos da amostra aleatoria
  xBeta = x[idx]
  yBeta = y[idx]
  
  #ajustar modelo a estes vals de x e y
  fitBeta = lm(yBeta~xBeta)
  
  #guardar coeficiente beta1 (declive) do vetor beta
  betaDecs[i] = fitBeta$coefficients[2]
}

#variancia e vies p beta1
matrixBiasVar = matrix(c(mean(betaDecs), var(betaDecs), mean(betaDecs)-beta1), 1,3)
colnames(matrixBiasVar) = c("Media", "Variancia", "Bias")
rownames(matrixBiasVar) = "beta1"
matrixBiasVar

beta1
#diferenca entre beta1 obtido por bootstrap e beta1 obtido pelo fit do modelo aos dados
difBetas = betaDecs - beta1

#calculo dos quantis associados a estas diferencas
alpha = 0.1 
a = quantile(difBetas, c(alpha/2, 1-alpha/2))

#ic pivotal a 90%
beta1Pivotal = beta1 - c(a[2],a[1])
beta1Pivotal

#comparando c alinea a)
confint(fit, level = 0.90)[2,]

#ambos os intervalos de confianca contem o declive
#o intervalo de confianca pivotal é mais pequeno (está contido no da alinea a)

#d) wild bootstrap
library(lmboot)
wildBoot = wild.boot(formula = y~x, B=10000, data = NULL,  1234)
matrixBiasVarWild = matrix(c(mean(wildBoot$bootEstParam[,2]), var(wildBoot$bootEstParam[,2]), mean(wildBoot$bootEstParam[,2])-beta1), 1,3)
colnames(matrixBiasVarWild) = c("Media", "Variancia", "Bias")
rownames(matrixBiasVarWild) = "beta1"
matrixBiasVarWild

################################ Optimization #################################
## Exc 4

############### a) ###########


#dados do exercício
vector4 = c(1.977866, 1.836622, 1.097168, 1.232889, 1.229526, 2.438342,
            1.551389, 1.300618, 1.068584, 1.183466, 2.179033, 1.535904,
            1.323500, 1.458713, 1.013755, 3.602314, 1.087067, 1.014013,
            1.613929, 2.792161, 1.197081, 1.021430 ,1.111531, 1.131036,1.064926)

#número de amostras
n=length(vector4)

#cálculo do MLE
MLE = function(x){
  res <- sum(log(x))
  return(n/res)
}
mleVar=MLE(vector4)
mleVar

#cálculo do MME
MME = function(x){
  x_bar = mean(x)
  alpha = x_bar/(x_bar-1)
  return(alpha)
}
mmeVar=MME(vector4)
mmeVar

############### b) ###########

#cálculo da variância
VAR = function(alpha, n){
  In=n/(alpha^2)
  return(1/In)
}
VAR(mleVar,n)


############### c) ###########

# Função de verosimilhança
lik <- function(alpha){
  res = vector()
  for(a in alpha) {
    res = c(res, (a^n)*prod(1/(vector4^(a+1))))
    #res = c(res, prod(a*1/(x^(a+1))))
  }
  return(res)
}
# Função log-verosimilhança
loglik <- function(alpha){
  res <- vector()
  for(a in alpha) {
    res <- c(res, n*log(a)-((a + 1) * sum(log(vector4))))
  }
  return(res)
}
# Função score
funScore <- function(alpha) {
  res <- vector()
  for(a in alpha) {
    res <- c(res, (n/a) - sum(log(vector4)))
  }
  return(res)
}

#fazer o plot das 3
#Representação gráfica
par(mfrow=c(1,3))
b <- seq(0.1,4,0.05)

plot(b,lik(b),ylab="verosimilhança", xlab=expression(alpha),lwd=2,type="l",
     cex.lab=1.5)

box(lwd=2)

plot(b,loglik(b),ylab="log-verosimilhança",xlab=expression(alpha),lwd=2,
     type="l",cex.lab=1.5)
box(lwd=2)

plot(b,funScore(b),ylab="score", xlab=expression(alpha),lwd=2,type="l",cex.lab=1.5)
abline(h=0,lty=3); box(lwd=2)

############### d) ###########

library(maxLik)

#dar como start a estimativa inicial de alpha dado pelo metodo dos momentos
#start = maximo da funcao loglikelihood
maxLik(loglik, start = 2.8)
#Assim, a estimativa de max verosimilhanca de alpha obtida usando a funcao do R foi 2.81

############### e) ###########

bisection <- function(x,a,b,eps){
#x: amostras
#a: limite esquerdo do intervalo para o teorema de Bolzano
#b: limite direito do intervalo para o teorema de Bolzano
#eps: valor para regra de paragem
  alpha.it = vector(); alpha.it[1] = (a+b)/2
  k = 1; diff = 1
  diffs= vector()
  while(diff>eps){
    #se a multiplicação for < 0, diminuir b
    if(funScore(alpha.it[k])*funScore(a)<0){
      b = alpha.it[k]
      alpha.it[k+1] = (a+b)/2
    }
    #se a multiplicação for > 0, aumentar a
    else{if(funScore(alpha.it[k])*funScore(a)>0){
      a = alpha.it[k]
      alpha.it[k+1] = (a+b)/2
    #se a multiplicação for = 0, chegamos ao valor e a regra de paragem é ativada
    }else{alpha.it[k+1]=alpha.it[k]}
    }
    #atualização da diferença de iterações para a regra de paragem
    diff = abs(alpha.it[k+1]-alpha.it[k])
    diffs[k]=diff
    k = k+1
  }
  result = as.matrix(alpha.it)
  colnames(result)<-"iterations"
  rownames(result)<-1:length(alpha.it)
  retList<-list("res"=result,"error"=diffs)
  retList
}
#resultado da bisection
l1=bisection(vector4,2,4,0.000001)
m1=t(l1$res)
m1

#erro da bisection
l1$error


#derivada da função score
prime <- function(alpha){
  out = numeric(length(alpha))
  if(length(alpha)==1){out =- n/alpha^2}
  if(length(alpha)!=1){
    for(i in 1:length(alpha)){out[i] = - n/alpha[i]^2}
  }
  return(out)
}

#método Newton-Raphson
NR <- function(x,alpha0,eps){
#x: amostras
#alpha0: primeiro valor do vetor a iterar. i.e., o MME
#eps: valor para regra de paragem
  alpha.it = vector()
  alpha.it[1] = alpha0
  k = 1
  diff = 1
  diffs=vector()
  #iterações
  while(diff>eps){
    alpha.it[k+1] = alpha.it[k]-funScore(alpha.it[k])/prime(alpha.it[k])
    diff = abs(alpha.it[k+1]-alpha.it[k])
    diffs[k]=diff
    k = k+1
  }
  result = as.matrix(alpha.it)
  colnames(result)<-"iterations"
  rownames(result)<-1:length(alpha.it)
  result
  retList<-list("res"=result,"error"=diffs)
  retList
}
#resultado do NR
l2=NR(vector4,mmeVar,0.000001)
m2=t(l2$res)
m2

#erro do NR
l2$error

#método Secant
secant <- function(x,alpha0,alpha1,eps){
#x: amostras
#alpha0: limite esquerdo do intervalo
#alpha1: limite direito do intervalo
#eps: valor para regra de paragem
  alpha.it = vector()
  alpha.it[1] = alpha0
  alpha.it[2] = alpha1
  k = 2
  diff = 1
  diffs = vector()
  diffs[1]=diff
  #iterações
  while(diff>eps){
    alpha.it[k+1] = alpha.it[k]-funScore(alpha.it[k])*((alpha.it[k]-alpha.it[k-1])/(funScore(alpha.it[k])-funScore(alpha.it[k-1])))
    diff = abs(alpha.it[k+1]-alpha.it[k])
    diffs[k]=diff
    k = k+1
  }
  result = as.matrix(alpha.it)
  colnames(result)<-"iterations"
  rownames(result)<-1:length(alpha.it)
  retList<-list("res"=result,"error"=diffs)
  retList
}
#resultado da secant
l3=secant(vector4,2,4,0.000001)
m3=t(l3$res)
m3

#erro da secant
l3$error

############### f) ###########

#Fórmula do I(alpha)
Ialpha<-function(alpha){
  n/(alpha^2)
}

#método Fisher Scoring
fisherScore <- function(x,alpha0,eps){
#x: amostras
#alpha0: primeiro valor para o vetor a iterar, i.e., MME
#eps: valor para regra de paragem
  alpha.it = vector()
  alpha.it[1] = alpha0
  k = 1
  diff = 1
  diffs=vector()
  diffs[1]=diff
  #iterações
  while(diff>eps){
    alpha.it[k+1] = alpha.it[k]+(1/Ialpha(alpha.it[k]))*funScore(alpha.it[k])
    diff = abs(alpha.it[k+1]-alpha.it[k])
    k = k+1
    diffs[k]=diff
  }
  result = as.matrix(alpha.it)
  colnames(result)<-"iterations"
  rownames(result)<-1:length(alpha.it)
  retList<-list("res"=result,"error"=diffs)
  retList
}
#resultado do fisher score
l4=fisherScore(vector4,mmeVar,0.000001)
m4=t(l4$res)
m4

#erro do fisher score
l4$error