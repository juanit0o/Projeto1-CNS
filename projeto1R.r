set.seed(123)

#Função pdf do enunciado
truncatedParetoPDF = function(x, alpha, L, H){
  (alpha*L^(alpha)*x^(-alpha-1))/(1-(L/H)^alpha)
}

funcIntegratedCDF = function(x, alpha, L, H){
  (1-(L^alpha)*x^-alpha)/(1-(L/H)^alpha)
}

inverseCDF = function(u, alpha, L, H){
  ((u*(1-(L/H)^alpha)-1)/(-L^alpha)) ^(1/-alpha)
}

#integrar de 0 a 1, 3x^2
sim.IT = function(m, alpha, L, H){
  x = vector()
  for(i in 1:m){
    u=runif(1,0,1)
    inversa = inverseCDF(u, alpha, L, H)
    #ir adicionando a inversa do resultado da integral
    x = c(x, inversa)
  }
  x
}

head(sim.IT(10000,0.5,2,4),10)
#Fazer o plot do histograma das observacoes geradas pelo metodo sim.IT
hist(observacoesSim.IT, freq=F,col="grey")


#Gerar o histograma
hist(sim.IT(10000,0.5,2,4), freq=F,col="grey", ylim=c(0,1), cex.main=1.1, ylab="Densidade",
     xlab="x", cex.lab=1.1, main="Histograma da sim.IT(m=10000, alpha=0.5, L=2, H=4)")

#Para adicionar a curva da p.d.f sobreposta ao histograma gerado pela simulação
curve(truncatedParetoPDF(x=x, alpha=0.5, L=2, H=4), add = T, col="red", lwd=4, lty=1)

#O método da transformada inversa serve para gerar variáveis aleatorias não uniformes com uma distribuição
#de probabilidade F desejada. A partir disto, podemos usar esta técnica para fazer 
#"random sampling" a partir dessa variável que gerámos.
#A metodologia para concretizar este metodo é começar por, usando a funcao pdf dada, calcular a respetiva cdf 
#(calculada a partir da integral cujo limite inferior é 0 e o superior é X) e de seguida calcular a inversa. 


#funcao h = f/g
#nossa funcao g vai ser a funcao uniforme y=1
hFunc = function(x, alpha, L, H){
  ((alpha*(L^alpha)*x^(-alpha-1)) / (1-(L/H)^alpha) ) / 1
}


#Para confirmar que o maximo da funcao calculado corresponde ao 
#correto usamos a funcao optimize
maximoH = optimize(hFunc, c(2,4), maximum = T, alpha= 0.5, L = 2,H = 4)$objective
maximoH
#maximoH = 0.85352

#multiplicar por x e dividir por x, senao dá erro a dizer que nao é uma funcao
candidataVezesM = function(x, M){
  M*1*x/x
}

#Como a funcao candidata usada é a uniforme, fazendo o plot da h (que resulta da divisao da f/g) a
#curva resultante sobrepõe a funcao f
curve(hFunc(x = x, alpha = 0.5, L=2, H=4), col="blue", xlim=c(2,4), ylim=c(0,1), main="Função h(x) e f(x)", ylab = "y")
curve(truncatedParetoPDF(x=x, alpha=0.5, L=2, H=4), add = T, col="red", lwd=2, lty=2)

#funcao vai retornar dois vetores para as coordenadas (vetor para x e para y) dos pontos debaixo da curva (para se aceitarem)
# mais dois vetores para as coordenadas (vetor para x e para y) dos pontos acima da nossa funcao (para serem rejeitados)
sim.AR = function(n, alpha, L, H){
  x <- rej_x <- yx <- yrej_x <- vector()
  M <- optimize(hFunc, c(2,4), maximum = T, alpha= alpha, L = L,H = H)$objective
  for(i in 1:n){
    u=1
    a=0
    while(u>a){
      #Gerar uma observação da distrib candidata no intervalo desejado
      x.c <- runif(1,L,H)
      a <- (1/M)*truncatedParetoPDF(x.c, alpha, L, H)
      u <- runif(1,0,1)
      #Caso a observaçao seja superior ao alpha calculado adicionar às obsv de rejeição
      if(u>a){
        rej_x <- c(rej_x,x.c); 
        yrej_x <- c(yrej_x,u*candidataVezesM(x.c, M))
      }
    }
    #Adicionar no caso das observações bem sucedidas
    x <- c(x,x.c);
    yx <- c(yx,u*candidataVezesM(x.c, M))
  }
  #Retornar as listas para os pontos x e y tanto rejeitados como aceites
  return(list(x=x,rej_x=rej_x,yx=yx,yrej_x=yrej_x))
}

simulAR10000 = sim.AR(10000, alpha = 0.5, L = 2, H = 4)
first10.AR = (simulAR10000$x)[1:10]
first10.AR

#fazer o histograma com os valores gerados a partir do algoritmo AR
hist(simulAR10000$x, freq=F,col="grey",xlab="x", ylim=c(0,1), main="Histograma da simul.AR (m=10k samples)")

curve(truncatedParetoPDF(x, alpha = 0.5, L = 2, H = 4),lwd=3,lty=1,ylab = "u*M*g(x)",
      main="trunc N(0,1)",cex.axis=1,
      col="black",cex.main=1,cex.lab=1,
      ylim=c(0,1.5),xlim=c(2,4), add =T, xlab="x" )

#fazer a simulacao para 15 pontos
simulAR = sim.AR(15, alpha = 0.5, L = 2, H = 4)


#fazer o plot da curva candidata ja multiplicada pelo M para ficar acima da nossa funcao f
curve(truncatedParetoPDF(x, alpha = 0.5, L = 2, H = 4),lwd=3,lty=1,ylab = "u*M*g(x)",
      main="sim.AR U(L,H)",cex.axis=1,
      col="black",cex.main=1,cex.lab=1,
      ylim=c(0,1.5),xlim=c(2,4))

M <- 0.85352
curve(candidataVezesM(x=x, M),add=T,lwd=2,col=4)
#fazer o plot dos pontos que foram aceites (debaixo da curva da nossa funcao)
points(simulAR$x,simulAR$yx,pch=4,cex=1,lwd=1.5)

#fazer o plot dos pontos que foram rejeitados (Acima da curva da nossa funcao)
points(simulAR$rej_x,simulAR$yrej_x,col=2,pch=4,cex=1,lwd=1.5)

#por uma borda mais bonita no grafico
box(lwd=2)

legend(3.5, 1.3, legend=c("f(x)", "Mg(x)"),
       col=c("black", "blue"), lty=1,
       cex=0.8,lwd=1.5)


#Calcular a taxa de rejeicao, taxa de pontos que "falharam o alvo", ou seja,
#ficaram fora da nossa zona de aceitao (acertaram acima da curva da funcao)
#nr de pontos rejeitados a divir pelo nr total de pontos
rej.rate <- length(simulAR10000$rej_x)/(length(simulAR10000$rej_x)+length(simulAR10000$x))
rej.rate
#para 100%
percentagemErro = rej.rate *100
cat(round(percentagemErro,4),"%")

#Compare times of sim.IT and sim.AR
timeToProcessIT <- proc.time()
simIT50000 = sim.IT(50000, 0.5, 2,4)
print(proc.time() - timeToProcessIT)

timeToProcessAR <- proc.time()
simAR50000 = sim.AR(50000, 0.5, 2,4)
print(proc.time() - timeToProcessAR)


############# Exc 2
set.seed(654)

#Código para gerar m observções aleatórias da distribuição Normal
sim.exp = function(m,lambda){
  x<-vector()
  for(i in 1:m){
    u=runif(1,0,1)
    x = c(x,-log(u)/lambda)
  }
  x
}

#Código para gerar amostra aleatória de m observações de uma distribuição Gammma
sim.gam = function(m, alpha, theta){
  x = vector()
  for(i in 1:m){
    somatorio = 0
    for(j in 1:alpha){
      obsExp = sim.exp(1,1)
      somatorio = somatorio + obsExp
    }
    res = theta*somatorio
    x = c(x,res)
  }
  x
}
# mostra-nos as 20 primeiras observações 
simulacoesGamma10k = head(sim.gam(10000,2,1), 20) 

#definição da função gamma(p.d.f)
functionExpGammaPDF = function(x, alpha, theta){
  ((theta^(-alpha))/gamma(alpha)) * exp(-(x/theta))*x^(alpha-1) 
}

#curva
curve(functionExpGammaPDF(x=x, alpha=2, theta=1), ylim=c(0,0.6), xlim=c(0,10), add=F, ylab="Densidade")
#histograma
hist(sim.gam(10000,2,1),main="10000 Observations - Gamma(2,1)", freq=F, add=T, breaks = 50 )

########## Monte Carlo #############
funcaoToIntegrate = function(x){
  exp(-x)/(1+x^2)
}
integrate(funcaoToIntegrate, 0, 1)

m=10000

# Gerar uma amostra m observações da Uniforme (0,1)
obsNorm = runif(m, 0,1)   

#Calculo da estimativa do integral a partir de uma amostra aleatória 
integralMC = (1-0) * mean(funcaoToIntegrate(obsNorm))
#0.5245013

varianceIntegr = var(funcaoToIntegrate(obsNorm))/(m*(1-0)^2)
varianceIntegr

####### Antithetic Variables ######

x=runif((m/2),0,1)
I.hat1=mean(funcaoToIntegrate(x))
I.hat2=mean(funcaoToIntegrate(1-x))
I.hat.a =(I.hat1+I.hat2)/2;
# Icont
I.hat.a

V.a <- 1/m*(1+cor(funcaoToIntegrate(x),funcaoToIntegrate(1-x)))*var(funcaoToIntegrate(x)); V.a

#Percentage variance reduction Icont vs Imc
percentageVRIAnti = 100*(1-V.a/varianceIntegr)
percentageVRIAnti
#97.36925%

##Control Variables
g = function(x){exp(x)}
u1=runif(m)
cast= -(cov(u1,funcaoToIntegrate(u1)))/var(u1)
cast
# control-based MC
#nr observacoes
m=10000
#variavel de controlo
h <- function(x){x}
x=runif(m,0,1)

# Icont
I.hat.c.mc = mean(funcaoToIntegrate(x)+cast*(h(x)-0.5))
I.hat.c.mc

V.c.mc <- var(funcaoToIntegrate(x))*(1-cor(funcaoToIntegrate(x),h(x))^2)/m
V.c.mc

#Percentage variance reduction Icont vs Imc
percentageVRICont = 100*(1-V.c.mc/varianceIntegr)
percentageVRICont

########### Importance Sampling ########
f = function(x){1}
w = function (x){abs(funcaoToIntegrate(x))*f(x)}
plot(w,lwd=2, ylim=c(0,2))
plot(w,lwd=2, ylim=c(0,2.5))
currentColor = 2
for(t in seq(-10, 0, 1)){
  phi = function(x){t*exp(t*x)/(exp(t)-1)}
  curve(phi,add= T, col = currentColor)
  currentColor = currentColor + 1
}

legend(0.85, 2.55, legend=c("|g(x)|f(x)", paste0("t=",seq(-10,0,1))),col=1:8,lwd=c(2, rep(1,8)), cex=0.7)

#com t = -1
phi = function(x){-1 * exp(-1 * x)/(exp(-1)-1)}

plot(w,xlab="x",ylab="",ylim=c(0,2),lwd=2)
curve(phi,add=T,col=4, lwd = 3)
legend(0.8, 2, legend=c("|g|*f", "phi"), col=c("black", "blue"), lty=1)

m = 10000
u = runif(m, 0,1)
x = -log(u*exp(-1)-u+1)
h= function(x){funcaoToIntegrate(x)*f(x)/phi(x)}

I.IS = mean(h(x))
I.IS
var.I.IS = var(h(x))/m
var.I.IS
percentageVRIImp = 100*(1-var.I.IS/varianceIntegr)
percentageVRIImp

varianceIntegr
V.a
percentageVRIAnti
V.c.mc
percentageVRICont
var.I.IS
percentageVRIImp

#intervalo de confianca sem contaminacao
#ou seja, todas as n=20 sao observ. provem de X~N(0,1)

#uma vez que sigma e conhecido, o nosso i.c. sera na forma 
#x_bar +- z_{a/2}*(sigma/sqrt(n))

#fixar a semente e alguns parametros
set.seed(569)
n=20; m=10000; mu=0; sigma=1; alpha=0.05; z=1.96 #explicar z
ci_lower = ci_upper = numeric(m)

#gerar amostras aleatorias e 
#determinar os limites inferior e superior do i.c.
for (i in 1:m) {
  x = rnorm(n,mu,sigma)
  x_bar = mean(x)
  ci_lower[i] = x_bar-z*(sigma/sqrt(n))
  ci_upper[i] = x_bar+z*(sigma/sqrt(n))
}

#nivel de confian�a emp�rico
nce = mean(ci_lower<=mu & ci_upper>=mu); 
nce    #0.943


#analisar se o nce diverge significativamente dos 95% de "cobertura"
binom.test(nce*m,m,p=0.95)$p.value
#0.3091904

#estimativa do SE do estimador de monte carlo
sqrt(nce*(1-nce)/m)
#0.007331507

#intervalo de confianca com contaminacao
#ou seja, 90% das n=20 observ. provem de X~N(0,1) e 10% de X~N(k,1)

#k=1
n1=18; n2=2 #90% e 10% das observaoes respet.
ci_lower = ci_upper = numeric(m)

#gerar amostras aleatorias e 
#determinar os limites inferior e superior do i.c.
for (i in 1:m) {
  x1 = rnorm(n1,mu,sigma)
  x2 = rnorm(n2,1,sigma)   #parte da amostra contaminada
  x <- c(x1,x2)            #amostra com contaminacao
  x_bar = mean(x)
  ci_lower[i] = x_bar-z*(sigma/sqrt(n))
  ci_upper[i] = x_bar+z*(sigma/sqrt(n))
}

#nivel de confianca empirico
nce = mean(ci_lower<=mu & ci_upper>=mu); 
nce  #0.921

#analisar se o nce diverge significativamente dos 95% de "cobertura"
binom.test(nce*m,m,p=0.95)$p.value
#8.267447e-05


#estimativa do SE do estimador de monte carlo
sqrt(nce*(1-nce)/m)
#0.008529889

############ k=5
ci_lower = ci_upper = numeric(m)

#gerar amostras aleatorias e 
#determinar os limites inferior e superior do i.c.
for (i in 1:m) {
  x1 = rnorm(n1,mu,sigma)
  x2 = rnorm(n2,5,sigma)   #parte da amostra contaminada
  x <- c(x1,x2)            #amostra com contaminacao
  x_bar = mean(x)
  ci_lower[i] = x_bar-z*(sigma/sqrt(n))
  ci_upper[i] = x_bar+z*(sigma/sqrt(n))
}

#nivel de confianca empirico
nce = mean(ci_lower<=mu & ci_upper>=mu); 
nce  #0.381

#analisar se o nce diverge significativamente dos 95% de "cobertura"
binom.test(nce*m,m,p=0.95)$p.value
#0

#estimativa do SE do estimador de monte carlo
sqrt(nce*(1-nce)/m)
#0.01535705


######### k=9
ci_lower = ci_upper = numeric(m)

#gerar amostras aleatorias e 
#determinar os limites inferior e superior do i.c.
for (i in 1:m) {
  x1 = rnorm(n1,mu,sigma)
  x2 = rnorm(n2,9,sigma)   #parte da amostra contaminada
  x <- c(x1,x2)            #amostra com contaminacao
  x_bar = mean(x)
  ci_lower[i] = x_bar-z*(sigma/sqrt(n))
  ci_upper[i] = x_bar+z*(sigma/sqrt(n))
}

#nivel de confianca empirico
nce = mean(ci_lower<=mu & ci_upper>=mu); 
nce  #0.015

#analisar se o nce diverge significativamente dos 95% de "cobertura"
binom.test(nce*m,m,p=0.95)$p.value
#0


#estimativa do SE do estimador de monte carlo
sqrt(nce*(1-nce)/m)
#0.003843826