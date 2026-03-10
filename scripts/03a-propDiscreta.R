##
## Modelo binomial:
## Modelo (verossimilhança):
## [Y|\theta] \sim Bin(n, \theta)]
## Priori:
## [\theta] \sim Be(a,b)
## dados: y=19, n=80  

##rm(list=ls())
##
a <- 3; b <- 13
n <- 80; y <- 19
p.hat <- y/n
npred <- 50

##
## Exemplo 1:
##   [Y|\theta] \sim Bin(n, \theta)
##   [\theta] \sum Beta(a, b)

##
## Parte 1: resultados analíticos (revisão)
##
## Posteriori:
## [\theta|y] \sim Be(y+a,(n-y)+b)
## Preditivas:
## - a priori    [Y] \sim BetaBin(n, a, b),
## - posteriori  [Y|y] \sim BetaBin(n, y+a, (n-y)+b)
## - plugin      [Y|y,\hat{theta}] \sim Bin(n, \hat{theta}) 
##

## curvas da priori, verossimilhança e posteriori
par(mfrow = c(1,2), mar = c(3,3,1,0), mgp = c(2,1,0))
curve(dbeta(x,  a, b), n = 501, 0, 1, col=2, ylim=c(0,10), xlab = expression(theta), ylab = "")
curve(dbeta(x, y+1, (n-y)+1), n = 501, 0, 1, , col="blue", add=T)
curve(dbeta(x, a+y, b+(n-y)), n = 501, 0, 1, add=T)
legend("topright",
       c(expression(paste("[", theta, "]")),
         expression(paste("L[",theta,"]")),
         expression(paste("[", theta, "|y]"))),
       lty = 1, col = c("red","blue","black"))

## preditivas a priori ([Y]), posteriori ([Y|y]) e plugin ([Y|y,\hat{theta}])
require(VGAM)  ## para dbetabinom.ab()
Y.yprior <- dbetabinom.ab(0:npred, size = npred, shape1 = 3, shape2 = 13)
Y.ypost <- dbetabinom.ab(0:npred, size = npred, shape1 = 22, shape2 = 74)
Y.yplug <- dbinom(0:npred, size = npred, prob = p.hat)

plot((0:npred), Y.yplug, type = "h", col = "blue", xlab = "y", ylab = "")
lines((0:npred)+0.2, Y.ypost, type = "h")
lines((0:npred)-0.2, Y.yprior, type = "h", col = 2)
legend("topright", c("[Y]",
                     expression(paste("[Y|y,", hat(theta), "]")),
                     "[Y|y]"),
       lty = 1, col = c("red","blue","black"))
mtext(bquote(paste("Preditivas para ", n == .(npred))))
##
## Aproximação discreta 
##
n.d <- 101
th.d <- seq(0, 1, l=n.d)
prior.d <- dbeta(th.d, 3, 13); prior.d <- prior.d/sum(prior.d)
LL.d <- dbinom(19, size=n, th.d); LL.d <- LL.d/sum(LL.d)
post.d <- prior.d*LL.d ; post.d <- post.d/sum(post.d)

par(mfrow=c(1,2))
plot(th.d+0.01, post.d, type="h", col = 1, xlab = expression(theta), ylab = "")
lines(th.d, LL.d, type = "h", col = adjustcolor("blue", alpha.f = 0.5))
lines(th.d-0.01, prior.d, type = "h", col = adjustcolor("red", alpha.f = 0.5))

plot(th.d, post.d, type="l", col = 1)
lines(th.d, LL.d, type = "l", col = "blue")
lines(th.d, prior.d, type = "l", col = 2)
par(mfrow=c(1,1))

## Solução (exata) anterior considera distribuição contínua (densidade) para  $\theta$
## e aproximada por grid obtem distribuição discreta  para  $\theta$
## Para permitir a sobreposição dos gráficos para avaliar aproximação multiplicamos
## cada valor pelo número de classes

par(mfrow=c(1,3))
curve(dbeta(x,  a, b), n = 501, 0, 1, col=2, ylim=c(0,10), xlab = expression(theta), ylab = "")
lines(th.d, prior.d*(n.d-1), type = "h", col = 2)

curve(dbeta(x, y+1, (n-y)+1), n = 501, 0, 1, , col="blue")
lines(th.d, LL.d*(n.d-1), type = "h", col = "blue")

curve(dbeta(x, a+y, b+(n-y)), n = 501, 0, 1)
lines(th.d, post.d*(n.d-1), type = "h", col = 1)
par(mfrow=c(1,1))
## Voltar e repetir em um grid "mais fino"

## Outra alternativa é de que podemos usar amostras para avaliar aproximação
post.sam <- sample(th, 100000, prob=post, replace=TRUE)
plot(density(post.sam))
hist(post.sam, prob = TRUE, br = 30, col = "transparent", add = TRUE)
curve(dbeta(x, 22, 74), n = 501, 0, 0.6, add=T, lwd = 2)

## (em geral não necessário) adicionando ruído para obter amostras de distr. contínua  
delta <- diff(th)[1]/2
post.sam1 <-post.sam + runif(length(post.sam), -delta, +delta)
lines(density(post.sam1))

plot(density(post.sam1))
hist(post.sam1, prob = TRUE, br = 30, col = "transparent", add = TRUE)
curve(dbeta(x, 22, 74), n = 501, 0, 1, add = TRUE, lwd = 2)

## Aproximação da Preditiva
Npred <- 100000
## i. a priori (poderia ver como fazer com a priori teórica, mas por amostragem é mais geral)
prior.sam <- sample(th, Npred, replace = TRUE, prob = prior)
## conferindo se bate com priori (ilustrar somente para grid "ralo")
plot(th, prior, type = "h", col = 2)
prior.sam.tb <- prop.table(table(factor(prior.sam, levels = th)))
lines(as.numeric(names(prior.sam.tb))+0.01, prior.sam.tb, col = 2, type = "h", lty = 2)
legend("topright", c("teorica","simulação"), lty = 2:1, col = 2)
mtext("priori")
##
predprior.sam <- rbinom(Npred, size = npred, prob = prior.sam)
predprior.sam.tb <- prop.table(table(factor(predprior.sam, levels = 0:npred)))

plot(predprior.sam.tb, col = 2, ylim = c(0, 0.15))

## ii. a posteriori
## Plug-in [Y|y, \hat{\theta}]
p.hat <- y/n
#plot(0:npred, dbinom(0:npred, size = npred, prob = p.hat), type = "h", col = "blue")
lines(0:npred, dbinom(0:npred, size = npred, prob = p.hat), type = "h", col = "blue")
#lines(0:npred, dbinom(0:npred, size = npred, prob = p.hat), type = "l", col = "blue")
## Bayesiana [Y|y] = \int [Y|y, \theta] [y|\theta] d\theta
predpost.sam <- rbinom(Npred, size = npred, prob = post.sam)
predpost.sam.tb <- prop.table(table(factor(predpost.sam, levels = 0:npred)))
##plot(predpost.sam.tb, col = 1)
lines(predpost.sam.tb, type = "h", col = 1)
#lines(predpost.sam.tb, ty="l", col = 1)
mtext("preditivas")
legend("topright", c("priori","plug-in","posteriori"), lty = 1, col = c("red","blue","black"))

##
## na ordem para acertar escala do eixo Y
##
plot(0:npred, dbinom(0:npred, size = npred, prob = p.hat), type = "h", col = "blue",
     xlab = "y", ylab = "")
#lines(0:npred, dbinom(0:npred, size = npred, prob = p.hat), type = "l", col = "blue")
lines((0:npred)-0.2, predprior.sam.tb, col = 2, type = "h")
#lines(predprior.sam.tb, ty="l", col = 2)
lines((0:npred)+0.2, predpost.sam.tb, type = "h", col = 1)
#lines(predpost.sam.tb, ty="l", col = 1)
legend("topright",
       c("[Y]",expression(paste("[Y|y,", hat(theta), "]")),"[Y|y]"),
       lty = 1, col = c("red","blue","black"))

## zoom
plot(0:npred, dbinom(0:npred, size = npred, prob = p.hat), type = "h", col = "blue", xlim = c(0,30), xlab = "y", ylab = "")
#lines(0:npred, dbinom(0:npred, size = npred, prob = p.hat), type = "l", col = "blue")
lines((0:npred)-0.2, predprior.sam.tb, col = 2, type = "h")
#lines(predprior.sam.tb, ty="l", col = 2)
lines((0:npred)+0.2, predpost.sam.tb, type = "h", col = 1)
#lines(predpost.sam.tb, ty="l", col = 1)
legend("topright",
       c("[Y]",expression(paste("[Y|y,", hat(theta), "]")),"[Y|y]"),
       lty = 1, col = c("red","blue","black"))

## comparando com teórico sem discretização (beta-binomial)
plot(0:npred, Y.yprior, type = "h")
lines(predprior.sam.tb, col = 2)

plot(0:npred, Y.ypost, type = "h")
lines(predpost.sam.tb, type = "h", col = 1)

##
## Exemplo 2
##   [Y|\theta] \sim Bin(n, \theta)
##   [\theta] \sum Tri(a, b)
##
## resultados analíticos: não há forma fechada
## Vamos obter por aproximação discreta para $\theta$

rm(list=ls())
#a <- 0; b <- 1; c <- 0.5
a <- 0; b <- 1; c <- 0.05
n <- 27; y <- 5
##n <- 4; y <- 1
##require(triangle)
by.d <- 0.01
th.d <- seq(0, 1, by = by.d)

par(mfrow=c(1,1))
curve(triangle::dtriangle(x, a=a, b=b, c=c), from = 0, to = 1)
prior.d <- triangle::dtriangle(th.d, a=a, b=b, c=c); prior.d <- prior.d/sum(prior.d)
LL.d <- dbinom(y, size=n, th.d); LL.d <- LL.d/sum(LL.d)
post.d <- prior.d*LL.d ; post.d <- post.d/sum(post.d)

par(mfrow=c(1,2))
plot(th.d+0.01, post.d, type="h", col = 1, xlab = expression(theta), ylab = "")
lines(th.d, LL.d, type = "h", col = "blue")
lines(th.d-0.01, prior.d, type = "h", col = 2)

plot(th.d, post.d, type="l", col = 1)
lines(th.d, LL.d, type = "l", col = "blue")
lines(th.d, prior.d, type = "l", col = 2)
par(mfrow=c(1,1))

par(mfrow=c(1,3))
plot(th.d, prior.d/by.d, type = "h", col = 2)
plot(th.d, LL.d/by.d, type = "h", col = "blue")
plot(th.d, post.d/by.d, type = "h", col = 1)
par(mfrow=c(1,1))
plot(th.d, post.d/by.d, type = "h", col = 1)
## Voltar e repetir em um grid "mais fino"

## preditiva a priori
Npred <- 1e5
##th.prior <- triangle::rtriangle(Npred, a = a, b= b, c = c)
th.sam <- sample(th.d, Npred, replace = TRUE, prob = prior.d)
#th.prior.tb <- prop.table(table(factor(th.sam, levels = th.d)))
#plot(th.d, th.prior.tb, type = "h")

y.prior <- rbinom(Npred, size = n, prob = th.sam)
y.prior.tb <- prop.table(table(factor(y.prior, levels = 0:n)))
plot(0:n, y.prior.tb, type = "h")

## preditiva a posteriori
Npred <- 1e5
th.post.sam <- sample(th.d, Npred, replace = TRUE, prob = post.d)
#th.post.tb <- prop.table(table(factor(th.post.sam, levels = th.d)))
#plot(th.d, th.post.tb, type = "h")

y.post <- rbinom(Npred, size = n, prob = th.post.sam)
y.post.tb <- prop.table(table(factor(y.post, levels = 0:n)))
plot((0:n)+0.2, y.post.tb, type = "h", col = "blue")

## comparando
plot((0:n)+0.2, y.post.tb, type = "h", col = "blue")
lines(0:n, y.prior.tb, type = "h")
