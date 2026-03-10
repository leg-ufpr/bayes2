##############################################################
## CE-315 Inferência Bayesiana
##        Paulo Justiniano Ribeiro Jr
##
## Aproximação da Posteriori: Aproximação Gaussiana (Laplace)
##
##############################################################

##
## Exemplo 1: 
## [Y|\theta] \sim Binomial(n, \theta)
## [\theta] \sim Beta(a, b)
##
## OBS: Neste caso a posteriori é conhecida e aproximação não é necessária
## Entretanto vamos obter a avaliar a aproximação numérica e comparar com resultado teórico

## Resultados analíticos
## [\theta|y] \sim Beta(a+y, b+(n-y))
## E[\theta|y] = \frac{a+y}{a+y + b+(n-y)} = \frac{a+y}{a+b+n}
## Moda[\theta|y] = \frac{a+y-1}{a+b+n-2}
## V[\theta|y] = \frac{(a+y)(b+(n-y))}{(a+b+n)^2 (a+b+n+1)}

## Aproximação Gaussiana
## [\theta|y] \sim N(\mu , \sigma^2)

## Possíveis aproximações para \mu e \sigma^2:
## \mu: moda da log-posteriori
## \sigma^2: curvatura da log-posteriori

## priori uniforme
a <- 1; b <- 1

## dados
#n <- 10+8 ; y <- 10
#n <- 80 ; y <- 19
#n <-  6 ; y <- 4
#n <- 24 ; y <- 2
n = 27; y = 5

##l.lik <- function(theta, n, y) y*log(theta) + (n-y)*log(1-theta)
##U.lik <- function(theta, n, y) (y/theta) - (n-y)/(1-theta)
##H.lik <- function(theta, n, y) - (y/theta^2) - ((n-y)/(1-theta)^2)

l.post <- function(theta, n, y, a, b) (y+a-1)*log(theta) + (n-y+b-1)*log(1-theta)
l.post.alt1 <- function(theta, n, y, a, b) dbinom(y, size = n, prob=theta, log = TRUE) + dbeta(theta, a, b, log=TRUE) 
l.post.alt2 <- function(theta, n, y, a, b) dbeta(theta, a+y, b+n-y, log=TRUE) 
U.post <- function(theta, n, y, a, b) (y+a-1)/theta - (n-y+b-1)/(1-theta)
H.post <- function(theta, n, y, a, b) -((y+a-1)/theta^2) - ((n-y+b-1)/(1-theta)^2)

## valores teóricos
## Moda[\theta|y] = \frac{a+y}{n+a+b-2}
## V[\theta|y] = \frac{(a+y)(b+(n-y))}{(a+b+n)^2 (a+b+n+1)}
(moda <- (y+a-1)/(n+a+b-2))  
(V <- ((y+a)*(n-y+b))/(((n+a+b)^2)*(n+a+b+1)))
sqrt(V)

## aproximações numéricas para moda
optimize(l.post, n=n, y=y, a=a, b=b, interval=c(0,1), maximum=TRUE)$maximum 
uniroot(U.post, n=n, y=y, a=a, b=b, interval=c(0,1))$root

## formas alternativas de escrever a função da log-posteriori
l.post <- function(theta, n, y, a, b) (y+a-1)*log(theta) + (n-y+b-1)*log(1-theta)
l.post.alt1 <- function(theta, n, y, a, b) dbinom(y, size = n, prob=theta, log = TRUE) + dbeta(theta, a, b, log=TRUE) 
l.post.alt2 <- function(theta, n, y, a, b) dbeta(theta, a+y, b+n-y, log=TRUE) 
optimize(l.post.alt1, n=n, y=y, a=a, b=b, interval=c(0,1), maximum=TRUE)$maximum 
optimize(l.post.alt2, n=n, y=y, a=a, b=b, interval=c(0,1), maximum=TRUE)$maximum 

## aproximações numéricas para moda
-n/(moda*(1-moda))                        ## análogo à Informação Esperada
H.post(moda, n=n, y=y, a=a, b=b)          ## análogo à Informação Observada
(H <- optimHess(moda, l.post, n=n, y=y, a=a, b=b))  ## análogo à Informação Observada (aprox. numérica)

## comparação com a variância (teórica) da posteriori Beta
V
-1/H

curve(dbeta(x, a+y, b+n-y), from=0, to=1, n = 501)
curve(dnorm(x, m=moda, sd=sqrt(V)), from=0, to=1, add=T, lty=3, col=4)
curve(dnorm(x, m=moda, sd=sqrt(-1/H)), from=0, to=1, add=T, lty=2, col=2)

## em resumo fazendo a aproximação por algoritmo numérico
## - escreva a função da log-posteriori
## - obtenha o máximo e a curvatura
## - use estes como parâmetros da distribuição normal que aproxima a posteriori
l.post <- function(theta, n, y, a, b) (a+y)*log(theta) + (b+n-y)*log(1-theta)
max.post <- optimize(l.post, n=n, y=y, a=a, b=b, interval=c(0,1), maximum=TRUE)
max.post$maximum
(max.H <- drop(optimHess(max.post$maximum, l.post, n=n, y=y, a=a, b=b)))
-1/max.H

curve(dnorm(x, m=max.post$maximum, sd=sqrt(-1/max.H)), from=0, to=1, lty=2, col=2)
## neste caso que conhecemos a posteriori analítica podemos e avaliar o ajuste
curve(dbeta(x, a+y, b+n-y), from=0, to=1, n = 501, add = TRUE)

## implementado no LearnBayes
LearnBayes::laplace(l.post, 0.5, y=y, n=n, a=a, b=b)
c(moda, -1/H)

rm(moda, V, H)
##
## Uma (boa) opção: aproximar em outra escala (reparametrização) em que a aproximação seja melhor
##  - evita valores fora do domínio
##  - pode conseguir melhor aproximação (e.g. simetria)

# psi = logito = log(θ/1−θ) mapeia [0,1] to [−∞,∞].

l.post <- function(par, n, y, a, b, repar=FALSE){
    if(repar) theta <- exp(par)/(1+exp(par))
    else theta <- par
    return((a+y)*log(theta) + (b+n-y)*log(1-theta))
}

##
## "tudo" numérico
## otimização em theta
optimize(l.post, n=n, y=y, a=a, b=b, interval=c(0,1), maximum=TRUE)$maximum
## otimização em psi
max.psi <- optimize(l.post, n=n, y=y, a=a, b=b, repar = TRUE, interval=c(-20,20), maximum=TRUE)
(mu.n <- max.psi$maximum)
(H.psi <- drop(optimHess(mu.n, l.post, n=n, y=y, a=a, b=b, repar = TRUE)))
(sd.n <- sqrt(-1/H.psi))
curve(dnorm(x, m=mu.n, sd=sqrt(-1/H.psi)), from=mu.n-4*sd.n, to=mu.n+4*sd.n, lty=2, col=2)

## função de densidade na escala reparametrizada
dpost <- function(par, n, y, a, b){
    post <- function(par, n, y, a, b) exp((a+y)*par - (a+b+n)*log(1+exp(par)))
    Cte <- integrate(post, -Inf, Inf, n=n, y=y, a=a, b=b)$value
    return(post(par=par, n=n, y=y, a=a, b=b)/Cte)
}
integrate(dpost, -Inf, Inf, n=n, y=y, a=a, b=b)

curve(dpost(x, n=n, y=y, a=a, b=b), from=mu.n-4*sd.n, to=mu.n+4*sd.n)
curve(dnorm(x, m=mu.n, sd=sd.n), from=mu.n-4*sd.n, to=mu.n+4*sd.n, lty=2, col=2, add = TRUE)

## Em resumo:
## aproximação na parametrização original
(moda.theta <- optimize(l.post, interval = c(0, 1), n=n, y=y, a=a, b=b, repar=FALSE, maximum=TRUE)$maximum)
(H.theta <- drop(optimHess(moda.theta, l.post, n=n, y=y, a=a, b=b, repar=FALSE)))
(sd.theta <- sqrt(-1/H.theta))
 ## ou na reparametrização
(moda.psi <- optimize(l.post, interval = c(-30, 30), n=n, y=y, a=a, b=b, repar=TRUE, maximum=TRUE)$maximum)
(H.psi <- drop(optimHess(moda.psi, l.post, n=n, y=y, a=a, b=b, repar=TRUE)))
(sd.psi <- sqrt(-1/H.psi))


## Após obter a aproximação na escala da reparametrização pode-se
## transformar para a posteriori (aproximada) na escala original 
par(mfrow=c(1,3))
## posteriori de theta e aprox na parametrizacao original
theta.vals <- seq(0,1, l=501)
post.theta <- dbeta(theta.vals, a+y, b+n-y)
aprox.post.theta <- dnorm(theta.vals, m=moda.theta, sd=sd.theta)
plot(theta.vals, post.theta, type="l") 
lines(theta.vals, aprox.post.theta, lty=2, col=2) 
## posteriori de psi e aprox. na reparametrização
psiI <- moda.psi - 4*sd.psi
psiS <- moda.psi + 4*sd.psi
psi.vals <- seq(psiI, psiS, l=501)
post.psi <- dpost(psi.vals, n=n, y=y, a=a, b=b)
aprox.post.psi <- dnorm(psi.vals, m=moda.psi, sd=sd.psi)
plot(psi.vals, post.psi, type="l") 
lines(psi.vals, aprox.post.psi, lty=2, col=2) 
## posteriori de theta e aprox pela transformação de volta da reparametrizacao 
post.psi <- dpost(psi.vals, n=n, y=y, a=a, b=b)
aprox.post.psi <- dnorm(psi.vals, m=moda.psi, sd=sd.psi)
## para transformar variável basta mudar valores no eixo-x (semelhante à invariância da verossimilhança)
theta.back <- exp(psi.vals)/(1+exp(psi.vals))
plot(theta.back, post.psi, type="l", xlim = c(0,1)) 
lines(theta.back, aprox.post.psi, lty=2, col=2) 
## e portanto a aproximação normal na escala transformada produz uma aproximação assimétrica na escala original que pode ser melhor do que a aproximação quadrática na escala original

