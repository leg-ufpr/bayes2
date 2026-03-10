##
## Pacotes para amostragem MCMC
## Pacotes considerados aqui:
##  - MCMCpack
##  - mcmc
##  - JAGS
##  - Stan
##
##
## O modelo:
## Y|theta ~ B(n, theta)        (verossimilhança)
## theta ~ U[0,1] \equiv B(1,1) (priori)
## theta|y ~ Be(y+a, (n-y) + b) (mas vamos supor que não temos esta forma fechada)

## rm(list=ls()))

## No exemplo:
a <- 1; b <-  1
n <- 80; y <- 19 

##
## 1. Pacote MCMCpack
##

require(MCMCpack)
## Implementa algoritmo de Metrópolis
##
## Usando a função MCMCmetrop1() que permite "user-defined continuous distribution"
## argumento "fun" : unnormalised (log)density
## theta|y \propto theta^(y+a) (1-theta)^(n-y+b)
## log(theta|y) \propto (y+a) * log(\theta) + (n-y+b) * log(1-theta)

## usando reparametrização eta = log(theta/(1-theta)) para parâmetro na escala do logito 
fun <- function(eta, apost = y+a, bpost = n-y+b){
    ## o calculo aqui seria:
    #p <- exp(eta)/(1+exp(eta))
    #return(apost * log(p) + bpost * log(1-p))
    ## mas a forma equivalente a seguir é mais estável numericamente
    logp <- ifelse(eta < 0, eta - log1p(exp(eta)), - log1p(exp(- eta)))
    logq <- ifelse(eta < 0, - log1p(exp(eta)), - eta - log1p(exp(- eta)))
    ## logl <- sum(logp[y == 1]) + sum(logq[y == 0])
    apost * logp + bpost * logq
}

ams1 <- MCMCmetrop1R(fun, theta.ini = 0, a = 7, b = 11, 
                    ##             optim.method = "L-BFGS-B", optim.lower = 0, optim.upper = 1,
                    mcmc = 10000,
                    burnin = 500, 
                    thin = 10,
                    tune = 3
                    )
str(ams1)
plot(ams1)
plot(acf(ams1))
summary(ams1)

## transformando para escala da proporção

ilogis <- function(x) exp(x)/(1+exp(x))
ams1.1 <- ilogis(ams1)
## comparando com resultado teórico
plot(density(ams1.1))
curve(dbeta(x,y+a,n-y+b), add=T)

## 
## O pacote possui funções "pré prontas" para certos modelos
## Nestes casos não é necessário definir "fun" 
##
detach("package:MCMCpack")

##
## 2. Pacote mcmc
##
## Usa algoritmo de Metrópolis
## Assim como em MCMCpack::MCMCmetrop1r() requer "log unnormalized density"

## mcmc::metrop():
#metrop(obj, initial, nbatch, blen = 1, nspac = 1, scale = 1, outfun, debug = FALSE, ...)
require(mcmc)

ams2 <- metrop(fun, initial = 0, nbatch = 1000)
str(ams2)
ams2$accept

# começa onde parou anterior (e desta forma, emula "burnin")
ams2 <- mcmc::metrop(ams2, nbatch = 10000, scale=3, blen = 50)
str(ams2)
names(ams2)
ams2$accept
ams2$nbatch
ams2$blen
str(ams2$batch)

with(ams2, plot(ts(batch)))
with(ams2, acf(batch))
with(ams2, plot(density(batch)))

## quantidade/parametro de interesse
## comparando com resultado teórico
with(ams2, plot(density(ilogis(batch))))
curve(dbeta(x,y+a,n-y+b), add=T)

## alternativa transformando direto na função
ams2 <- mcmc::metrop(fun, initial = 0, nbatch = 10000, scale=3,
                    outfun = \(x){exp(x)/(1+exp(x))})
str(ams2)
names(ams2)
ams2$accept
ams2$nbatch
ams2$blen
str(ams2$batch)

with(ams2, plot(ts(batch)))
with(ams2, acf(batch))
with(ams2, plot(density(batch)))
curve(dbeta(x,y+a,n-y+b), add=T)

ams2 <- mcmc::metrop(fun, initial = 0, nbatch = 1000, scale=3,
                    outfun = \(x){c(x, exp(x)/(1+exp(x)))})
ams2 <- mcmc::metrop(ams2, nbatch = 10000, scale=3, nspac = 10,
                    outfun = \(x){c(x, exp(x)/(1+exp(x)))})
str(ams2)
str(ams2$batch)
head(ams2$batch)
with(ams2, plot(ts(batch)))
with(ams2, acf(batch))
with(ams2, plot(density(batch[,1])))
with(ams2, plot(density(batch[,2])))
curve(dbeta(x,y+a,n-y+b), add=T)

## Comandos como a seguir podem ser úteis para alguns testes/diagnósticos de convergência (usando blen para múltiplas cadeias)
##ams2 <- mcmc::metrop(ams2, nbatch = 100, blen=100)
### ...
detach("package:mcmc")

##
## 3. Jags (instalado "stand alone" no sistema
##    interface com R via via pacotes rjags ou runjags
##
require(rjags)

cat("model {
## verossimilhança [Y|\theta]
	y ~ dbin(theta, n)
##priori [\theta]
	theta ~ dbeta(5, 12)
        phi <- log(theta/(1-theta))
 }
",
 file="binom.modelo"
)

## 3 cadeias começando em locais diferentes
inis <- list(list(theta = 0.1),
             list(theta = 0.5),
             list(theta = 0.9)
             )

jags <- jags.model("binom.modelo",
                   data = list('y' = y, 'n' = n),
                   n.chains = 3,
                   inits = inis,
                   n.adapt = 100
                   )
ams3 <- coda.samples(jags, c('theta', "phi"), n.iter=10000, thin=10)

str(ams3)
plot(ams3)
plot(ams3[[1]])
plot(ams3[[2]])
plot(ams3[[3]])

par(mfrow=c(2,2))
plot(ams3)
str(ams3)
plot(acf(ams3[[1]]))
plot(acf(ams3[[2]]))
plot(acf(ams3[[3]]))
summary(ams3)
HPDinterval(ams3)
##
detach("package:rjags")

## pacote/interface alternativa
## require(runjags)
## datalist <- dump.format(list(x = 6, n = 16))
## params <- c("theta")
## inicial <- dump.format(list(theta = 0.5))
## ## Modelo
## mod <- "model{
## x ~ dbin(theta, n)
## theta ~ dbeta(1, 1)
## }"
## ## Ajuste
##m.jags <- run.jags(model = mod,
##                   monitor = params,
##                   data = datalist,
##                   inits = c(inicial, inicial),
##                   n.chains = 2,
##                   burnin = 5000,
##                   thin = 5,
##                   sample = 10000
##)

## Resultados
## m.jags
## qbeta(c(0.025, 0.5, 0.975), a + y + 1, b + n - y + 1)
## plot(m.jags)

##
## 4. Stan via pacote rstan()
##
options(mc.cores = parallel::detectCores())
require(rstan)

modelo <- "// exemplo binomial
data {
  int n;          
  int y;          
}
parameters {
  real<lower=0, upper=1> theta;  
}
transformed parameters { //generated_quantities
  real phi;
  phi = log(theta/(1-theta));
}
model {
    theta ~ uniform(0,1);
    y ~ binomial(n, theta);
}"

dados <- list(n = 80, y = 19)

fit <- stan(model_code = modelo,
            data = dados,
            iter = 10000,
            thin = 10)

fit
traceplot(fit)
summary(fit)
plot(fit)

require(shinystan)
fit.shiny <- as.shinystan(fit)
launch_shinystan(fit.shiny)

