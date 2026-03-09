rm(list=ls())
##
## Taxas e suavizações bayesianas, bayesianas empíricas globais e locais
##

## 1. Modelo para as taxas individuais de cada área:
## Y_i|\theta_i \sim P(n_i \theta_i)
## MLE:
## \hat{\theta}_i = \frac{y_i}{n_i}

## 2. Modelo com taxa global (comum a todas as áreas):
## Y_i|\theta \sim P(N_i \theta) 
## MLE:
## \hat{\theta} = \frac{\sum y_i}{\sum N_i}

## 3. Modelo bayesiano para taxas em cada unidade 
## Y_i|\theta_i \sim P(n_i \theta_i)
## \theta_i \sim G(\alpha, \beta)
## E[\theta_i] = \mu = \frac{\alpha}{\beta}
## Posteriori
## \theta_i|y \sim G(y_i + \alpha, N_i + \beta)
## E[\theta_i|y_i] = \tilde{\theta}_i = \frac{y_i + \alpha}{n_i + \beta}
##                           \ldots   = w_i \hat{\theta}_i + (1-w_i) \mu 
## w_i = \frac{n_i}{n_i + \beta} \in (0, 1)

## Reparametrização da gamma
## T \sim G(a, b)
## \mu = a/b   e    \phi = a/b^2
## a = \mu^2/phi  e  b = \mu/\phi
## b = a/mu

## Reparametrização pode ser usada como forma mais conveniente para
## - Especificar priori (3)
## - "Estimar" priori (4) e (5)

## 4. Modelo para taxas bayesianas empíricas (global)
## Y_i|\theta_i \sim P(n_i \theta_i)
## \theta_i \sim G(\hat{\alpha}, \hat{\beta})
## \tilde{\theta}^G_i = E[\theta_i|y_i] = \frac{y_i + \hat{\alpha}}{n_i + \hat{\beta}}
##                                    = \hat{w}_i \hat{\theta} + (1-\hat{w}_i) \hat{\mu} 

## 5. Modelo para taxas bayesianas empíricas (local)
## Y_i|\theta_i \sim P(n_i \theta_i)
## \theta_i \sim G(\hat{\alpha}_i, \hat{\beta}_i)
## \tilde{\theta}^L_i = E[\theta_i|y_i] = \frac{y_i + \hat{\alpha}_i}{n_i + \hat{\beta}_i}
##                                    = \hat{w}_i \hat{\theta} + (1-\hat{w}_i) \hat{\mu} 

## Pacote com dados e funções relevantes: spdep
##install.packages(spdep, dep = TRUE)
require(spdep)

## Conjunto de dados
## Colunas de interesse:
auckland <- st_read(system.file("shapes/auckland.gpkg", package="spData")[1], quiet=TRUE)
str(auckland)
dim(auckland)
head(auckland)
## M77_85: mortalidade infantil em 9 anos
## Und5_81: população de 5 anos ou menos no censo de 1981 (multiplica por 9 anos nas análises)

##
## 1. \hat{\theta}: Taxa de mortalidade para toda área (global)
## Y_i|\theta \sim P(N \theta) com N = \sum n_i
## \hat{\theta} = \frac{\sum y_i}{\sum n_i}
##
(rt <- with(auckland, sum(M77_85)/sum(9*Und5_81)))

##
## 2. \hat{\theta}_i: Taxas individuais
## Y_i|\theta_i \sim P(n_i \theta_i) 
## \hat{\theta}_i = \frac{y_i}{n_i}
##
## probmap: Função que calcula:
## - raw: taxas brutas (individual)
## - expCount: contagem esperada em cada unidade sob taxa global
## - relRisk: 100* taxa individual/taxa global
## - pmap: probabilidade acumulada na distribuição de Poisson da contagem observada sob média dada pela esperada (calculada com base na taxa global)
res <- with(auckland, probmap(M77_85, 9*Und5_81))
dim(res)
head(res, n = 10)

## fazendo as mesmas contas "na mão", ou seja, sem usar a função probmap
df <- data.frame(yi = auckland$M77_85,
                 ni = 9*auckland$Und5_81
                 )
df <- transform(df,
                ri = yi/ni,
                Eyi = rt * ni
                )
df <- transform(df,
                relRisk = 100*ri/rt,
                pmap = ppois(yi, Eyi)
                )
head(df, n = 10)


## Juntando resultados de probmap com data-frame espacial para mostrar resultados em mapas
res$id <- 1:nrow(res)
auckland$id <- res$id <- 1:nrow(res)
auckland <- merge(auckland, res, by="id")

## alguns gráficos
plot(auckland[, "raw"], main="Estimativas individuais")
plot(auckland[, "relRisk"], main="Taxas de mortalidade padronizadas (SMR)")
plot(auckland[, "pmap"], main="Probabilidade de Poisson",
     breaks=c(0, 0.05, 0.1, 0.5, 0.9, 0.95, 1))

## taxa por 1000 
auckland$raw1000 <- 1000*auckland$raw
plot(auckland[, "raw1000"],
     main="Estimativas individuais (por 1000)")

## importante definir palheta de cores adequada
summary(auckland[, "raw1000", drop=T])
plot(auckland[, "raw1000"],
     main="Estimativas individuais (por 1000)",
     breaks=c(0,2,2.5,3,3.5,20),
     pal = rev(hcl.colors(5, "YlOrRd"))
     )

summary(auckland[, "relRisk", drop=T])
plot(auckland[, "relRisk"],
     main="Risco Relativo (x 100)",
     breaks=c(0, 40, 60, 80, 100, 120, 200, 300, 705),
     pal = rev(hcl.colors(8, "Blue-Red 2"))
     )


##
## 3. \tilde{\theta}_i: Taxas Bayesianas 
## Y_i|\theta_i \sim P(n_i \theta_i)
## \theta_i \sim G(\alpha, \beta)
## \tilde{\theta}_i = E[\theta_i|y_i] = \frac{y_i + \alpha}{n_i + \beta}
##                                    = w_i \hat{\theta}_i + (1-w_i) \mu 
## Reparametrização da gamma
## T \sim G(a, b)
## \mu = a/b   e    \phi = a/b^2
## a = \mu^2/phi  e  b = \mu/\phi
## b = a/mu
## Reparametrização pode ser usada como forma mais conveniente para especificar priori
##
## Taxas obtidas com prioris "subjetivas" para escolhas da alpha e beta

Estthetai <- function(yi, ni, alpha, beta, plot = TRUE, ...){
    if(plot) curve(dgamma(x, alpha, beta), ...)
    (yi + alpha)/(ni + beta)
}
df$rest1 <- with(df, Estthetai(yi, ni, 2, 3))
df$rest1 <- with(df, Estthetai(yi, ni, 2, 3, to = 2.5)) 
df$rest2 <- with(df, Estthetai(yi, ni, 1.2, 10))
df$rest3 <- with(df, Estthetai(yi, ni, 0.5, 0.5))
df$rest4 <- with(df, Estthetai(yi, ni, 0.01, 0.01)) 

with(df, plot(ri, rest1, asp = 1, xlim = c(0, 0.020), ylim = c(0, 0.020), cex = 0.5, pch=19))
abline(0,1)
with(df, points(df$ri, rest2, asp = 1, cex = 0.5, pch=19, col = 2))
with(df, points(df$ri, rest3, asp = 1, cex = 0.5, pch=19, col = 4))
with(df, points(df$ri, rest4, asp = 1, cex = 0.5, pch=19, col = 3))

## taxas obtidas com Prioris "subjetivas" para escolhas de mu e phi
Est2thetai <- function(yi, ni, mu, phi, plot = TRUE, ...){
    alpha <- mu^2/phi
    beta <- mu/phi
    print(c(alpha = alpha, beta = beta))
    if(plot) curve(dgamma(x, alpha, beta), ...)
    (yi + alpha)/(ni + beta)
}
df$rest5 <- with(df, Est2thetai(yi, ni, 0.002, 2E-4)) 
df$rest6 <- with(df, Est2thetai(yi, ni, 0.002, 2E-6))
with(df, plot(ri, rest5, asp = 1, xlim = c(0, 0.020), ylim = c(0, 0.020), cex = 0.5, pch=19))
abline(0,1)
with(df, points(ri, rest6, asp = 1, cex = 0.5, pch=19, col = 2))

df.ord <- df[order(df$ri), ]
df.ord
par(mfrow=c(1,1))
with(df.ord, plot(ri, pch = 19, cex = 0.5))
abline(h = rt, lty = 3)
with(df.ord, points(rest1, pch = 19, cex = 0.5, col = "blue"))
with(df.ord, points(rest6, pch = 19, cex = 0.5, col = "red"))


##
## 4. \tilde{\theta}^G_i : Taxas Bayesianas empíricas global
## Y_i|\theta_i \sim P(n_i \theta_i)
## \theta_i \sim G(\hat{\alpha}, \hat{\beta})
## \tilde{\theta}^G_i = E[\theta_i|y_i] = \frac{y_i + \hat{\alpha}}{n_i + \hat{\beta}}
##                                    = \hat{w}_i \hat{\theta} + (1-\hat{w}_i) \hat{\mu} 
##
## as taxas são médias ponderadas entre individuais e global
foo <- with(auckland, EBest(M77_85, 9*Und5_81))
str(foo)
attr(foo, "parameters")         ## estimativas de média e variância pelo método dos momentos
with(attr(foo, "parameters"),
     curve(dgamma(x, (b^2)/a, b/a), to = 0.01))
## adicionando ao data-frame espacial
auckland$EBglobal <- foo$estmm*1000

summary(auckland[,"EBglobal"])
plot(auckland[,"EBglobal"], 
     breaks=c(0,2,2.5,3,3.5,5),
     main="Mortalidade infantil anual")

## reproduzindo "na mão" o cálculo da função:
### (estimativa por método dos momentos)
(mu.MM <- with(df, sum(yi)/sum(ni)))
(phi.MM <- with(df, sum(ni*(ri-mu.MM)^2)/sum(ni) - rt/mean(ni)))
(alpha.MM <- mu.MM^2/phi.MM)
(beta.MM <- mu.MM/phi.MM)
curve(dgamma(x, alpha.MM, beta.MM), to = 0.01, col = 2)
### pesos
df$wi.MM <- with(df, phi.MM/(phi.MM + mu.MM/ni))
summary(df$wi.MM)
## duas formas equivalentes de calcular as taxas
df$rEB <- with(df, wi.MM * ri + (1-wi.MM)*rt)
df$rEB1 <- with(df, (yi + alpha.MM)/(ni + beta.MM))
all.equal(df$rEB, df$rEB1)
all.equal(auckland$EBglobal/1000, df$rEB)
      
## as taxas Bayesianas são suavizações (smoothing ou shrinkage) 
with(auckland, {
    plot(raw1000, EBglobal, asp = 1, pch = 19, cex = 0.5);
    abline(0,1)
    abline(h = 1000*rt, lty = 3)
    })
with(auckland, {
    plot(raw1000, EBglobal, asp = 1, xlim = c(0, 20), ylim = c(0, 20), pch = 19, cex = 0.5);
    abline(0,1)
    abline(h = 1000*rt, lty = 3)
    })

df.ord <- df[order(df$ri), ]
df.ord
par(mfrow=c(1,1))
with(df.ord, plot(ri, pch = 19, cex = 0.5))
abline(h = rt, lty = 3)
with(df.ord, points(rEB, pch = 19, cex = 0.5, col = "blue"))

##
## 5. Taxas Bayesianas empírica local
## Y_i|\theta_i \sim P(n_i \theta_i)
## \theta_i \sim G(\hat{\alpha}_i, \hat{\beta}_i)
## \tilde{\theta}^L_i = E[\theta_i|y_i] = \frac{y_i + \hat{\alpha}_i}{n_i + \hat{\beta}_i}
##                                    = \hat{w}_i \hat{\theta} + (1-\hat{w}_i) \hat{\mu} 
##
## as taxas são médias ponderadas entre individuais e a taxa da vizinhança
auckland.nb <- poly2nb(auckland)
auckland$EBlocal  <- with(auckland, EBlocal(M77_85,  9*Und5_81, auckland.nb))$est*1000
plot(auckland[,"EBlocal"], breaks=c(0,2,2.5,3,3.5,8),
     main="Infant mortality per 1000 per year")
## as locais suavizam menos
with(auckland, {
    plot(raw1000, EBlocal, asp = 1, pch = 19, cex = 0.5)
    abline(0,1)
    abline(h = 1000*rt, lty = 3)
    })
with(auckland, {
    plot(raw1000, EBlocal, asp = 1, xlim = c(0, 20), ylim = c(0, 20), pch = 19, cex = 0.5)
    abline(0,1)
    abline(h = 1000*rt, lty = 3)
    })

## comparando suavizações
with(auckland, {
    plot(raw1000, EBglobal, asp = 1, xlim = c(0, 20), ylim = c(0, 20), cex = 0.5, pch=19)
    points(raw1000, EBlocal, cex = 0.5, pch=19, col = 4);
    abline(0,1)
    abline(h = 1000*rt, lty = 3)
    })
with(auckland, {
    plot(EBlocal, EBglobal, asp = 1, xlim = c(0, 20), ylim = c(0, 20), cex = 0.5, pch=19)
    abline(0,1)
    })

summary(auckland[,c("raw1000", "EBlocal", "EBglobal"), drop = T])
## comparando suavizações nos mapas
plot(auckland[,c("raw1000", "EBlocal", "EBglobal")],
     breaks=c(0,2,2.5,3,3.5,8), key.pos = 1)

##
## Apêndice: Estimação de (hiper)parâmetros (EB global mas métodos valem para local tb) 
##           por máxima verossimilhança dada pela marginal  
##
## Estimação alternativa (e melhor!)
## Y_i|\theta_i \sim P(n_i \theta_i)
## \theta_i \sim G(\hat{\alpha}, \hat{\beta})
## Y_i|\alpha,\beta \sim BN(\alpha, prob = \frac{n_i}{n_i+\beta})
##P[Y_i = y_i] = \frac((\alpha + y_i - 1)!}{(\alpha-1)! y!} (\frac{n_i}{n_i+\beta})^{y_i} (1 - \frac{n_i}{n_i+\beta})^\alpha
##             = \frac(\gamma(\alpha + y_i}{\gamma(\alpha) \gamma(y+1)} (\frac{n_i}{n_i+\beta})^{y_i} (\frac{beta}{n_i+\beta})^\alpha

dnb <- function(yi, ni, alpha, beta, log = FALSE){
    ld1 <- (lgamma(alpha+yi) - lgamma(alpha) - lgamma(yi+1))
    lnib <- log(ni+beta)
    ld2 <- yi * (log(ni)-lnib) + alpha * (log(beta)-lnib)
    ld <- ld1 + ld2
    if(log) return(ld)
    else return(exp(ld))
}

dnb(yi = 5, ni = 900, alpha = 8, beta = 5000)
dnbinom(5, size = 8, prob = 5000/(900+5000))
dnbinom(5, size = 8, prob = 5000/(900+5000))


nll <- function(pars, yi, ni,
                parametriza = c("logab","ab","medvar","logmedvar", "alogmed")){
    parametriza < match.arg(parametriza,
                            c("logab","ab","medvar","logmedvar", "alogmed"))
    pars.ab <- switch(parametriza,
                      "logab" = exp(pars),
                      "ab" = pars,
                      "medvar" = c((pars[1]^2)/pars[2], pars[1]/pars[2]),
                      "logmedvar" = {pars <- exp(pars);
                          c((pars[1]^2)/pars[2], pars[1]/pars[2])},
                      "alogmed" = c(pars[1], pars[1]/exp(pars[2]))
                      )
    dens <- dnb(yi, ni, pars.ab[1], pars.ab[2], log = TRUE)
    res <- -sum(dens)
    attr(res, "parametriza") <- parametriza
    return(res)
}

## testando reparametrizações
ini <- c(8, 500)

## verificando equivalência das parametrizações
with(df, nll(ini, yi, ni, parametriza = "ab"))
with(df, nll(log(ini), yi, ni, parametriza = "logab"))
with(df, nll(c(ini[1]/ini[2], ini[1]/ini[2]^2), yi, ni, parametriza = "medvar"))
with(df, nll(log(c(ini[1]/ini[2], ini[1]/ini[2]^2)), yi, ni, parametriza = "logmedvar"))
with(df, nll(c(ini[1], log(ini[1]/ini[2])), yi, ni, parametriza = "alogmed"))

## parametrização alpha e beta nao estima bem 
mle1 <- optim(ini, nll, yi = df$yi, ni = df$ni, parametriza = "ab")
mle1 <- optim(ini, nll, yi = df$yi, ni = df$ni, parametriza = "ab",
             method = "L-BFGS-B", lower = c(0, 0))
mle1$value

## com log(alpha) e log(beta) estima bem (estimativas próximas às de Bailey e Gattrel)
mle2 <- optim(log(ini), nll,
             yi = df$yi, ni = df$ni, parametriza = "logab")
mle2$value
mle2$par
exp(mle2$par)  ## alpha e beta

## com mu e phi (media e variância), tb estima bem
mle3 <- optim(c(ini[1]/ini[2], ini[1]/ini[2]^2), nll,
             yi = df$yi, ni = df$ni, parametriza = "medvar")
mle3$value
mle3$par
{
    mle3$value
    temp <- mle3$par
    mu.mle <- temp[1]
    phi.mle <- temp[2]
    (temp <- c(temp[1]^2/temp[2], temp[1]/temp[2])) ## alpha e beta
}  
alpha.mle <- temp[1]
beta.mle <- temp[2]

## log(mu) e log(phi) (media e variância)
mle4 <- optim(c(ini[1]/ini[2], ini[1]/ini[2]^2), nll,
             yi = df$yi, ni = df$ni, parametriza = "logmedvar")
mle4$value
mle4$par
{
    temp <- exp(mle4$par);
    (temp <- c(temp[1]^2/temp[2],temp[1]/temp[2])) ## alpha e beta
}  

## a e log(mu) 
mle5 <- optim(c(ini[1], log(ini[1]/ini[2])), nll,
             yi = df$yi, ni = df$ni, parametriza = "alogmed")
mle5$value
mle5$par
{
    temp <- mle5$par;
    (temp <- c(temp[1], temp[1]/exp(temp[2]))) ## alpha e beta
}  

c(alpha.mle, beta.mle) ## Bailey & Gattrell pag 306: 11.55 e 4336
c(mu.mle, phi.mle)     ## Bailey & Gattrell pag 306: 2.664e-3  e 6.14e-7

## "atalho" usando a MASS::glm.nb 
fit <- MASS::glm.nb(yi ~ 1 + offset(log(ni)), data = df)
exp(coef(fit))              ## mu
fit$theta                   ## alpha
exp(coef(fit))^2/fit$theta  ## phi
fit$theta/exp(coef(fit))    ## beta

## verificando valor do log-verossimilhança
logLik(fit)
mle3$value

rEBmle <- with(df, (yi + alpha.mle)/(ni + beta.mle))

## forma equivalentes de calcular as taxas
df$wi.mle <- with(df, phi.mle/(phi.mle + mu.mle/ni))
df$rEBmle1 <- with(df, wi.mle * ri + (1-wi.mle)*(alpha.mle/beta.mle))
with(df, all.equal(rEBmle, rEBmle1))   
summary(df$wi.MM)
summary(df$wi.mle)
summary(df$rEBmle)
summary(df$rEBmle1)

## Prioris EB com estimação por MM e MLE
curve(dgamma(x, alpha.mle, beta.mle), to = 0.01, col = 2)
curve(dgamma(x, alpha.MM, beta.MM), to = 0.01, col = 2, lty = 2, add = TRUE)
legend("topright", c("priori MM","priori MLE"), lty = 2:1, col = 2)


## Preditiva para n_i = 1000 
yseq <- 0:12
dseq <- dnb(yseq, n = 1000, alpha.MM, beta.MM)
plot(yseq, dseq, pch = 19, type = "h")
dseq <- dnb(yseq, n = 1000, alpha.mle, beta.mle)
lines(yseq+0.1, dseq, pch = 19, col = 4, type = "h")
legend("topright", c("preditiva MM","preditiva MLE"), lty = 1:2, col = c(1,4))

with(df, plot(1000*ri, 1000*rEB, asp = 1, pch = 19, cex = 0.5));
with(df, points(1000*ri, 1000*rEBmle, asp = 1, pch = 19, cex = 0.5, col = 4));
abline(0,1)
with(df, abline(h = 1000*rt, lty = 3));
legend("topleft", c("priori MLE","priori MM"), lty = 1:2, col = c(1,4))

with(df, plot(1000*ri, 1000*rEB, xlim = c(0, 20), ylim = c(0, 20), asp = 1, pch = 19, cex = 0.5));
with(df, points(1000*ri, 1000*rEBmle, asp = 1, pch = 19, cex = 0.5, col = 4));
abline(0,1)
abline(h = 1000*rt, lty = 3)
legend("topleft", c("priori MLE","priori MM"), lty = 1:2, col = c(1,4))

with(df, plot(wi.MM, wi.mle, asp = 1, pch = 19, cex = 0.6)); abline(0,1)
with(df, plot(rEBmle, rEBmle1, asp = 1, pch = 19, cex = 0.6)); abline(0,1)

## outro pacote (método de estimação e modelo um pouco diferente)
## ajusta BN? Estima \alpha e b_0 de regressão BN?
eB <- with(df, SpatialEpi::eBayes(yi, ni))
names(eB)
str(eB)
eB$alpha
exp(eB$beta)  ## mu
eB$alpha/exp(eB$beta)  ## beta
c(eB$alpha, eB$alpha/exp(eB$beta))

head(cbind(auckland$EBglobal, df$rEB, eB$RR, df$rEBmle))
