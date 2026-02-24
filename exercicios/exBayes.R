##
## Exemplos de teorema de Bayes
## Solução organizada como um data.frame/tibble
## com colunas: theta, priori, vero1, vero2, ..., produto, posteriori
## onde priori = P(theta), veroi = P(Yi|theta), produto = priori * vero1 * vero2 * ...
## e posteriori = produto / sum(produto)
##

## Função para produzir gráfico dos resultados
plot.exemplo <- function(obj, YLIM = NULL, shift=0.05,
                         priori.plot = TRUE, verossim.plot = TRUE, posteriori.plot = TRUE){
    priori <- prop.table(obj$priori)
    veroCols <- grep("vero", names(obj))
    if(length(veroCols)==1) verossim <- obj$vero
    else verossim <- do.call( "*", obj[,veroCols])
    verossim <- prop.table(verossim)
    posteriori <- prop.table(obj$posteriori)
    if(is.null(YLIM))
        YLIM <- c(0, max(c(priori, verossim, posteriori))*1.1)
    plot(obj$theta+shift, posteriori, type="n", ylim=YLIM,
         xlab=expression(theta), ylab="")
    if(priori.plot) lines(obj$theta-shift, priori, col = "red", type="h")
    if(verossim.plot) lines(obj$theta, verossim, col = "blue", type="h")
    if(posteriori.plot) lines(obj$theta+shift, posteriori, type="h")
    legend("topright",
           legend=c("Priori","Verossim.","Posteriori"),
           col=c("red","blue","black"), lty = 1, horiz=TRUE, cex = 0.70)
    return(invisible(NULL))
}
##undebug(plot.exemplo)


## Exemplo 1: Teste diagnóstico de uma doença rara

## Considere o problema do teste diagnóstico de uma doença rara, que afeta 2% da população. Um teste diagnóstico para a doença tem sensibilidade de 90% e especificidade de 80%. Ou seja, se uma pessoa tem a doença, o teste dá positivo com probabilidade 90%, e se a pessoa não tem a doença, o teste dá negativo com probabilidade 80%.
## O objetivo é determinar a probabilidade de uma pessoa ter ou não a doença dado que o teste deu positivo.

## Definindo notação:
## - $D$ : evento "a pessoa tem a doença"
## - $T$ : evento "o teste deu positivo"
## - $P(D) = 0.02$ : prevalência da doença na população
## - $P(T|D) = 0.9$ : sensibilidade do teste
## - $P(T|D^c) = 0.2$ : taxa de falso positivo (1 - especificidade)

## Redefinindo notação:
## - parâmetro $\theta \in \{0, 1\}$  
## - variável observada $Y \in \{0, 1\}$
## - $P(\theta = 1) = 0.02$
## - $P(Y = 1 | \theta = 1) = 0.9
## - $P(Y = 1 | \theta = 0) = 0.2$
## - $P(\theta = 0) = 0.98$
## - $P(Y = 0 | \theta = 1) = 0.1$
## - $P(Y = 0 | \theta = 0) = 0.8

## Priori: $P(\theta)$
## Para teste POSITIVO:
## Verossimilhança: $P(Y = 1 | \theta)$

ex01.1 <- tibble::tibble(
theta = c(0,1),
priori = c(0.98, 0.02),
vero = c(0.20, 0.90),
produto = priori * vero,
posteriori = produto / sum(produto)
)
ex01.1

# Para teste NEGATIVO:
Verossimilhança: $P(Y = 0 | \theta)$
ex01.2 <- tibble::tibble(
  theta = c(0,1),
  priori = c(0.98, 0.02),
  vero = c(0.80, 0.10),
  produto = priori * vero,
  posteriori = produto / sum(produto)
)
ex01.2

## Agora suponha que a pessoa fez o teste duas vezes, e ambos os testes deram positivo. Qual as probabilidade de a pessoa ter ou não a doença dado que ambos os testes deram positivo?

## Priori: $P(\theta)$
## Verossimilhança: $P(Y_1 = 1, Y_2 = 1 | \theta)$
## Assumindo que os testes são independentes, temos
## $P(Y_1 = 1, Y_2 = 1 | \theta) = P(Y_1 = 1 | \theta) * P(Y_2 = 1 | \theta)$
ex01.3 <- tibble::tibble(
theta = c(0,1),
priori = c(0.98, 0.02),
vero1 = c(0.20, 0.90),
vero2 = c(0.20, 0.90),
produto = priori * vero1 * vero2,
posteriori = produto / sum(produto)
)
ex01.3

## Agora suponha que a pessoa fez o teste três vezes, e os resultados foram: positivo, negativo e positivo. Qual as probabilidade de a pessoa ter ou não a doença dado que os testes deram esses resultados?
## Priori: $P(\theta)$
## Verossimilhança: $P(Y_1 = 1, Y_2 = 0, Y_3 = 1 | \theta)$
## Assumindo independência, temos
## $P(Y_1 = 1, Y_2 = 0, Y_3 = 1 | \theta) = P(Y_1 = 1 | \theta) * P(Y_2 = 0 | \theta) * P(Y_3 = 1 | \theta)$
ex01.4 <- tibble::tibble(
  theta = c(0,1),
  priori = c(0.98, 0.02),
  vero1 = c(0.20, 0.90),
  vero2 = c(0.80, 0.10),
  vero3 = c(0.20, 0.90),
  produto = priori * vero1 * vero2 * vero3,
  posteriori = produto / sum(produto)
)
ex01.4


## Exemplo 2: Teste de avaliação de conhecimento

## Estima-se que um estudante tem uma chance de 40% de dominar um determinado assunto.
## O estudante faz uma questão de múltipla escolha (com cinco alternativas) para avaliar seu conhecimento.
## O estudante tem 85% de chance de acertar a questão se domina o assunto.
## Avalie a probabilidade de o estudante dominar o assunto, dado que ele acertou a questão.

## Definindo notação:
## - $\theta \in \{0, 1\}$ : parâmetro "o estudante domina o assunto" ($\theta = 1$) ou "o estudante não domina o assunto" ($\theta = 0$)
## - $Y \in \{0,1\}$ : variável observada "o estudante acertou a questão" ($Y = 1$) ou "o estudante errou a questão" ($Y = 0$)
## $P(\theta = 1) = 0.40$
## $P(Y = 1 | \theta = 1) = 0.85$
## $P(Y = 1 | \theta = 0) = 0.20$

## Se acerta a questão

ex02.1 <- tibble::tibble(
  theta = c(0,1),          
  priori = c(0.60, 0.40),             ## P (θ)
  vero = c(0.20, 0.85),               ## P(Y=1|θ)
  produto = priori * vero,
  posteriori = produto / sum(produto) ## P(θ|Y=1)
)
ex02.1


## Se erra a questão
ex02.2 <- tibble::tibble(
                       theta = c(0,1),
                       priori = c(0.60, 0.40),
                       vero = c(0.80, 0.15),
                       produto = priori * vero,
                       posteriori = produto / sum(produto)
                   )
ex02.2

## Suponha que o estudante faz uma segunda questão com probabilidade de acerto de 70% se domina o assunto.
## Se o estudante acerta ambas, qual a probabilidade de ele dominar ou não o assunto?

ex02.3 <- tibble::tibble(
  theta = c(0,1),
  priori = c(0.60, 0.40),
  vero1 = c(0.20, 0.85),
  vero2 = c(0.20, 0.70),
  produto = priori * vero1 * vero2,
  posteriori = produto / sum(produto)
  )
ex02.3

## Exemplo 3: Monty Hall

## Notação:
## - $\theta \in \{1, 2, 3\}$ : porta onde está o prêmio
## - $Y \in \{1, 2, 3\}$      : porta revelada

## Supondo a porta 1 é escolhida e a porta 2 é revelada
p <- 0.5 ## prob de revelar a porta 2 se escolhe a porta 1 inicialmente
ex03.1 <- tibble::tibble(
  theta = c(1,2,3),  
  priori = c(1/3, 1/3, 1/3),          ## P(θ)
  vero = c(p, 0, 1),                  ## P(Y=2|θ)
  produto = priori * vero,
  posteriori = produto / sum(produto) ## P(θ|Y=2)
)
ex03.1

## Exemplo 4: Urna com bolas

## Seja uma urna com 1 bola branca.
## Bola(s) pretas são adicionadas à urna. O número de boas adicionadas é dado pelo resultado do lançamento de um dado mas o resultado não é revelado.
## Retira-se uma bola da urna e observa-se que ela é preta.
## Qual a distribuição de probabilidades da face do dado?

## Notação
## - $\theta \in \{1, 2, 3, 4, 5, 6\}$ : face do dado
## - $Y \in \{0, 1\}$ : cor da bola retirada (0=branca, 1=preta)
ex04.1 <- tibble::tibble(
  theta = 1:6,                      
  priori = rep(1/6, 6),             ## P(θ)
  vero = (1:6)/(2:7),               ## P(Y=1|θ)
  produto = priori * vero,
  posteriori = produto /sum(produto)## P(θ|Y=preta)
)
ex04.1

with(ex04.1,{
     plot(theta+0.05, posteriori, type="h", ylim=c(0,0.30),
          xlab=expression(theta), ylab="");
     lines(theta-0.05, priori, col = 2, type="h");
     lines(theta, prop.table(vero), col = 4, type="h");
     legend("topright", legend=c("Priori","verossimilhança","Posteriori"), col=c(2,4,1), lty = 1)
     }
)

## Mudando priori
ex04.2 <- tibble::tibble(
  theta = 1:6,                      
  priori = c(3,3,2,2,1,1)/12,       ## P(θ)
  vero = (1:6)/(2:7),               ## P(Y=1|θ)
  produto = priori * vero,
  posteriori = produto /sum(produto)## P(θ|Y=preta)
)
ex04.2

with(ex04.2,{
     plot(theta+0.05, posteriori, type="h", ylim=c(0,0.30),
          xlab=expression(theta), ylab="");
     lines(theta-0.05, priori, col = 2, type="h");
     lines(theta, prop.table(vero), col = 4, type="h");
     legend("topright", legend=c("Priori","verossimilhança","Posteriori"), col=c(2,4,1), lty = 1)
     }
)

## Suponha que uma segunda bola é retirada e também é preta. Qual a distribuição de probabilidades da face do dado? (sem reposição da primeira bola, com a priori inicial uniforme)

ex04.3 <- tibble::tibble(
  theta = 1:6,                      
  priori = rep(1/6, 6),             ## P(θ)
  vero1 = (1:6)/(2:7),              ## P(Y1=1|θ)
  vero2 = (0:5)/(1:6),              ## P(Y2=1|θ,Y1=1)
  produto = priori * vero1 * vero2,
  posteriori = produto /sum(produto)## P(θ|Y1=1,Y2=1)
)
ex04.3

with(ex04.3,{
     plot(theta+0.05, posteriori, type="h", ylim=c(0,0.30),
          xlab=expression(theta), ylab="");
     lines(theta-0.05, priori, col = 2, type="h");
     lines(theta, prop.table(vero1*vero2), col = 4, type="h");
     legend("topright", legend=c("Priori","verossimilhança","Posteriori"), col=c(2,4,1), lty = 1)
     }
)

## com a segunda priori

ex04.4 <- tibble::tibble(
  theta = 1:6,                      
  priori = c(3,3,2,2,1,1)/12,       ## P(θ)
  vero1 = (1:6)/(2:7),              ## P(Y1=1|θ)
  vero2 = (0:5)/(1:6),              ## P(Y2=1|θ,Y1=1)
  produto = priori * vero1 * vero2,
  posteriori = produto /sum(produto)## P(θ|Y1=1,Y2=1)
)
ex04.4

with(ex04.4,{
     plot(theta+0.05, posteriori, type="h", ylim=c(0,0.30),
          xlab=expression(theta), ylab="");
     lines(theta-0.05, priori, col = 2, type="h");
     lines(theta, prop.table(vero1*vero2), col = 4, type="h");
     legend("topright", legend=c("Priori","verossimilhança","Posteriori"), col=c(2,4,1), lty = 1)
     }
)

par(mfrow=c(1,1))

par(mfrow=c(2,2), mar=c(2,2,1,1), mgp = c(2,1,0))
plot.exemplo(ex04.1, YLIM=c(0,0.30))
plot.exemplo(ex04.2, YLIM=c(0,0.30))
plot.exemplo(ex04.3, YLIM=c(0,0.30))
plot.exemplo(ex04.4, YLIM=c(0,0.30))
par(mfrow=c(1,1))

## Exemplo 5: proporção de canhotos

## Suponha que a proporção de canhotos na população é desconhecida e queremos estimá-la considetando apenas três possíveis valores: 0.05, 0.10 e 0.15 com probabilidades 0.60, 0.30 e 0.10, respectivamente. Se em um grupo de 27 pessoas, 5 são canhotos, qual a distribuição a posteriori da proporção de canhotos na população?

## Notação
## - $\theta \in \{0.05, 0.10, 0.15\}$ : proporção de canhotos na população
## - $Y \in \{0, 1, ..., 27\}$ : número de canhotos no grupo
ex05.1 <- tibble::tibble(
   theta = c(0.05, 0.10, 0.15),
   priori = c(0.60, 0.30, 0.10),                        ## P(θ)
   vero = dbinom(5, size=27, prob=theta), ## P(Y=5|θ)
   produto = priori * vero,
   posteriori = produto/sum(produto) ## P(θ|Y=5)
   )
ex05.1
plot.exemplo(ex05.1, shift = 0.001)

## Suponha que em um segundo grupo de 40 pessoas, 6 são canhotos. Qual a distribuição a posteriori da proporção de canhotos na população considerando a priori inicial?
ex05.2 <- tibble::tibble(
  theta = c(0.05, 0.10, 0.15),
  priori = c(0.60, 0.30, 0.10),                        ## P(θ)
  vero1 = dbinom(5, size=27, prob=theta), ## P(Y1=5|θ)
  vero2 = dbinom(6, size=40, prob=theta), ## P(Y2=6|θ)
  produto = priori * vero1 * vero2,
  posteriori = produto/sum(produto) ## P(θ|Y1=5,Y2=6)
  )
ex05.2
plot.exemplo(ex05.2, shift = 0.001)

## verifique que considerando um só grupo de 27+40=67 pessoas com 5+6 = 11 canhotos, a distribuição a posteriori é a mesma 
ex05.3 <- tibble::tibble(
  theta = c(0.05, 0.10, 0.15),
  priori = c(0.60, 0.30, 0.10),                         ## P(θ)
  vero = dbinom(11, size=67, prob=theta), ## P(Y=11|θ)
  produto = priori * vero,
  posteriori = produto/sum(produto) ## P(θ|Y=11)
  )
ex05.3

## Valores de $\theta$ entre 1 e 30% e priori proporcional a 5, 3 e 1
## para $\theta \in (0.01, 0.10]$, $\theta \in (0.11, 0.20]$ e $\theta \in (0.21, 0.30]$, respectivamente.

ex05.4 <- tibble::tibble(
  theta = seq(0.01, 0.30, by=0.01),
  priori = rep(c(5,3,1), each = 10),                    ## P(θ)
  vero = dbinom(11, size=67, prob=theta), ## P(Y=11|θ)
  produto = priori * vero,
  posteriori = produto/sum(produto) ## P(θ|Y=11)
  )
ex05.4
plot.exemplo(ex05.4, shift = 0.01)

##
## Exemplo 6: Média de ocorrências (Poisson)
##

## Suponha que o número de ocorrências de um determinado evento em um intervalo de tempo segue uma distribuição de Poisson com parâmetro $\theta$. Considera-se dez possíveis valores para $\theta$: $1, 2, 3, \ldots 10$
## 1. Supondo com probabilidades iguais para o valor do parâmetro $\theta$:
##   a) se em um intervalo de tempo foram observadas 8 ocorrências, qual a distribuição a posteriori de $\theta$?
##   b) se em um segundo intervalo de tempo foram observadas 6 ocorrências, qual a nova distribuição a posteriori de $\theta$ considerando a priori inicial?
##   c) Verifique que considerando um só intervalo de tempo com 8+6=14 ocorrências, a distribuição a posteriori é a mesma.
## 2. Repita itens anteriores considerando agora probabilidades para $\theta$ proporcionais a $0.25 \exp\{-0.25 \theta\}$.
## 3. Proponha outra priori para $\theta$ e repita os itens anteriores.
## 4. Considere agora que $\theta$ pode assumir qualquer valor positivo. 
Considere como priori valores proporcionais aos de uma distribuição gama com parâmetros $a=3$ e $b=1$. Repita os itens anteriores. 

ex06.1a <- tibble::tibble(
  theta = 1:10, 
  priori = rep(0.1, 10),           ## P(θ)
  vero = dpois(8, lambda = theta), ## P(Y=11|θ)
  produto = priori * vero,
  posteriori = produto/sum(produto)## P(θ|Y=11)
  )
ex06.1a
plot.exemplo(ex06.1a, shift = 0.1)

ex06.1b <- tibble::tibble(
  theta = 1:10, 
  priori = rep(0.1, 10),            ## P(θ)
  vero1 = dpois(8, lambda = theta), ## P(Y_1=8|θ)
  vero2 = dpois(6, lambda = theta), ## P(Y_2=6|θ)
  produto = priori * vero1 * vero2,
  posteriori = produto/sum(produto)## P(θ|Y_1=8,Y_2=6)
  )
ex06.1b
plot.exemplo(ex06.1b, shift = 0.1)


Vpois <- Vectorize(function(theta, y){
    exp(sum(dpois(y, lambda = theta, log = TRUE)))
}, "theta")
ex06.1c <- tibble::tibble(
  theta = 1:10, 
  priori = rep(0.1, 10),            ## P(θ)
  vero = Vpois(theta, c(8,6)),      ## P(Y_1=8, Y2=6|θ)
  produto = priori * vero,
  posteriori = produto/sum(produto) ## P(θ|Y_1=8,Y_2=6)
  )
ex06.1c
plot.exemplo(ex06.1c, shift = 0.1)
    
ex06.2a <- tibble::tibble(
  theta = 1:10, 
  priori = 0.25 * exp(-0.25 * theta), ## P(θ)
  vero = dpois(8, lambda = theta),    ## P(Y=11|θ)
  produto = priori * vero,
  posteriori = produto/sum(produto)   ## P(θ|Y=11)
  )
ex06.2a
plot.exemplo(ex06.2a, shift = 0.1)

ex06.2b <- tibble::tibble(
  theta = 1:10, 
  priori = 0.25 * exp(-0.25 * theta),  ## P(θ)
  vero1 = dpois(8, lambda = theta),    ## P(Y_1=8|θ)
  vero2 = dpois(6, lambda = theta),    ## P(Y_2=6|θ)
  produto = priori * vero1 * vero2,
  posteriori = produto/sum(produto)    ## P(θ|Y_1=8,Y_2=6)
  )
ex06.2b
plot.exemplo(ex06.2b, shift = 0.1)

ex06.2c <- tibble::tibble(
  theta = 1:10, 
##  priori = 0.25 * exp(-0.25 * theta),## P(θ)
  priori = dexp(theta, 0.25),         ## P(θ)
  vero = Vpois(theta, c(8,6)),         ## P(Y_1=8, Y2=6|θ)
  produto = priori * vero,
  posteriori = produto/sum(produto)    ## P(θ|Y_1=8,Y_2=6)
  )
ex06.2c
plot.exemplo(ex06.2c, shift = 0.1)

par(mfrow=c(2,3), mar=c(2,2,1,1), mgp = c(2,1,0))
plot.exemplo(ex06.1a, shift = 0.1, YLIM=c(0,0.25))
plot.exemplo(ex06.1b, shift = 0.1, YLIM=c(0,0.25))
plot.exemplo(ex06.1c, shift = 0.1, YLIM=c(0,0.25))
plot.exemplo(ex06.2a, shift = 0.1, YLIM=c(0,0.25))
plot.exemplo(ex06.2b, shift = 0.1, YLIM=c(0,0.25))
plot.exemplo(ex06.2c, shift = 0.1, YLIM=c(0,0.25))
par(mfrow=c(1,1))


ex06.4a <- tibble::tibble(
  theta = seq(0.5, 20, by=0.5),
  priori = dgamma(theta, 3, 1),    ## P(θ)
  vero = dpois(8, lambda = theta), ## P(Y=11|θ)
  produto = priori * vero,
  posteriori = produto/sum(produto) ## P(θ|Y=11)
  )
ex06.4a
plot.exemplo(ex06.4a, shift = 0.1)

ex06.4b <- tibble::tibble(
  theta = seq(0.5, 20, by=0.5),
  priori = dgamma(theta, 3, 1),     ## P(θ)
  vero1 = dpois(8, lambda = theta), ## P(Y_1=8|θ)
  vero2 = dpois(6, lambda = theta), ## P(Y_2=6|θ)
  produto = priori * vero1 * vero2,
  posteriori = produto/sum(produto) ## P(θ|Y_1=8,Y_2=6)
  )
ex06.4b
plot.exemplo(ex06.4b, shift = 0.1)

ex06.4c <- tibble::tibble(
  theta = seq(0.5, 20, by=0.5), 
  priori = dgamma(theta, 3, 1),     ## P(θ)
  vero = Vpois(theta, c(8,6)),      ## P(Y_1=8, Y2=6|θ)
  produto = priori * vero,
  posteriori = produto/sum(produto) ## P(θ|Y_1=8,Y_2=6)
  )
ex06.4c
plot.exemplo(ex06.4c, shift = 0.1)


ex06.4d <- tibble::tibble(
  theta = seq(0.5, 20, by=0.5), 
  priori = dgamma(theta, 3, 1),     ## P(θ)
  vero = Vpois(theta, c(8,6,7,9,5,7)),      ## P(Y_1=8, Y2=6|θ)
  produto = priori * vero,
  posteriori = produto/sum(produto) ## P(θ|Y_1=8,Y_2=6)
  )
ex06.4d
plot.exemplo(ex06.4d, shift = 0.1)

ex06.4e <- tibble::tibble(
  theta = seq(0.1, 20, by=0.1), 
  priori = dgamma(theta, 3, 1),     ## P(θ)
  vero = Vpois(theta, c(8,6,7,9,5,7)),      ## P(Y_1=8, Y2=6|θ)
  produto = priori * vero,
  posteriori = produto/sum(produto) ## P(θ|Y_1=8,Y_2=6)
  )
ex06.4e
plot.exemplo(ex06.4e, shift = 0.2)

