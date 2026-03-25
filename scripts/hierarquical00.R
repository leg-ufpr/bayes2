omega <- 0.25
K <- 300
Ni <- 10

alpha <- w * (K-2) + 1
beta <- (1-w) * (K-2) + 1

theta <- rbeta(Ni, alpha, beta)

ni <- sample(10:100, Ni, replace = TRUE)
yi <- rbinom(Ni, ni, theta)

sim <- data.frame(1:10,
                  ni,
                  yi,
                  theta,
                  theta.hat = yi/ni)
sim


