library(tidyverse)
library(patchwork)
theme_set(theme_bw(base_size = 14))

da <- read_csv("dados_turma_bayes2.csv")

ggplot(da, aes(x = Altura, y = Peso)) +
    geom_point()

library(rjags)

# Preparar os dados
dados_jags <- list(
    y = da$Peso,
    x = da$Altura,
    n = nrow(da)
)

# Modelo JAGS
modelo_string <- "
model {
    # Likelihood
    for (i in 1:n) {
        y[i] ~ dnorm(mu[i], tau)
        mu[i] <- beta0 + beta1 * x[i]
    }
    # Prioris
    beta0 ~ dnorm(0, 0.01)
    beta1 ~ dnorm(10, 0.1)
    tau ~ dgamma(0.001, 0.001)  # priori para precisão
    sigma <- 1/sqrt(tau)  # desvio padrão
}
"

# Compilar o modelo
modelo <- jags.model(
    textConnection(modelo_string),
    data = dados_jags,
    n.chains = 3
)

# Burn-in
update(modelo, 1000)

# Amostragem
amostras <- coda.samples(
    modelo,
    variable.names = c("beta0", "beta1", "sigma"),
    n.iter = 5000
)

# Resumo
summary(amostras)
plot(amostras)


# Extrair as amostras como data frame
amostras_df <- as.data.frame(do.call(rbind, amostras))

# Estimativas pontuais do lm
mod <- lm(Peso ~ Altura, data = da)
coef_lm <- coef(mod)

# Plotar posteriores de beta0
p1 <- ggplot(amostras_df, aes(x = beta0)) +
    geom_histogram(aes(y = ..density..), bins = 50, fill = "lightblue", alpha = 0.7) +
    geom_density(color = "blue", size = 1) +
    geom_vline(xintercept = coef_lm[1], color = "red", linetype = "dashed", size = 1) +
    labs(title = "Posterior de Beta0 (Intercepto)",
         subtitle = "Linha vermelha: estimativa do lm") +
    theme_bw()

# Plotar posteriores de beta1
p2 <- ggplot(amostras_df, aes(x = beta1)) +
    geom_histogram(aes(y = ..density..), bins = 50, fill = "lightgreen", alpha = 0.7) +
    geom_density(color = "darkgreen", size = 1) +
    geom_vline(xintercept = coef_lm[2], color = "red", linetype = "dashed", size = 1) +
    labs(title = "Posterior de Beta1 (Altura)",
         subtitle = "Linha vermelha: estimativa do lm") +
    theme_bw()

p1 + p2

# Comparação numérica
print(colMeans(amostras_df))

print(coef_lm)
print(summary(mod)$sigma)

##------------------------------------------------------------------------------



# Modelo 2: dados padronizados
dados_jags_pad <- list(
    y = scale(da$Peso)[,1],
    x = scale(da$Altura)[,1],
    n = nrow(da)
)

modelo_string_pad <- "
model {
    for (i in 1:n) {
        y[i] ~ dnorm(mu[i], tau)
        mu[i] <- beta0 + beta1 * x[i]
    }
    # Prioris mais fracas/adequadas
    beta0 ~ dnorm(0, 0.0001)  # ainda mais fraca
    beta1 ~ dnorm(0, 0.0001)
    tau ~ dgamma(0.01, 0.01)
    sigma <- 1/sqrt(tau)
}
"

# Ou usar prioris informativas baseadas no conhecimento:
# beta0 ~ dnorm(70, 0.01)  # peso médio esperado
# beta1 ~ dnorm(0.5, 0.1)  # relação esperada altura-peso


modelo_pad <- jags.model(
    textConnection(modelo_string_pad),
    data = dados_jags_pad,
    n.chains = 3
)

update(modelo_pad, 1000)

amostras_pad <- coda.samples(
    modelo_pad,
    variable.names = c("beta0", "beta1", "sigma"),
    n.iter = 5000
)

amostras_df_pad <- as.data.frame(do.call(rbind, amostras_pad))

# Calcular médias e desvios padrão dos dados
mean_x <- mean(da$Altura)
sd_x <- sd(da$Altura)
mean_y <- mean(da$Peso)
sd_y <- sd(da$Peso)

# Transformar os parâmetros padronizados para escala original
amostras_df_pad_transf <- amostras_df_pad
amostras_df_pad_transf$beta1 <- amostras_df_pad$beta1 * (sd_y / sd_x)
amostras_df_pad_transf$beta0 <- mean_y + amostras_df_pad$beta0 * sd_y -
    amostras_df_pad_transf$beta1 * mean_x


# Plotar beta0
p1 <- ggplot() +
    geom_density(data = amostras_df,
        aes(x = beta0, color = "Original"), size = 1.2) +
    geom_density(data = amostras_df_pad_transf,
        aes(x = beta0, color = "Padronizado"), size = 1.2) +
    geom_vline(xintercept = coef_lm[1], color = "red", linetype = "dashed", size = 1) +
    scale_color_manual(values = c("Original" = "blue", "Padronizado" = "purple")) +
    labs(title = "Posterior de Beta0", x = "Beta0", y = "Densidade",
         color = "Modelo") +
    theme_bw()

# Plotar beta1
p2 <- ggplot() +
    geom_density(data = amostras_df,
        aes(x = beta1, color = "Original"), size = 1.2) +
    geom_density(data = amostras_df_pad_transf,
        aes(x = beta1, color = "Padronizado"), size = 1.2) +
    geom_vline(xintercept = coef_lm[2], color = "red", linetype = "dashed", size = 1) +
    scale_color_manual(values = c("Original" = "darkgreen", "Padronizado" = "orange")) +
    labs(title = "Posterior de Beta1", x = "Beta1", y = "Densidade",
         color = "Modelo") +
    theme_bw()

p1 + p2
