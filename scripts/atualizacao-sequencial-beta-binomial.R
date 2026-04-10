##
## Atualização Sequencial Bayesiana
## Modelo Beta-Binomial: proporção de canhotos em turmas escolares
##
## Disciplina: CE-062C -- Modelagem Bayesiana (UFPR)
##

source("_functions.R")
library(tidyverse)
theme_set(theme_bw(base_size = 14))
set.seed(2025)

## ---------------------------------------------------------------
## 1. Simulação dos dados
## ---------------------------------------------------------------

## Proporção "verdadeira" (desconhecida) de canhotos na escola
theta_real <- 0.12

## 10 turmas com tamanhos variados (entre 15 e 45 alunos)
n_turmas <- 10
tamanhos <- sample(15:45, size = n_turmas)

## Número de canhotos em cada turma (simulado da Binomial)
canhotos <- rbinom(n = n_turmas, size = tamanhos, prob = theta_real)

turmas <- tibble(
    turma = 1:n_turmas,
    n_alunos = tamanhos,
    n_canhotos = canhotos,
    prop_obs = canhotos / tamanhos
)
turmas

ggplot(turmas, aes(x = factor(turma), y = prop_obs)) +
    ## geom_col() +
    geom_point(size = 3) +
    geom_hline(yintercept = theta_real, linetype = "dashed", color = "tomato") +
    labs(x = "Turma", y = "Proporção")

## ---------------------------------------------------------------
## 2. Modelo Beta-Binomial: conjugação
## ---------------------------------------------------------------
##
## Modelo:      Y_j | theta ~ Binomial(n_j, theta)
## Priori:      theta ~ Beta(a0, b0)
## Posteriori:  theta | Y ~ Beta(a0 + sum(y), b0 + sum(n) - sum(y))
##
## Na atualização sequencial, após observar turma j:
##   a_j = a_{j-1} + y_j
##   b_j = b_{j-1} + n_j - y_j
##
## O resultado é idêntico ao obtido com todas as turmas de uma vez.
##

## Hiperparâmetros da priori
a0 <- 7
b0 <- 3
a0/(a0 + b0)

## Grid de valores para theta
theta_grid <- seq(0, 0.4, length.out = 500)

## Visualização da priori
tibble(theta = theta_grid) |>
    mutate(dens = dbeta(theta, a0, b0)) |>
    ggplot(aes(x = theta, y = dens)) +
    geom_line(colour = "steelblue", linewidth = 1) +
    geom_vline(xintercept = theta_real, linetype = "dashed",
        colour = "tomato") +
    labs(
        x = expression(theta),
        y = "Densidade",
        title = paste0("Priori: Beta(", a0, ", ", b0, ")"),
        subtitle = paste0("Média = ", round(a0/(a0+b0), 3),
            "  —  Linha vermelha: valor real (", theta_real, ")")
    )

## ---------------------------------------------------------------
## 3. Atualização sequencial
## ---------------------------------------------------------------

## Armazena os parâmetros a cada etapa (etapa 0 = priori)
historico <- tibble(
    etapa = 0:n_turmas,
    a = NA_real_,
    b = NA_real_,
    turma_label = c("Priori", paste("Turma", 1:n_turmas))
)
historico$a[1] <- a0
historico$b[1] <- b0
historico

for (j in seq_len(n_turmas)) {
    historico$a[j + 1] <- historico$a[j] + turmas$n_canhotos[j]
    historico$b[j + 1] <- historico$b[j] + turmas$n_alunos[j] - turmas$n_canhotos[j]
    plot_posterior(j, historico, theta_grid, theta_real, n_turmas, k = 2)
}

historico

historico <- historico |>
    mutate(
        media = a / (a + b),
        dp = sqrt(a * b / ((a + b)^2 * (a + b + 1)))
    )
historico

## ---------------------------------------------------------------
## 4. Verificação: batch vs sequencial
## ---------------------------------------------------------------

## Posteriori com TODAS as turmas de uma vez
a_batch <- a0 + sum(turmas$n_canhotos)
b_batch <- b0 + sum(turmas$n_alunos) - sum(turmas$n_canhotos)

## Posteriori sequencial (última etapa)
a_seq <- historico$a[n_turmas + 1]
b_seq <- historico$b[n_turmas + 1]

cat("\n--- Posteriori em lote (todas as turmas de uma vez) ---\n")
cat("  a =", a_batch, " b =", b_batch, "\n")
cat("  E[theta] =", round(a_batch / (a_batch + b_batch), 6), "\n")

cat("\n--- Posteriori sequencial (uma turma por vez) ---\n")
cat("  a =", a_seq, " b =", b_seq, "\n")
cat("  E[theta] =", round(a_seq / (a_seq + b_seq), 6), "\n")

cat("\n--- Diferença ---\n")
cat("  |a_batch - a_seq| =", abs(a_batch - a_seq), "\n")
cat("  |b_batch - b_seq| =", abs(b_batch - b_seq), "\n")

## ---------------------------------------------------------------
## 5. Visualização da atualização sequencial
## ---------------------------------------------------------------

## Densidades em cada etapa
densidades <- cross_join(historico, y = tibble(theta = theta_grid)) |>
    mutate(dens = dbeta(theta_grid, a, b),
        turma_label = factor(turma_label,
            levels = c("Priori", paste("Turma", 1:n_turmas)))
    )

## Gráfico individual
p_facetas <- densidades |>
    ggplot(aes(x = theta, y = dens)) +
    geom_line(colour = "steelblue", linewidth = 0.8) +
    geom_vline(xintercept = theta_real, linetype = "dashed",
        colour = "tomato", linewidth = 0.4) +
  facet_wrap(~ turma_label, ncol = 4, scales = "free_y") +
    labs(
        x = expression(theta),
        y = "Densidade",
        title = "Atualização sequencial: proporção de canhotos",
        subtitle = paste0(
            "Priori: Beta(", a0, ", ", b0, ") — ",
            "Linha vermelha: valor real (", theta_real, ")"
        )
    )
p_facetas

## Usando ggridges
library(ggridges)
p_ridges <- densidades |>
    ggplot(aes(x = theta, y = turma_label, height = dens,
        fill = turma_label)) +
    geom_ridgeline(scale = 1, alpha = 0.6) +
    geom_vline(xintercept = theta_real, linetype = "dashed",
        colour = "tomato", linewidth = 0.6) +
    scale_fill_viridis_d() +
    scale_y_discrete(limits = rev(levels(densidades$turma_label))) +
    labs(
        x = expression(theta),
        y = NULL,
        title = "Evolução da posteriori a cada turma adicionada",
        subtitle = paste0("Linha vermelha: valor real (", theta_real, ")")
    ) +
    theme(legend.position = "none")
p_ridges

## ---------------------------------------------------------------
## 6. Evolução da média e ICr95%
## ---------------------------------------------------------------

historico <- historico |>
  mutate(
    icr_lo = qbeta(0.025, a, b),
    icr_hi = qbeta(0.975, a, b)
  )

p_evolucao <- historico |>
  ggplot(aes(x = etapa, y = media)) +
  geom_ribbon(aes(ymin = icr_lo, ymax = icr_hi),
      fill = "steelblue", alpha = 0.2) +
  geom_line(colour = "steelblue", linewidth = 0.8) +
  geom_point(colour = "steelblue", size = 2) +
  geom_hline(yintercept = theta_real, linetype = "dashed",
      colour = "tomato") +
  scale_x_continuous(
    breaks = 0:n_turmas,
    labels = c("Priori", paste0("T", 1:n_turmas))
  ) +
  labs(
    x = NULL,
    y = expression(E(theta ~ "|" ~ dados)),
    title = "Evolução da média posteriori e ICr95%",
    subtitle = "Linha vermelha: valor real"
  )
p_evolucao

## ---------------------------------------------------------------
## 7. Confirmação final: batch = sequencial
## ---------------------------------------------------------------

## Sobreposição das duas posterioris finais
tibble(theta = theta_grid) |>
    mutate(
        Sequencial = dbeta(theta, a_seq, b_seq),
        Lote = dbeta(theta, a_batch, b_batch)
    ) |>
    pivot_longer(-theta, names_to = "metodo", values_to = "dens") |>
  ggplot(aes(x = theta, y = dens, colour = metodo)) +
  geom_line(linewidth = 1) +
  geom_line(linewidth = 3, alpha = 0.3) +
  geom_vline(xintercept = theta_real, linetype = "dashed",
             colour = "tomato") +
  scale_colour_manual(values = c(Lote = "tomato", Sequencial = "steelblue")) +
  labs(
    x = expression(theta),
    y = "Densidade",
    colour = NULL,
    title = "Posteriori final: lote vs. sequencial",
    subtitle = "As duas curvas são idênticas"
  )
