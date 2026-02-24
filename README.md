# CE-315 (antiga ce227)

[![deploy](https://github.com/leg-ufpr/ce227/actions/workflows/run.yml/badge.svg)](https://github.com/leg-ufpr/ce227/actions/workflows/run.yml)

## Inferência Bayesiana

Repositório das disciplina **Inferência Bayesiana** (CE-315), ministrada
na UFPR, para o curso de Estatística. Versão atualizada de CE-227.

Este repositório contém todo o material de aula e os arquivos
necessários para gerar a página da disciplina, disponível em:
http://www.leg.ufpr.br/ce315

### Para gerar o site

O site é todo construído usando apenas o [R Markdown][], por isso, o
código fonte está nos arquivos `Rmd`. Para gerar o site você precisará
das versões mais recentes dos pacotes `rmarkdown` e `knitr`.

1. Copie (clone ou fork) esse repositório
2. Abra o R na raíz e renderize o site com `render_site()`

```r
library(rmarkdown)
render_site()
```

**Observações** (desatualizado):

A publicação no site é automatizada através do [GitHub
Actions](https://github.com/leg-ufpr/ce227/actions), e o arquivo de
interesse é o
[.github/workflows/run.yml](https://github.com/leg-ufpr/ce227/blob/master/.github/workflows/run.yml).

[Licença Creative Commons 4.0]: https://creativecommons.org/licenses/by-nc-sa/4.0/deed.pt_BR
[R Markdown]: http://rmarkdown.rstudio.com
