# CE-062(C) Tópicos: Modelagem Bayesiana

[![Render and Publish Site](https://github.com/leg-ufpr/bayes2/actions/workflows/render-and-publish.yml/badge.svg)](https://github.com/leg-ufpr/bayes2/actions/workflows/render-and-publish.yml)

## Inferência Bayesiana

Repositório da disciplina optativa **Modelagem Bayesiana**, ministrada
na UFPR, para o curso de Estatística e Ciência de Dados.

Este repositório contém materiais de aula e os arquivos
necessários para gerar a página da disciplina, disponível em:
http://www.leg.ufpr.br/bayes2

### Publicação automática

A cada `git push` para os branches `main` ou `master`, o site é
automaticamente renderizado e publicado via [GitHub Actions][]. O
workflow `.github/workflows/render-and-publish.yml`:

1. Instala o R e os pacotes `rmarkdown` e `knitr`
2. Renderiza o site com `rmarkdown::render_site()` (saída em `docs/`)
3. Publica o resultado no branch `gh-pages` via [peaceiris/actions-gh-pages][]
4. O site fica disponível em https://cursos.leg.ufpr.br/bayes2

### Para gerar o site localmente

O site é todo construído usando apenas o [R Markdown][], por isso, o
código fonte está nos arquivos `Rmd`. Para gerar o site você precisará
das versões mais recentes dos pacotes `rmarkdown` e `knitr`.

1. Copie (clone ou fork) esse repositório
2. Abra o R na raíz e renderize o site com `render_site()`

```r
library(rmarkdown)
render_site()
```

[Licença Creative Commons 4.0]: https://creativecommons.org/licenses/by-nc-sa/4.0/deed.pt_BR
[R Markdown]: http://rmarkdown.rstudio.com
[GitHub Actions]: https://docs.github.com/en/actions
[GitHub Pages]: https://pages.github.com
[peaceiris/actions-gh-pages]: https://github.com/peaceiris/actions-gh-pages
