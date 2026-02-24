# CE-062(C) Tópicos: Modelagem Bayesiana


## Inferência Bayesiana

Repositório da disciplina optativa **Modelagem Bayesiana**, ministrada
na UFPR, para o curso de Estatística e Ciência de Dados.

Este repositório contém materiais de aula e os arquivos
necessários para gerar a página da disciplina, disponível em:
http://www.leg.ufpr.br/bayes2

### Para gerar o site () verificar se ainda funciona

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
