# TuneBoostTreeBayesian

`TuneBoostTreeBayesian` é um pacote R para ajuste Bayesiano de hiperparâmetros de árvores boosted binárias com XGBoost ou LightGBM. A API principal foi redesenhada para ficar curta e segura: os detalhes de validação cruzada, Limbo, paralelismo, balanceamento, scoring e engine ficam em funções auxiliares que retornam listas documentáveis.

## API principal

```r
resultado <- TuneBoostTreeBayesian(
  formula,
  data,
  initial = 10L,
  nIter = 30L,
  engine = "xgboost"
)
```

Argumentos principais:

- `formula`: fórmula binária, por exemplo `classe ~ x1 + x2`; a resposta deve ser `factor` com exatamente dois níveis.
- `data`: `data.frame`, `tibble` ou `data.table` com resposta e preditores. A classe positiva é a classe rara; em empate, é o primeiro nível de `levels(target)`.
- `initial`: `NULL`, inteiro com número de pontos aleatórios iniciais, ou tabela (`data.frame`, `tibble`, `data.table`) com histórico de avaliações.
- `nIter`: número de iterações de otimização após a inicialização.
- `engine`: `"xgboost"`, `"lightgbm"`, `TuneBoostTreeXgboost()` ou `TuneBoostTreeLightgbm()`.

Os nomes públicos de hiperparâmetros seguem `parsnip::boost_tree()` sempre que aplicável: `trees`, `tree_depth`, `min_n`, `loss_reduction`, `sample_size`, `mtry`, `learn_rate` e `stop_iter`. O espaço de busca também aceita regularização e amostragem adicionais: `lambda`, `alpha`, `max_delta_step`, `colsample_bytree`, `colsample_bylevel`, `num_leaves`, `min_data_in_leaf` e `scale_pos_weight`.

## Instalação

```r
install.packages(c(
  "cli",
  "data.table",
  "Matrix",
  "xgboost",
  "Rfast",
  "modeldata",
  "rsample"
))
```

Para LightGBM, siga a instalação recomendada do pacote `lightgbm`. Para Limbo, compile um executável ask/tell baseado em [`resibots/limbo`](https://github.com/resibots/limbo) e configure `TBTB_LIMBO_COMMAND`.

```bash
R CMD INSTALL .
```

## Cenário 1: uso padrão seguro

```r
resultado <- TuneBoostTreeBayesian(
  formula = Attrition ~ Age + DailyRate + DistanceFromHome + MonthlyIncome,
  data = train_data,
  initial = 10L,
  nIter = 30L
)
```

Esse cenário usa:

- XGBoost com `tree_method = "hist"`.
- Limbo se `TBTB_LIMBO_COMMAND` estiver configurado.
- Fallback interno seguro se Limbo não estiver disponível.
- `parallel = "auto"`.
- PR-AUC backend `"auto"`.
- `scale_pos_weight = "auto"`.

## Cenário 2: warm start com tibble ou data.table

```r
resultado_2 <- TuneBoostTreeBayesian(
  formula = formula_attrition,
  data = train_data,
  initial = resultado_1$initial,
  nIter = 20L
)
```

Quando `initial` é tabular, a tabela deve conter as colunas de parâmetros (`learn_rate`, `tree_depth`, `min_n`, `sample_size`, `mtry`, `loss_reduction`, `max_bin`) e `Value`.

## Cenário 3: configuração explícita de boosting

```r
resultado <- TuneBoostTreeBayesian(
  formula = formula_attrition,
  data = train_data,
  boost = TuneBoostTreeBoostParams(
    trees = 750L,
    stop_iter = 25L
  ),
  search_space = TuneBoostTreeSearchSpace(
    learn_rate = c(0.005, 0.12),
    tree_depth = c(2L, 8L),
    min_n = c(2L, 60L),
    loss_reduction = c(0, 6),
    sample_size = c(0.55, 1),
    mtry = c(0.25, 1),
    max_bin = c(64L, 384L),
    lambda = c(0, 10),
    alpha = c(0, 5),
    colsample_bytree = c(0.5, 1)
  ),
  nIter = 40L
)
```

## Cenário 4: Limbo estrito em produção/HPC

```r
resultado <- TuneBoostTreeBayesian(
  formula = formula_attrition,
  data = train_data,
  optimizer = TuneBoostTreeLimbo(
    command = Sys.getenv("TBTB_LIMBO_COMMAND"),
    fallback = FALSE,
    acquisition = "ucb",
    kappa = 2.576,
    eps = 0
  ),
  control = TuneBoostTreeControl(parallel = "auto", verbose = TRUE),
  initial = 20L,
  nIter = 60L
)
```

Com `fallback = FALSE`, a execução falha antes da CV se o executável Limbo não estiver configurado ou não for executável.

## Cenário 5: balanceamento com argumentos exclusivos

Toda configuração que recebe uma função expõe `...` para parâmetros exclusivos dessa função. O balanceamento é chamado uma vez por fold como `balance_fn(data, formula, ...)`.

```r
meu_balanceador <- function(data, formula, target_ratio = 0.5, seed = 1L) {
  set.seed(seed)
  data
}

resultado <- TuneBoostTreeBayesian(
  formula = formula_attrition,
  data = train_data,
  imbalance = TuneBoostTreeImbalance(
    balance_fn = meu_balanceador,
    scale_pos_weight = "auto",
    target_ratio = 0.7,
    seed = 2026L
  )
)
```

`scale_pos_weight` aceita e também pode ser exposto no `TuneBoostTreeSearchSpace()` quando houver validação suficiente para otimizar esse peso:

- `"auto"`: calcula `qtd_negativa / qtd_positiva` conforme a classe positiva selecionada; quando há balanceamento, o cálculo ocorre após o balanceamento de cada fold.
- `numeric(1)`: usa peso fixo, com ou sem balanceamento.
- `NULL`: não usa peso de classe.

## Cenário 6: paralelismo automático ou explícito

```r
resultado_auto <- TuneBoostTreeBayesian(
  formula = formula_attrition,
  data = train_data,
  control = TuneBoostTreeControl(parallel = "auto")
)

resultado_manual <- TuneBoostTreeBayesian(
  formula = formula_attrition,
  data = train_data,
  control = TuneBoostTreeControl(
    parallel = TuneBoostTreeParallel(
      workers = 4L,
      threads_per_worker = 2L,
      strategy = "folds"
    )
  )
)
```

`parallel = "auto"` prioriza segurança: detecta cores, evita oversubscription, usa execução sequencial para bases pequenas e distribui folds quando há ganho provável.

## Cenário 7: ultra otimizado

```r
resultado <- TuneBoostTreeBayesianUltra(
  formula = formula_attrition,
  data = train_data,
  initial = 20L,
  nIter = 60L,
  command = Sys.getenv("TBTB_LIMBO_COMMAND"),
  strict_limbo = TRUE
)
```

Esse cenário usa orçamento maior, Limbo obrigatório, PR-AUC compilado quando disponível, `parallel = "auto"`, `scale_pos_weight = "auto"`, limiar de decisão otimizado por F1 out-of-fold e limites de busca mais amplos.

## Interface R ⇄ Limbo

A ponte usa protocolo de arquivos para manter o objetivo de CV no R e deixar a proposta Bayesiana no executável C++:

1. O R cria diretório temporário por iteração.
2. Escreve `bounds.csv`, `observations.csv` e `config.csv`.
3. Chama `limboCommand bounds.csv observations.csv config.csv candidate.csv`.
4. O executável escreve exatamente uma linha em `candidate.csv` com `learn_rate`, `tree_depth`, `min_n`, `sample_size`, `mtry`, `loss_reduction` e `max_bin`.
5. O R valida finitude, limites e inteiros antes de executar CV.

Veja `inst/limbo/README.md` para o contrato completo.

## Referências científicas e de bibliotecas

- Cully, A., Chatzilygeroudis, K., Allocati, F., & Mouret, J.-B. (2018). **Limbo: A Fast and Flexible Library for Bayesian Optimization**. *Journal of Open Source Software*, 3(26), 545. DOI: 10.21105/joss.00545. Projeto: <https://github.com/resibots/limbo>.
- Chen, T., & Guestrin, C. (2016). **XGBoost: A Scalable Tree Boosting System**. *KDD 2016*. <https://arxiv.org/abs/1603.02754>.
- Ke, G. et al. (2017). **LightGBM: A Highly Efficient Gradient Boosting Decision Tree**. *NeurIPS 2017*. <https://proceedings.neurips.cc/paper/2017/hash/6449f44a102fde848669bdd9eb6b76fa-Abstract.html>.
- Tidymodels/parsnip. **Boosted trees — boost_tree**. <https://parsnip.tidymodels.org/reference/boost_tree.html>.
- Davis, J., & Goadrich, M. (2006). **The Relationship Between Precision-Recall and ROC Curves**. *ICML 2006*.

## Desenvolvimento e verificação

Consulte `CHECKLIST.md` para o checklist completo de revisão de implementação, documentação, segurança, performance, empacotamento e validação estatística.
