# TuneBoostTreeBayesian

`TuneBoostTreeBayesian` é um pacote R para ajuste Bayesiano de hiperparâmetros de árvores boosted binárias com LightGBM como engine padrão, XGBoost como engine alternativa obrigatória e `rBayesianOptimization` como otimizador padrão. O pacote foi preparado para execução dedicada em servidor Intel Xeon Platinum 8260 com 2 sockets NUMA, 48 cores físicos, 96 CPUs lógicas e grande capacidade de RAM. O padrão do pacote foi simplificado para otimizar exclusivamente os hiperparâmetros de `parsnip::boost_tree()` solicitados: `min_n`, `tree_depth`, `learn_rate`, `loss_reduction` e `sample_size`.

## API principal

```r
resultado <- TuneBoostTree(
  formula,
  data,
  initial = 20L,
  nIter = 60L,
  engine = "lightgbm"
)
```

Argumentos principais:

- `formula`: fórmula binária, por exemplo `classe ~ x1 + x2`; a resposta deve ser `factor` com exatamente dois níveis.
- `data`: `data.frame`, `tibble` ou `data.table` com resposta e preditores. A classe positiva é a classe rara; em empate, é o primeiro nível de `levels(target)`.
- `initial`: `NULL`, inteiro com número de pontos iniciais, ou tabela (`data.frame`, `tibble`, `data.table`) com histórico de avaliações.
- `nIter`: número de iterações Bayesianas após a inicialização.
- `engine`: `"lightgbm"` é o padrão principal; `"xgboost"`, `TuneBoostTreeLightgbm()` e `TuneBoostTreeXgboost()` continuam disponíveis.
- `optimizer`: por padrão usa `TuneBoostTreeOptimizerRBayesianOptimization()`; Limbo externo é opcional e deve ser escolhido explicitamente com `TuneBoostTreeOptimizerLimbo()`.

Por padrão, o espaço de busca contém somente `learn_rate`, `tree_depth`, `min_n`, `loss_reduction` e `sample_size`. Parâmetros como `mtry` e `max_bin` continuam aceitos como fixos em `TuneBoostTreeBoostParams()` ou como opcionais em `TuneBoostTreeSearchSpace()`, mas não são mais tunados por padrão para manter a busca mais rápida, estável e alinhada ao objetivo operacional. Em `TuneBoostTreeBoostParams()`, `mtry = "default"` fixa o uso de aproximadamente 80% das features por divisão/nó.

## Instalação

LightGBM e XGBoost são dependências obrigatórias do pacote, e `rBayesianOptimization` é o otimizador padrão da função principal. Instale primeiro as dependências R obrigatórias:

```r
install.packages(c(
  "cli",
  "data.table",
  "Matrix",
  "lightgbm",
  "xgboost",
  "rBayesianOptimization"
), repos = "https://cran.r-project.org")
```

Dependências úteis para exemplos, métricas externas, relatórios e backends opcionais:

```r
install.packages(c(
  "Rfast",
  "modeldata",
  "rsample",
  "yardstick",
  "tibble",
  "testthat",
  "knitr",
  "rmarkdown"
), repos = "https://cran.r-project.org")
```

Depois instale o pacote localmente:

```bash
R CMD INSTALL .
```

### Instalação opcional do Limbo externo

O uso padrão não exige Limbo: `TuneBoostTree()` usa LightGBM com `TuneBoostTreeOptimizerRBayesianOptimization()` por padrão. Se quiser usar o Limbo C++ como otimizador externo ask/tell, rode o script incluído no pacote:

```bash
./inst/scripts/install_limbo.sh
```

O script clona/compila `https://github.com/resibots/limbo`, instala dependências de build em sistemas com `apt-get` e configura `TBTB_LIMBO_ROOT`, `TBTB_LIMBO_COMMAND` e `TBTB_LIMBO_TIMEOUT` em `~/.Renviron` e `~/.profile`. Exemplo com prefixo explícito:

```bash
./inst/scripts/install_limbo.sh \
  --prefix /opt/tbtb-limbo \
  --adapter-command /opt/tbtb-limbo/bin/tbtb-limbo-ask
```

Importante: Limbo é a biblioteca C++; o pacote R chama um executável externo compatível com o contrato `tbtb-limbo-ask bounds.csv observations.csv config.csv candidate.csv`. Esse adaptador deve existir no caminho configurado em `TBTB_LIMBO_COMMAND` ou ser passado em `TuneBoostTreeOptimizerLimbo(command = "/caminho/tbtb-limbo-ask")`. Se o Limbo externo for selecionado com `fallback = TRUE` e falhar, o pacote usa o otimizador interno seguro.

## Cenário 1: uso padrão seguro e rápido

```r
resultado <- TuneBoostTree(
  formula = Attrition ~ Age + DailyRate + DistanceFromHome + MonthlyIncome,
  data = train_data,
  initial = 20L,
  nIter = 60L,
  engine = "lightgbm"
)
```

Esse cenário usa:

- LightGBM com métrica `average_precision` como engine principal; XGBoost permanece disponível com `engine = "xgboost"`.
- `rBayesianOptimization` como otimizador padrão da busca Bayesiana.
- Busca Bayesiana sobre `learn_rate`, `tree_depth`, `min_n`, `loss_reduction` e `sample_size`.
- Limbo externo somente quando configurado explicitamente via `TuneBoostTreeOptimizerLimbo()`.
- `parallel = "auto"`, dividindo folds e threads para evitar oversubscription.
- PR-AUC backend `"auto"`.
- `scale_pos_weight = "auto"`.

## Cenário 2: warm start com tibble ou data.table

```r
resultado_2 <- TuneBoostTree(
  formula = formula_attrition,
  data = train_data,
  initial = resultado_1$initial,
  nIter = 40L
)
```

Quando `initial` é tabular, a tabela deve conter `learn_rate`, `tree_depth`, `min_n`, `sample_size`, `loss_reduction` e `Value`. Colunas opcionais antigas são ignoradas quando não fazem parte do espaço de busca atual.

## Cenário 3: configuração explícita de boosting

```r
resultado <- TuneBoostTree(
  formula = formula_attrition,
  data = train_data,
  boost = TuneBoostTreeBoostParams(
    trees = 1000L,
    stop_iter = 30L,
    mtry = 1,
    max_bin = 256L
  ),
  searchSpace = TuneBoostTreeSearchSpace(
    learn_rate = c(0.005, 0.2),
    tree_depth = c(2L, 12L),
    min_n = c(1L, 80L),
    loss_reduction = c(0, 8),
    sample_size = c(0.55, 1)
  ),
  initial = 20L,
  nIter = 60L
)
```

## Cenário 4: Limbo opcional estrito em produção/HPC

```r
resultado <- TuneBoostTree(
  formula = formula_attrition,
  data = train_data,
  optimizer = TuneBoostTreeOptimizerLimbo(
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

Com `fallback = FALSE`, a execução falha antes da CV se o executável Limbo não estiver configurado ou não for executável. O protocolo externo recebe `bounds.csv`, `observations.csv`, `config.csv` e deve escrever exatamente uma linha em `candidate.csv`.

## Cenário 5: balanceamento com argumentos exclusivos

Toda configuração que recebe uma função expõe `...` para parâmetros exclusivos dessa função. O balanceamento é chamado uma vez por fold como `balanceFn(data, formula, ...)`.

```r
meu_balanceador <- function(data, formula, target_ratio = 0.5, seed = 1L) {
  set.seed(seed)
  data
}

resultado <- TuneBoostTree(
  formula = formula_attrition,
  data = train_data,
  imbalance = TuneBoostTreeImbalance(
    balanceFn = meu_balanceador,
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
resultado_auto <- TuneBoostTree(
  formula = formula_attrition,
  data = train_data,
  control = TuneBoostTreeControl(parallel = "auto")
)

resultado_manual <- TuneBoostTree(
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

`parallel = "auto"` prioriza segurança: detecta o orçamento de cores físicos, evita oversubscription, usa execução sequencial para bases pequenas e distribui folds quando há ganho provável.

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
4. O executável escreve exatamente uma linha em `candidate.csv` com os parâmetros ativos; no perfil padrão são `learn_rate`, `tree_depth`, `min_n`, `loss_reduction` e `sample_size`.
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

## Uso direto das funções Fit, Split e Predict

As funções públicas `FitBoostTreeModel()`, `PredictBoostTreeModel()` e `SplitDataBoostTreeFolds()` são exportadas no pacote e podem ser chamadas diretamente após `library(TuneBoostTreeBayesian)`. O fluxo recomendado é:

1. usar `TuneBoostTree()` para encontrar hiperparâmetros;
2. treinar o modelo final com `FitBoostTreeModel()` usando `resultado$bestHyperparameters` — por padrão em LightGBM, ou com `engine_boost_tree = "xgboost"` para a alternativa;
3. gerar predições com `PredictBoostTreeModel()`;
4. calcular métricas externas, por exemplo com `yardstick`.

## Distribuição compilada do pacote

O pacote é distribuível como pacote R binário/source padrão. O `DESCRIPTION` define `ByteCompile: true`, então o código R é byte-compilado na instalação, e as rotinas nativas C/Fortran em `src/` são compiladas na biblioteca compartilhada do pacote. O arquivo-fonte `R/TuneBoostTreeBayesian.R` continua no repositório para auditoria, manutenção e reinstalação reproduzível. Para ativar JIT nível 3 na sessão de execução, use `compiler::enableJIT(3)` antes de carregar/rodar o pacote; isso é mantido explícito para evitar alterar configurações globais do R sem consentimento.

Para criar um artefato de instalação local no servidor dedicado:

```bash
R CMD build .
R CMD INSTALL TuneBoostTreeBayesian_*.tar.gz
```

Para verificar o pacote antes de distribuir internamente:

```bash
R CMD check --no-manual TuneBoostTreeBayesian_*.tar.gz
```

## Checklist de conferência

O repositório contém `CHECKLIST.md` com os itens esperados de validação de API, dados, CV, ponte Limbo, paralelismo, engines, métricas, documentação e checks de pacote. Para conferência local completa, rode:

```bash
Rscript -e 'testthat::test_dir("tests/testthat")'
R CMD build .
R CMD check --no-manual TuneBoostTreeBayesian_*.tar.gz
```

## Exemplo completo: LightGBM principal com Limbo externo/fake e métricas yardstick

O exemplo abaixo usa `modeldata::two_class_dat` em formato tibble, `rsample::initial_split()`, tuning, fit final, predição holdout e métricas `yardstick`. O Limbo real pode ser usado trocando `fake_limbo` pelo caminho de `tbtb-limbo-ask`.

```r
library(TuneBoostTreeBayesian)
library(modeldata)
library(rsample)
library(tibble)
library(yardstick)

data(two_class_dat, package = "modeldata")
split <- initial_split(as_tibble(two_class_dat), prop = 0.75, strata = "Class")
train_data <- training(split)
test_data <- testing(split)

fake_limbo <- tempfile("fake-limbo-")
writeLines(c(
  "#!/bin/sh",
  "candidate=\"$4\"",
  "printf 'learn_rate,tree_depth,min_n,loss_reduction,sample_size\\n0.05,4,8,0,0.8\\n' > \"$candidate\""
), fake_limbo)
Sys.chmod(fake_limbo, "0755")

resultado <- TuneBoostTree(
  Class ~ A + B,
  data = train_data,
  engine = "lightgbm",
  initial = 2L,
  nIter = 1L,
  boost = TuneBoostTreeBoostParams(trees = 12L, stop_iter = 3L, mtry = 1, max_bin = 64L),
  searchSpace = TuneBoostTreeSearchSpace(
    learn_rate = c(0.03, 0.12),
    tree_depth = c(2L, 4L),
    min_n = c(1L, 12L),
    loss_reduction = c(0, 2),
    sample_size = c(0.7, 1)
  ),
  cv = TuneBoostTreeCv(folds = 2L),
  optimizer = TuneBoostTreeOptimizerLimbo(command = fake_limbo, fallback = FALSE),
  control = TuneBoostTreeControl(parallel = FALSE, verbose = FALSE)
)

modelo <- FitBoostTreeModel(
  Class ~ A + B,
  train_data,
  resultado$bestHyperparameters,
  nThreads = 1L,
  engine_boost_tree = "lightgbm"
)

pred <- PredictBoostTreeModel(modelo, test_data)
metric_data <- data.frame(
  truth = factor(test_data$Class, levels = modelo$targetLevels),
  estimate = factor(pred$predictedClass, levels = modelo$targetLevels),
  probability = pred$probabilitySecondClass
)

pr_auc(metric_data, truth, probability, event_level = "second")
roc_auc(metric_data, truth, probability, event_level = "second")
sens(metric_data, truth, estimate, event_level = "second")
spec(metric_data, truth, estimate, event_level = "second")
accuracy(metric_data, truth, estimate)
bal_accuracy(metric_data, truth, estimate, event_level = "second")
```

## Exemplo completo: LightGBM principal com rBayesianOptimization

```r
optimizer_rbo <- TuneBoostTreeOptimizerRBayesianOptimization(acquisition = "ucb", kappa = 2.576, eps = 0)

resultado_lgb_rbo <- TuneBoostTree(
  Class ~ A + B,
  data = train_data,
  engine = "lightgbm",
  initial = 2L,
  nIter = 1L,
  boost = TuneBoostTreeBoostParams(trees = 12L, stop_iter = 3L, mtry = 1, max_bin = 64L),
  searchSpace = TuneBoostTreeSearchSpace(
    learn_rate = c(0.03, 0.12), tree_depth = c(2L, 4L), min_n = c(1L, 12L),
    loss_reduction = c(0, 2), sample_size = c(0.7, 1)
  ),
  cv = TuneBoostTreeCv(folds = 2L),
  optimizer = optimizer_rbo,
  control = TuneBoostTreeControl(parallel = FALSE, verbose = FALSE)
)
```

## Exemplo completo: XGBoost alternativo com Limbo externo/fake

```r
resultado_xgb_limbo <- TuneBoostTree(
  Class ~ A + B,
  data = train_data,
  engine = "xgboost",
  initial = 2L,
  nIter = 1L,
  boost = TuneBoostTreeBoostParams(trees = 12L, stop_iter = 3L, mtry = 1, max_bin = 64L),
  searchSpace = TuneBoostTreeSearchSpace(
    learn_rate = c(0.03, 0.12), tree_depth = c(2L, 4L), min_n = c(1L, 12L),
    loss_reduction = c(0, 2), sample_size = c(0.7, 1)
  ),
  cv = TuneBoostTreeCv(folds = 2L),
  optimizer = TuneBoostTreeOptimizerLimbo(command = fake_limbo, fallback = FALSE),
  control = TuneBoostTreeControl(parallel = FALSE, verbose = FALSE)
)

modelo_xgb <- FitBoostTreeModel(
  Class ~ A + B,
  train_data,
  resultado_xgb_limbo$bestHyperparameters,
  nThreads = 1L,
  engine_boost_tree = "xgboost"
)
```

## Exemplo completo: XGBoost alternativo com rBayesianOptimization

```r
resultado_xgb_rbo <- TuneBoostTree(
  Class ~ A + B,
  data = train_data,
  engine = "xgboost",
  initial = 2L,
  nIter = 1L,
  boost = TuneBoostTreeBoostParams(trees = 12L, stop_iter = 3L, mtry = 1, max_bin = 64L),
  searchSpace = TuneBoostTreeSearchSpace(
    learn_rate = c(0.03, 0.12), tree_depth = c(2L, 4L), min_n = c(1L, 12L),
    loss_reduction = c(0, 2), sample_size = c(0.7, 1)
  ),
  cv = TuneBoostTreeCv(folds = 2L),
  optimizer = optimizer_rbo,
  control = TuneBoostTreeControl(parallel = FALSE, verbose = FALSE)
)
```
