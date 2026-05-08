# TuneBoostTreeBayesian

`TuneBoostTreeBayesian` agora é um **pacote R** para ajuste Bayesiano de hiperparâmetros de árvores boosted binárias com XGBoost ou LightGBM. O pacote mantém validação cruzada estratificada, cache de folds, early stopping, warm start, predição final e backends de PR-AUC em C, Fortran, Rfast ou R base.

## Estrutura do pacote

```text
TuneBoostTreeBayesian/
├── DESCRIPTION
├── NAMESPACE
├── R/
│   └── TuneBoostTreeBayesian.R
├── man/
│   ├── FitBoostTreeModel.Rd
│   ├── PerformanceBoostTreeModel.Rd
│   ├── PredictBoostTreeModel.Rd
│   ├── SplitDataBoostTreeFolds.Rd
│   ├── TuneBoostTreeBayesian-package.Rd
│   └── TuneBoostTreeBayesian.Rd
├── src/
│   ├── init.c
│   ├── tbtb_native.c
│   └── tbtb_native.f90
└── README.md
```

## O que foi otimizado

1. **Sem recriar dados dentro da função objetivo**: os folds estratificados e objetos nativos (`xgb.DMatrix` ou `lgb.Dataset`) são criados uma única vez antes da otimização Bayesiana.
2. **Sem oversubscription de CPU**: `nThreads` e `nWorkersFolds` são combinados para limitar o total de threads usadas por workers paralelos.
3. **Cache por hiperparâmetro**: configurações equivalentes normalizadas não são reavaliadas.
4. **Early stopping agressivo no tuning**: `nRoundsTuning` é separado de `nRoundsFinal`, evitando treinar milhares de rounds durante busca.
5. **PR-AUC com múltiplos backends seguros**:
   - `"c"`: implementação em C puro registrada como rotina nativa do pacote.
   - `"fortran"`: implementação em Fortran puro registrada como rotina nativa do pacote.
   - `"rfast"`: usa `Rfast::Order()` para acelerar a ordenação sem depender do código nativo do pacote.
   - `"r"`: fallback 100% base R.
   - `"auto"`: tenta C, depois Fortran, depois Rfast, depois R.

> Segurança: se um backend opcional não estiver disponível, o código cai automaticamente para uma alternativa portável em R em vez de interromper um tuning longo.

## Instalação passo a passo

### 1. Instale dependências R

```r
install.packages(c(
  "cli",
  "data.table",
  "Matrix",
  "rBayesianOptimization",
  "xgboost",
  "Rfast",
  "modeldata",
  "rsample"
))
```

Para LightGBM, siga a instalação recomendada do pacote `lightgbm` para sua plataforma.

### 2. Compile e instale o pacote

A partir do diretório acima do repositório:

```bash
R CMD build tune_boost_tree_bayesian
R CMD INSTALL TuneBoostTreeBayesian_0.1.0.tar.gz
```

Ou, durante desenvolvimento, a partir da raiz do repositório:

```bash
R CMD INSTALL .
```

Como o pacote contém `src/tbtb_native.c` e `src/tbtb_native.f90`, o `R CMD INSTALL` compila os backends C e Fortran automaticamente quando o toolchain do R está disponível.

### 3. Valide a instalação

```bash
R CMD check TuneBoostTreeBayesian_0.1.0.tar.gz
```

## Processo de uso recomendado

1. Carregue o pacote com `library(TuneBoostTreeBayesian)`.
2. Prepare uma base binária com preditores numéricos.
3. Rode `TuneBoostTreeBayesian()` com `prAucBackend = "auto"` para usar o backend mais rápido disponível.
4. Treine o modelo final com `FitBoostTreeModel()` usando `resultado$bestHyperparameters`.
5. Avalie em holdout com `PerformanceBoostTreeModel()`.
6. Reaproveite `resultado$initGridDt` em novas execuções para warm start.

## Exemplo completo com base binária do `modeldata`

O exemplo usa `modeldata::attrition`, cuja variável `Attrition` é binária. Para manter a entrada compatível com a implementação atual, o exemplo seleciona preditores numéricos.

```r
library(TuneBoostTreeBayesian)
library(modeldata)
library(rsample)

data(attrition, package = "modeldata")

set.seed(2026)
split <- initial_split(attrition, prop = 0.8, strata = Attrition)
train_data <- training(split)
test_data <- testing(split)

numeric_predictors <- names(train_data)[vapply(train_data, is.numeric, logical(1))]
formula_attrition <- reformulate(numeric_predictors, response = "Attrition")

resultado <- TuneBoostTreeBayesian(
  formula = formula_attrition,
  dataTrain = train_data,
  nFolds = 5,
  initPoints = 5,
  nIter = 10,
  nRoundsTuning = 250,
  earlyStoppingRounds = 15,
  nThreads = parallel::detectCores(logical = TRUE),
  nWorkersFolds = 1,
  engine_boost_tree = "xgboost",
  prAucBackend = "auto",
  verbose = TRUE
)

resultado$bestHyperparameters
resultado$bestScore

modelo_final <- FitBoostTreeModel(
  formula = formula_attrition,
  dataTrain = train_data,
  hyperparameters = resultado$bestHyperparameters,
  nThreads = parallel::detectCores(logical = TRUE),
  engine_boost_tree = "xgboost"
)

avaliacao <- PerformanceBoostTreeModel(
  modelObj = modelo_final,
  testData = test_data,
  formula = formula_attrition
)

avaliacao$prAuc
avaliacao$confusionSummary
head(avaliacao$predictions)
```

## Como escolher backend de PR-AUC

| Backend | Quando usar | Observação de segurança |
| --- | --- | --- |
| `auto` | Produção e uso geral | Escolhe a melhor opção disponível sem falhar por ausência de backend opcional. |
| `c` | Máximo desempenho em scoring repetido | Usa a rotina C registrada no pacote quando o pacote foi compilado. |
| `fortran` | Ambientes HPC ou comparação com toolchain Fortran | Usa a rotina Fortran registrada no pacote quando o pacote foi compilado. |
| `rfast` | Quando `Rfast` já está instalado | Cai para `r` se o pacote não estiver instalado. |
| `r` | Portabilidade e depuração | Mais lento, porém sem dependências opcionais. |

## Observações de desempenho

- Para bases pequenas, `nWorkersFolds = 1` costuma ser mais rápido porque evita overhead de paralelização.
- Para bases maiores, aumente `nWorkersFolds`, mas mantenha `nThreads * nWorkersFolds` próximo ao total de cores físicos/lógicos disponíveis.
- Use `initGridDt = resultado$initGridDt` em novas execuções para continuar a busca Bayesiana sem perder avaliações já realizadas.
- Prefira reduzir `nRoundsTuning` e usar `earlyStoppingRounds` em vez de aumentar rounds cegamente durante a otimização.
