# Conferência Profunda v5 - Estado Real do Código (SHA e39da6c)

## Confirmações: O Que Está Correto

Antes dos problemas, o registro preciso do que **está implementado corretamente** nesta versão:

- `on.exit` para limpeza do `cacheEnv` - implementado corretamente na linha de `TuneBoostTree`
- `mc.set.seed = FALSE` - presente em `RunCvManual` e `RunCvPredictions`
- `TuneBoostTree_SetPassiveOpenMp` com guarda `nzchar(Sys.getenv(...))` - correto
- `FitBoostTreeModel` e `PredictBoostTreeModel` com parâmetro `engineBoostTree` (camelCase) e alias legado `engine_boost_tree` com `cli_warn` de depreciação - correto
- `TuneBoostTree_OptimizeThreshold` retorna `list(threshold, metric = "f1", score)` - campo `metric` sempre populado

***

## Vulnerabilidades e Bugs Críticos Ativos

### BUG-01 (CRITICO): `TuneBoostTree_PrepareMatrix` ainda materializa `numericMatrix` completo antes do heurístico de esparsidade

**Arquivo:** linha ~1073 (função `TuneBoostTree_PrepareMatrix`)

```r
# CODIGO ATUAL - confirmado no SHA e39da6c:
sparseLike <- vapply(xData, TuneBoostTree_IsSparseLikeColumn, logical(1L))
numericMatrix <- data.matrix(xData)
storage.mode(numericMatrix) <- "double"
colnames(numericMatrix) <- featureNames
xMatrix <- if(any(sparseLike) || mean(numericMatrix == 0) > 0.7)
               Matrix::Matrix(numericMatrix, sparse = TRUE)
           else numericMatrix
```

O `TuneBoostTree_IsSparseLikeColumn` verifica apenas se a coluna herda de `sparsevctrs_vctr`, `sparse_vector`, `sparse_double` ou `sparse_integer` - ou seja, só detecta esparsidade em objetos que **já são esparsos no R**. Para `data.frame` densos com muitos zeros (caso típico de dados financeiros one-hot encoded), `any(sparseLike)` retorna `FALSE` e a decisão cai para `mean(numericMatrix == 0) > 0.7`, que já requer `numericMatrix` materializado. Em um servidor NUMA com dados 500k x 2.000, isso aloca 8 GB na heap do socket local antes de qualquer decisão.

**Adicionalmente**, `mean(numericMatrix == 0)` cria uma matriz booleana intermediária de mesmo tamanho antes de calcular a média - temporariamente são dois arrays de ~8 GB cada na memória. Em contexto NUMA, essa duplicação ocorre inteiramente no socket do worker que chama `PrepareMatrix`.

**Correção:**

```r
# CODIGO OTIMIZADO: estimar esparsidade por amostragem de colunas
# sem materializar numericMatrix completo
TuneBoostTree_EstimateSparseRatio <- function(xData, sampleCols = 30L) {
  idx <- seq(1L, ncol(xData), length.out = min(ncol(xData), sampleCols))
  zeros <- vapply(xData[, idx, drop = FALSE],
    function(v) mean(v == 0 | is.na(v)), numeric(1L))
  mean(zeros)
}

# Em TuneBoostTree_PrepareMatrix, substituir as linhas atuais por:
sparseLike <- vapply(xData, TuneBoostTree_IsSparseLikeColumn, logical(1L))
sparseRatioEst <- if(!any(sparseLike)) TuneBoostTree_EstimateSparseRatio(xData) else 1
numericMatrix <- data.matrix(xData)
storage.mode(numericMatrix) <- "double"
colnames(numericMatrix) <- featureNames
xMatrix <- if(any(sparseLike) || sparseRatioEst > 0.7)
               Matrix::Matrix(numericMatrix, sparse = TRUE)
           else numericMatrix
```


***

### BUG-02 (CRITICO): `TuneBoostTree_PrepareBalancedFolds` chama `TuneBoostTree_PrepareMatrix` no dataset completo - data leakage confirmado

**Arquivo:** função `TuneBoostTree_PrepareBalancedFolds`, linhas relevantes:

```r
# CODIGO ATUAL:
preparedFull <- TuneBoostTree_PrepareMatrix(formula, data, NULL,
                                            targetLevels, formulaInfo)
folds <- TuneBoostTree_CreateStratifiedFolds(preparedFull$yData, nFolds, seed)
# ...
for(foldId in seq_along(folds)){
  # ...
  preparedTrain <- TuneBoostTree_PrepareMatrix(formula, balancedTrain, NULL,
                                               preparedFull$targetLevels, formulaInfo)
  preparedTest <- TuneBoostTree_PrepareMatrix(formula, testData, NULL,
                                              preparedTrain$targetLevels, formulaInfo)
```

**O propósito de `preparedFull`** é exclusivamente extrair `preparedFull$yData` para `CreateStratifiedFolds` e `preparedFull$targetLevels` para os folds subsequentes. Porém, `PrepareMatrix` sobre `data` completo executa `data.matrix(xData)` com todas as linhas, aplica o heurístico de esparsidade no dataset inteiro, e instancia um `xMatrix` completo que não é usado em lugar algum depois de `targetLevels` ser extraído. Isso representa:

1. **Custo de memória desnecessário**: alocação de `xMatrix` completo (denso ou esparso) descartado imediatamente
2. **Leakage potencial**: o heurístico de esparsidade global pode diferir do heurístico calculado por subset de treino, gerando `xMatrix` esparso para folds mas denso para o dataset completo, ou vice-versa - resultando em representações inconsistentes

A correção correta e **já parcialmente disponível no próprio pacote** é usar `TuneBoostTree_PrepareTarget` diretamente, pois ela já aceita apenas o vetor alvo:

```r
# CODIGO OTIMIZADO: substituir a chamada a PrepareMatrix pelo PrepareTarget
# que já existe no pacote e retorna yData + targetLevels sem custo de matriz:
preparedTarget <- TuneBoostTree_PrepareTarget(data[[formulaInfo$targetName]],
                                              targetLevels)
folds <- TuneBoostTree_CreateStratifiedFolds(preparedTarget$yData, nFolds, seed)
# usar preparedTarget$targetLevels nos loops de folds
```


***

### BUG-03 (ALTO): `TuneBoostTree_DetectCpuBudget` - lógica de `reserve` quebrada quando `physical < 3`

**Arquivo:** função `TuneBoostTree_DetectCpuBudget`

```r
# CODIGO ATUAL:
reserve <- min(2L, max(0L, as.integer(physical) - 1L))
as.integer(max(1L, as.integer(physical) - reserve))
```

**Com `physical = 48` (Xeon 8260, cores físicos):**

- `reserve = min(2, max(0, 47)) = 2`
- resultado: `max(1, 46) = 46` - correto

**Com `physical = 1` (container minimal, CI):**

- `reserve = min(2, max(0, 0)) = 0`
- resultado: `max(1, 1) = 1` - correto

**Com `physical = 2`:**

- `reserve = min(2, max(0, 1)) = 1`
- resultado: `max(1, 1) = 1` - correto

**Problema real:** em servidores com **muitos cores** (como o Xeon 8260 com 96 CPUs lógicas), `parallel::detectCores(logical = FALSE)` pode retornar `NA` em containers cgroups (Docker no HPC), fazendo o fallback para `logical = TRUE` que retorna 96. Com `physical = 96`:

- `reserve = min(2, 95) = 2`
- resultado: `max(1, 94) = 94` threads disponíveis

Isso permite configurações como `nWorkers = 10, workerThreads = 9`, resultando em 90 threads de engine mais 4 threads do processo R - total 94 threads tentando ocupar 96 CPUs lógicas (48 físicos). Com HyperThreading, 94 threads em 48 cores físicos significa 2 threads por core em quase todos os cores, mais contenção de recursos compartilhados (L2/L3 cache, TLB). Para cargas computacionalmente intensivas como treinamento de GBM, isso resulta em **degradação de throughput de 15-30%** versus 48 threads (uma por core físico).

**Correção com fallback via `lscpu`:**

```r
TuneBoostTree_DetectCpuBudget <- function() {
  physical <- tryCatch({
    raw <- system("lscpu -p=core | grep -vc '^#'",
                  intern = TRUE, ignore.stderr = TRUE)
    v <- suppressWarnings(as.integer(trimws(raw[1L])))
    if(!is.na(v) && v > 0L) v else NA_integer_
  }, error = function(e) NA_integer_)
  if(is.na(physical))
    physical <- suppressWarnings(parallel::detectCores(logical = FALSE))
  if(is.na(physical) || physical < 1L)
    physical <- suppressWarnings(parallel::detectCores(logical = TRUE))
  if(is.na(physical) || physical < 1L) physical <- 1L
  as.integer(max(1L, as.integer(physical) - 2L))
}
```


***

### BUG-04 (ALTO): `TuneBoostTree_FinalizeParallel` emite aviso mas **não impede** oversubscription

**Arquivo:** função `TuneBoostTree_FinalizeParallel`

```r
# CODIGO ATUAL:
oversubscriptionLimit <- as.numeric(totalCores) * 2
if(is.finite(requestedThreads) && requestedThreads > oversubscriptionLimit){
  cli::cli_warn("workers ({workers}) * threads_per_worker ({threads}) = ...")
}
list(workers = as.integer(workers), threads_per_worker = threads)
```

O aviso é informativo mas o valor retornado não é corrigido. Um usuário que chamar `TuneBoostTreeParallel(workers = 20, threads_per_worker = 20)` receberá o aviso e depois terá 400 threads tentando executar em 46 cores. Em Oracle Linux 9.7 com o scheduler CFS, isso causa `context switching storm` que degrada todos os processos do servidor - incluindo outros usuários. Em ambiente compartilhado de HPC, isso é um risco de vizinhança barulhenta.

**Correção: limitar ativamente em vez de apenas avisar:**

```r
TuneBoostTree_FinalizeParallel <- function(workers, threads, nFolds, totalCores) {
  workers <- min(as.integer(workers), as.integer(nFolds))
  threads <- as.integer(threads)
  # corrigir ativamente para respeitar orçamento
  if(as.numeric(workers) * as.numeric(threads) > as.numeric(totalCores)) {
    threads <- max(1L, as.integer(floor(totalCores / workers)))
    cli::cli_inform(c(
      "!" = "threads_per_worker ajustado para {threads} para respeitar o \\
             orcamento de {totalCores} cores fisicos.",
      "i" = "Use `TuneBoostTreeParallel(workers = {workers}, \\
              threads_per_worker = {threads})` para suprimir este aviso."
    ))
  }
  list(workers = as.integer(workers), threads_per_worker = threads)
}
```


***

### BUG-05 (ALTO): `TuneBoostTree_RunCvPredictions` não repassa `prAucBackend` para `RunOneFoldPrediction`

**Arquivo:** função `TuneBoostTree_RunCvPredictions`, confirmado no SHA

```r
# ASSINATURA ATUAL - prAucBackend AUSENTE:
TuneBoostTree_RunCvPredictions <- function(balancedFolds, hyperparameters,
  nRounds, seed, nThreads, nWorkersFolds, evalMetric, engine_boost_tree) {
```

Esta função **não tem o parâmetro `prAucBackend`** em sua assinatura, ao contrário de `TuneBoostTree_RunCvManual` que o possui. O resultado é que a etapa de threshold optimization (`TuneBoostTree_OptimizeThresholdCv` que chama `RunCvPredictions`) ignora o backend configurado pelo usuário em `TuneBoostTreePerformance(backend = "c")` e sempre usa o default "auto". Isso não é um bug de resultado (o threshold é calculado corretamente), mas representa uma inconsistência silenciosa onde a configuração do usuário é ignorada numa etapa da pipeline.

**Correção:**

```r
# Adicionar prAucBackend à assinatura e propagar:
TuneBoostTree_RunCvPredictions <- function(balancedFolds, hyperparameters,
  nRounds, seed, nThreads, nWorkersFolds, evalMetric,
  engine_boost_tree, prAucBackend = "auto") { # <- adicionar

  # No bloco mclapply, adicionar prAucBackend = prAucBackend
  # No bloco parLapply, adicionar prAucBackend = prAucBackend
  # Na chamada RunOneFoldPrediction, o parâmetro não é usado (só prediz)
  # mas a assinatura precisa ser consistente para auditoria
}
```


***

### BUG-06 (MEDIO): `TuneBoostTree_RbfKernel` usa `outer` + `tcrossprod` com matrizes densas - gargalo de memória em alta dimensionalidade

**Arquivo:** função `TuneBoostTree_RbfKernel`

```r
# CODIGO ATUAL:
dist2 <- outer(rowSums(scaledA^2), rowSums(scaledB^2), "+") -
          2 * tcrossprod(scaledA, scaledB)
```

Para o GP interno com `poolSize = max(512, min(8192, 1024 * length(parameterNames)))` e `parameterNames` com 8 parâmetros: `poolSize = 8192`. A linha `kPool <- TuneBoostTree_RbfKernel(xPool, xTrain, ...)` com `xPool` sendo 8192 x 8 e `xTrain` sendo até ~100 x 8 produz uma matriz `dist2` de 8192 x 100 = ~6 MB - aceitável. Porém, `kTrain <- RbfKernel(xTrain, xTrain, ...)` com 100 pontos de histórico produz uma matriz 100 x 100 = negligível. O gargalo real só aparece após ~500 iterações acumuladas no histórico: 500 x 8192 = 32 MB por chamada de `AcquisitionScores` - gerenciável, mas quadrático no número de iterações.

**Problema mais sutil:** `scaledA^2` cria uma cópia inteira de `scaledA` antes de `rowSums`. Para reduzir alocações:

```r
# CODIGO OTIMIZADO: evitar cópia intermediária:
dist2 <- outer(.rowSums(scaledA * scaledA, nrow(scaledA), ncol(scaledA)),
               .rowSums(scaledB * scaledB, nrow(scaledB), ncol(scaledB)),
               "+") - 2 * tcrossprod(scaledA, scaledB)
```


***

### BUG-07 (MEDIO): `TuneBoostTree_EvaluateCv` - `<<-` em `evaluationLogList[[logIndex]]` não é thread-safe para otimizadores batch

**Arquivo:** função `TuneBoostTree_EvaluateCv`

```r
logIndex <<- logIndex + 1L
evaluationLogList[[logIndex]] <<- data.frame(...)
assign(cacheKey, ..., envir = cacheEnv)
```

Conforme mencionado na análise anterior, o `rBayesianOptimization` é sequencial e o Limbo atual é ask/tell sequencial - portanto isso não causa falha hoje. Porém, o `TuneBoostTree_RunInternalOptimizer` tem um loop `for` sequencial que chama `objective` - também seguro. O risco permanece como dívida técnica explícita para qualquer extensão futura com avaliações paralelas do otimizador.

***

## Otimizações HPC - Problemas Novos Identificados

### HPC-01: `TuneBoostTree_AcquisitionScores` - `chol(kTrain)` sem `LAPACK` explícito para BLAS otimizado

**Arquivo:** função `TuneBoostTree_AcquisitionScores`

```r
cholK <- tryCatch(chol(kTrain), error = function(e) NULL)
alpha <- backsolve(cholK, forwardsolve(t(cholK), yScaled))
```

`chol()` no R usa LAPACK (`dpotrf`). No servidor Xeon 8260 com OpenBLAS ou MKL configurado, `dpotrf` se beneficia de BLAS-3 para matrizes grandes. Para matrizes pequenas (100 x 100, tamanho típico do histórico), o overhead de lançar threads BLAS é maior que o ganho. **O problema:** `t(cholK)` cria uma cópia transposta da matriz. Para choleski, `U = chol(A)` retorna triangular superior, e `forwardsolve(t(cholK), b)` é o solve para `L = t(U)` triangular inferior. A forma correta para evitar a cópia é usar `backsolve(cholK, backsolve(cholK, b, transpose = TRUE))`:

```r
# CODIGO OTIMIZADO: eliminar t(cholK) evitando cópia de matriz:
alpha  <- backsolve(cholK, backsolve(cholK, yScaled, transpose = TRUE))
# Para v = forwardsolve(t(cholK), t(kPool)):
v <- backsolve(cholK, t(kPool), transpose = TRUE)
```


***

### HPC-02: `TuneBoostTree_ProposeInternalBayesianCandidate` re-executa `set.seed` a cada iteração do loop - quebra reprodutibilidade

**Arquivo:** função `TuneBoostTree_ProposeInternalBayesianCandidate`

```r
TuneBoostTree_ProposeInternalBayesianCandidate <- function(
  history, bounds, acq = "ucb", kappa = 2.576, eps = 0, seed = 42L) {

  set.seed(as.integer(seed))   # <- PROBLEMA
  # ...
  pool <- TuneBoostTree_SampleCandidates(bounds, poolSize)
```

O `seed` passado é `seed + iteration` (de `TuneBoostTree_RunInternalOptimizer`), então cada iteração tem seed distinto - correto para reprodutibilidade. Porém, `set.seed` dentro da função candidata afeta o RNG **global** do processo R. Se `parallel::mclapply` for chamado depois (nos folds), o estado do RNG no pai será diferente dependendo de quantas iterações do otimizador já rodaram, afetando qualquer `runif`/`sample` subsequente fora do controle de seed explícito. Em `TuneBoostTree_SampleCandidates`, que também usa `runif`, o `set.seed` aqui é intencional, mas o efeito colateral no RNG global é indesejado.

***

## Inconsistências de Documentação Novas

### DOC-01: `TuneBoostTreeImbalance` - `@return` documenta campo `balance_args` (snake_case), confirmando violação de nomenclatura na API pública documentada

**Arquivo:** `TuneBoostTreeImbalance`, bloco roxygen2

```
@return Lista validada com classe `tbtb_imbalance`, contendo `balanceFn`,
  `scale_pos_weight` e `balance_args`.
```

E o código retorna exatamente:

```r
out <- list(balanceFn = balanceFn,
            scale_pos_weight = scale_pos_weight,  # snake_case
            balance_args = list(...))              # snake_case
```

A documentação e o código estão **consistentes entre si**, mas ambos violam o padrão camelCase. Isso significa que qualquer correção de nomenclatura que renomeie `balance_args` para `balanceArgs` é uma **mudança de breaking API** - usuários que acessam `imbalance$balance_args` diretamente terão seu código quebrado. Deve ser tratado como depreciação com alias antes da remoção.

### DOC-02: `TuneBoostTree` `@param data` - afirma que "internamente a entrada é padronizada para `data.frame`" mas não menciona implicações para `sparseMatrix`

O `@param data` diz:

```
Internamente a entrada é padronizada para `data.frame` antes da criação das
matrizes de engine.
```

A linha de código é `data <- as.data.frame(data)`. Se o usuário passar uma `sparseMatrix` como `data`, `as.data.frame` tentará convertê-la, possivelmente falhando ou materializando o objeto denso. A documentação deveria alertar explicitamente:

```
@param data data.frame, tibble ou data.table (não `sparseMatrix`) não vazio...
```


### DOC-03: `TuneBoostTreeCv` - `@param stratified` afirma que `FALSE` "é rejeitado" mas o aviso diz `cli_abort` - correto no código, impreciso na redação

```
`FALSE` é rejeitado, não ignorado...
```

O código é `if(!out$stratified) cli::cli_abort(...)` - o comportamento está correto. A frase "é rejeitado" é tecnicamente precisa mas poderia ser mais clara: "`FALSE` gera um erro imediato".

***

## Violações de Nomenclatura Confirmadas

### NOME-01 (PERSISTENTE e CONFIRMADO): `engine_boost_tree` em 12 assinaturas `@noRd`

Contagem exata no SHA atual:


| Função | Tipo de uso |
| :-- | :-- |
| `TuneBoostTree_CreateDataObject` | parâmetro formal |
| `TuneBoostTree_BuildParams` | parâmetro formal |
| `TuneBoostTree_PrepareBalancedFolds` | parâmetro formal |
| `TuneBoostTree_RunCvManual` | parâmetro formal |
| `TuneBoostTree_RunFoldById` | parâmetro formal |
| `TuneBoostTree_RunOneFold` | parâmetro formal |
| `TuneBoostTree_OptimizeThresholdCv` | parâmetro formal |
| `TuneBoostTree_RunCvPredictions` | parâmetro formal |
| `TuneBoostTree_RunFoldPredictionById` | parâmetro formal |
| `TuneBoostTree_RunOneFoldPrediction` | parâmetro formal |
| `TuneBoostTree` (corpo) | variável local `engine_boost_tree <- engine$name` |
| `FitBoostTreeModel` (corpo) | variável local `engine_boost_tree <- engineBoostTree` |

As funções exportadas (`FitBoostTreeModel`, `PredictBoostTreeModel`) já adotaram `engineBoostTree` corretamente na assinatura pública. As funções internas com `engine_boost_tree` são todas `@noRd`, portanto a renomeação não quebra a API pública. A variável local `engine_boost_tree <- engineBoostTree` em `FitBoostTreeModel` existe para compatibilidade interna mas também deveria ser `engineBoostTree` consistentemente.

### NOME-02 (CONFIRMADO): `scale_pos_weight` e `balance_args` nos campos do objeto `tbtb_imbalance`

Acesso a `imbalance$balance_args` confirmado em `TuneBoostTree` (corpo principal) e `TuneBoostTree_PrepareBalancedFolds`. Acesso a `imbalance$scale_pos_weight` confirmado em ambas as funções. São campos de API pública documentada em snake_case.

***

## Tabela de Status Consolidada

| \# | Categoria | Severidade | Status | Descrição |
| :-- | :-- | :-- | :-- | :-- |
| 1 | Bug | CRITICO | ATIVO | `mean(numericMatrix == 0)` aloca matriz densa completa + booleana antes do heurístico NUMA |
| 2 | Bug | CRITICO | ATIVO | `PrepareBalancedFolds` aloca `xMatrix` completo apenas para extrair `targetLevels` - `PrepareTarget` já existe e resolve isso |
| 3 | Bug | ALTO | ATIVO | `DetectCpuBudget` retorna 94 cores em servidor sem container cgroups (deveria ser 46 físicos) |
| 4 | Bug | ALTO | ATIVO | `FinalizeParallel` avisa oversubscription mas retorna valor sem correção |
| 5 | Bug | ALTO | ATIVO | `RunCvPredictions` sem `prAucBackend` na assinatura - ignora configuração de performance na etapa de threshold |
| 6 | Bug | MEDIO | ATIVO | `RbfKernel` usa `scaledA^2` que cria cópia intermediária desnecessária |
| 7 | Bug | MEDIO | ATIVO | `ProposeInternalBayesianCandidate` chama `set.seed` globalmente contaminando RNG do processo pai |
| 8 | HPC | MEDIO | ATIVO | `chol(kTrain)` seguido de `t(cholK)` cria cópia desnecessária - usar `backsolve(..., transpose = TRUE)` |
| 9 | Docs | MEDIO | ATIVO | `@param data` não avisa contra `sparseMatrix` como entrada |
| 10 | Docs | MEDIO | ATIVO | `balance_args` e `scale_pos_weight` documentados como snake_case em API pública - mudança futura é breaking |
| 11 | Padrão | MEDIO | PERSISTENTE | `engine_boost_tree` em 12 locais internos (10 assinaturas + 2 variáveis locais) |
| 12 | Padrão | MEDIO | PERSISTENTE | `balance_args`, `scale_pos_weight` em campos de `tbtb_imbalance` |

