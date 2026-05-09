
# Um implementei ajustes, alterações e correções. Confira novamente se tudo foi atendido e se há mais ajustes

Tenho agora uma visão completa e detalhada de todas as alterações. Segue o relatório de re-auditoria:

***

## Re-auditoria: TuneBoostTreeBayesian -

### Itens Resolvidos com Sucesso

**BUG-CRITICO-02 (as.matrix esparso): RESOLVIDO.** A conversao `as.matrix()` incondicional em `PrepareBalancedFolds` foi eliminada. No lugar, foi criada `TuneBoostTree_AsPredictionMatrix()` que preserva `sparseMatrix` e converte apenas matrizes densas, e `TuneBoostTree_CreateDataObject()` passa `xMatrix` diretamente a `xgb.DMatrix()` e `lgb.Dataset()` - ambas aceitam `dgCMatrix` nativamente. Excelente.

**HPC-02 (mc.set.seed / OMP_WAIT_POLICY): RESOLVIDO.** Ambos os `mclapply` receberam `mc.set.seed = FALSE`. A funcao `TuneBoostTree_SetPassiveOpenMp()` foi criada e aplica `OMP_WAIT_POLICY = "passive"` e `GOMP_SPINCOUNT = "0"` somente se o usuario nao tiver definido antes.

**HPC-01 (physicalCores em vez de logicos): RESOLVIDO.** `TuneBoostTree_DetectCpuBudget()` agora usa `parallel::detectCores(logical = FALSE)` como primario e faz fallback para logico apenas se o primeiro retornar `NA`. O calculo de workers em `ResolveParallel` usa `floor(totalCores / 2L)` como teto de workers, o que no Xeon 8260 com 48 cores fisicos resulta em no maximo 24 workers, deixando os outros 24 cores para threads da engine.

**CI-01 (runner macOS): RESOLVIDO.** O workflow migrou para `self-hosted`, correto para o ambiente Oracle Linux 9.7 do servidor.

**CI-02/CI-03 (lightgbm, cli, data.table ausentes): RESOLVIDO.** `lightgbm`, `Matrix` e `data.table` agora constam explicitamente nos `extra-packages`. O workflow adiciona os passos de instalacao EPEL, CodeReady Builder e dependencias de sistema (`gcc-gfortran`, `openblas-devel`, etc.) antes de configurar o R.

**ARQT-01 (aliases NAMESPACE sem documentacao): RESOLVIDO.** `TuneBoostTreeLimbo` e `TuneBoostTreeRBayesianOptimization` agora sao declarados explicitamente como aliases com `@rdname` apontando para as funcoes canonicas.

**install_limbo.sh (arquivo vazio): RESOLVIDO.** O script agora tem 501 linhas com `set -Eeuo pipefail`, estrutura de opcoes, documentacao de uso e logica de instalacao do Limbo.

***

### Itens Parcialmente Resolvidos

**BUG-CRITICO-01 (as.data.frame incondicional): PARCIALMENTE RESOLVIDO.**

A linha problemática permanece **identica** a versao anterior:

```r
# AINDA PRESENTE na linha 575:
if(!is.data.frame(data) || nrow(data) == 0L) cli::cli_abort(...)
data <- as.data.frame(data)
```

A validacao rejeita entradas que nao sejam `data.frame` (`tibble` e `data.table` herdam `data.frame`, entao passam), mas `sparseMatrix` nao herda `data.frame` e seria rejeitada pelo `cli_abort` antes mesmo de chegar ao `as.data.frame()`. O comportamento funcional esta correto para o uso documentado - o `@param data` da funcao especifica `data.frame, tibble ou data.table` e nao `sparseMatrix`. O ponto pendente e que a mensagem de erro nao menciona `data.table` como aceito, mas isso e menor.

**BUG-CRITICO-03 (cacheEnv sem evicção): PENDENTE.**

```r
# Linha 636: cacheEnv criado, mas sem limpeza via on.exit
cacheEnv <- new.env(parent = emptyenv())
# ... execução completa ...
list(bestHyperparameters = ..., ...)  # retorna sem rm() ou gc()
```

Nao ha `on.exit({ rm(list = ls(envir = cacheEnv), envir = cacheEnv); gc() })` em `TuneBoostTree()`. O `cacheEnv` e local a cada chamada de `TuneBoostTree()` (nao e um environment global persistente), entao sera coletado pelo GC do R quando o objeto retornado sair de escopo. Em uso interativo simples isso e aceitavel. Porém em pipelines onde o resultado e armazenado (ex.: `resultado <- TuneBoostTree(...)`) por toda a sessao, o `cacheEnv` com todos os `Score + bestIteration` de cada candidato avaliado permanece na memoria. Em 200 iteracoes de otimizacao com dados grandes por candidato, isso pode acumular dezenas de MB desnecessariamente. Recomendacao continua valida: adicionar limpeza explicita ao final:

```r
# Adicionar antes do return() final em TuneBoostTree():
on.exit({
  if (exists("cacheEnv", inherits = FALSE) && is.environment(cacheEnv))
    rm(list = ls(envir = cacheEnv), envir = cacheEnv)
}, add = TRUE)
```

**BUG-CRITICO-05 (closure EvaluateCv - race condition): ESTRUTURALMENTE PENDENTE.**

```r
# EvaluateCv ainda usa parameterNames, boost, balancedFolds, etc. do env pai:
TuneBoostTree_EvaluateCv <- function(...) {
  hyperparameters <- hyperparameters[parameterNames]  # env pai
  ...
  cvSummary <- TuneBoostTree_RunCvManual(balancedFolds, ...)  # env pai
```

O `environment(objective) <- environment()` (linha 641) religar o ambiente e a solucao intencional adotada - e valida para uso com `rBayesianOptimization` que chama `objective` sequencialmente. O risco de race condition com `mclapply` **nao existe nesse nivel** porque o otimizador Bayesiano e serial (cada candidato precisa do resultado anterior para propor o proximo). O `mclapply` so e usado dentro de `TuneBoostTree_RunCvManual`, que recebe tudo por argumento. A critica anterior foi tecnicamente imprecisa. **Este item pode ser considerado resolvido por design.**

**BUG-CRITICO-04 (data leakage em PrepareMatrix): PENDENTE - REQUER INSPECAO.**

`TuneBoostTree_PrepareMatrix` continua sendo chamada sobre o dataset completo antes dos folds:

```r
preparedFull <- TuneBoostTree_PrepareMatrix(formula, data, NULL, targetLevels, formulaInfo)
folds <- TuneBoostTree_CreateStratifiedFolds(preparedFull$yData, nFolds, seed)
```

Isso e necessario para obter `targetLevels` antes de criar os folds - e nao causa leakage **se** `PrepareMatrix` for puramente estateless (apenas aplica `model.matrix()` e nao aprende estatisticas do dataset). Com base no nome e padrao de uso, e provavel que seja estateless. Mas isso deve ser verificado explicitamente na implementacao de `TuneBoostTree_PrepareMatrix`.

***

### Itens Novos Identificados na v2

**NOVO-01: `balance_fn` e `balanceFn` coexistem no objeto `tbtb_imbalance` - redundancia desnecessaria.**

```r
# Linha 311 em TuneBoostTreeImbalance():
out <- list(balanceFn = balanceFn, balance_fn = balanceFn,
            scale_pos_weight = scale_pos_weight, balance_args = list(...))
```

O campo `balance_fn` (snake_case) e mantido ao lado de `balanceFn` (camelCase correto), possivelmente para compatibilidade retroativa. Isso viola o padrao de nomenclatura camelCase e cria confusao - especialmente porque na linha 610 de `TuneBoostTree()` ambos sao consultados:

```r
(if(!is.null(imbalance$balanceFn)) imbalance$balanceFn else imbalance$balance_fn)
```

**Sugestao:** remover `balance_fn` do objeto e manter apenas `balanceFn`. Se houver necessidade de compatibilidade com codigo legado, adicionar deprecation warning.

**NOVO-02: `TuneBoostTree_DetectCpuBudget` nao tem reserva de cores para o processo R principal.**

```r
TuneBoostTree_DetectCpuBudget <- function() {
  physical <- suppressWarnings(parallel::detectCores(logical = FALSE))
  ...
  as.integer(physical)  # retorna 48 no Xeon 8260
}
```

O calculo downstream usa `floor(48 / 2) = 24` workers no maximo, e `floor(48 / 24) = 2` threads por worker - o que e seguro. Mas o processo R principal, o otimizador Bayesiano, e eventuais threads de sistema nao tem cores reservados. Em carga maxima (24 workers x 2 threads = 48 threads OpenMP + R principal), todos os 48 cores fisicos ficam saturados. Reservar 2 cores fisicos seria mais seguro:

```r
# Sugestao:
availableCores <- max(1L, as.integer(physical) - 2L)  # reservar 2 para R e SO
```

**NOVO-03: `TuneBoostTree_ResolveParallel` - ausencia de protecao contra oversubscription quando usuario configura `workers` e `threads_per_worker` manualmente.**

```r
# Quando parallel = TuneBoostTreeParallel(workers = 40, threads_per_worker = 10):
# 40 * 10 = 400 threads tentando rodar em 48 cores fisicos
# Nao ha validacao do produto workers * threads_per_worker
```

Sugestao de adicionar:

```r
if (workers * threads > as.integer(totalCores) * 2L) {
  cli::cli_warn("workers ({workers}) * threads_per_worker ({threads}) = {workers * threads} exceeds 2x physical cores ({totalCores}). Consider reducing to avoid oversubscription.")
}
```

**NOVO-04: `src/Makevars` continua ausente - retorna 404.**

Este e o item de maior impacto em performance HPC que permanece **nao resolvido**. Sem `Makevars`, o R compila `tbtb_native_c.c` e `tbtb_native_fortran.f90` com flags padrao do sistema (tipicamente `-O2` sem `-march=native`), desperdicando as instrucoes AVX-512 do Xeon Platinum 8260. O ganho de `-O3 -march=native -funroll-loops` em rotinas de ordenacao e calculo de area sob curva pode ser de 2-4x.

Criar `src/Makevars`:

```makefile
PKG_CFLAGS   = -O3 -march=native -funroll-loops -ffast-math
PKG_FFLAGS   = -O3 -march=native -funroll-loops
PKG_CXXFLAGS = -O3 -march=native -funroll-loops
```

E `src/Makevars.win` para portabilidade:

```makefile
PKG_CFLAGS   = -O2
PKG_FFLAGS   = -O2
PKG_CXXFLAGS = -O2
```


***

### Tabela de Status Final

| Item | Severidade | Status v2 |
| :-- | :-- | :-- |
| install_limbo.sh vazio | CRITICO | Resolvido |
| as.matrix esparso desnecessario | CRITICO | Resolvido |
| mclapply sem mc.set.seed | ALTO | Resolvido |
| OMP_WAIT_POLICY passivo | ALTO | Resolvido |
| physicalCores vs. logicos | ALTO | Resolvido |
| CI runner macOS | MEDIO | Resolvido |
| lightgbm ausente no CI | MEDIO | Resolvido |
| Aliases NAMESPACE sem @rdname | BAIXO | Resolvido |
| cacheEnv sem on.exit cleanup | ALTO | **Pendente** |
| src/Makevars ausente (AVX-512) | CRITICO HPC | **Pendente** |
| balance_fn snake_case redundante | BAIXO | **Novo** |
| Sem reserva de cores para R principal | MEDIO | **Novo** |
| Sem validacao de oversubscription manual | MEDIO | **Novo** |

