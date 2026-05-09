<img src="https://r2cdn.perplexity.ai/pplx-full-logo-primary-dark%402x.png" style="height:64px;margin-right:32px"/>

# Ajustes implementados. Faça uma nova conferência verificando tudo novamente com profundidade

Análise completa concluída. Segue o relatório de conferência da v3:

***

## Conferência v3 - Auditoria Completa TuneBoostTreeBayesian

### Status dos Itens Pendentes da v2

**BUG-CRITICO-03 (cacheEnv sem on.exit): RESOLVIDO.** O `on.exit` foi implementado corretamente nas linhas 638-642, com `ls(envir = cacheEnv, all.names = TRUE)` para capturar símbolos com ponto no nome também. Excelente.

**NOVO-01 (balance_fn redundante): RESOLVIDO COM DEPRECATION.** A abordagem adotada é mais robusta do que a sugerida: em vez de remover o campo, a função `TuneBoostTree_ResolveImbalance` agora emite `cli_warn` informando que `balance_fn` é deprecated e migra o valor para `balanceFn`. Isso preserva compatibilidade retroativa sem manter a dualidade silenciosa.

**NOVO-03 (validação de oversubscription manual): RESOLVIDO.** `TuneBoostTree_FinalizeParallel` verifica `workers * threads_per_worker > 2 * totalCores` e emite `cli_warn` adequado. O cálculo automático usa `floor(46/2) = 23` como teto de workers (com 48 cores fisicos e reserva de 2), resultando em no máximo 10 x 4 = 40 threads para 10 folds, dentro do orçamento.

**NOVO-04 (src/Makevars ausente): RESOLVIDO COM DESIGN CONSCIENTE.** O `Makevars` foi criado com a estratégia `TBTB_OPT_FLAGS ?=`, que é a abordagem correta para um pacote que pode ser publicado no CRAN - flags fixas como `-march=native` proibiriam a distribuição de binários portáteis. O usuário HPC ativa via `~/.R/Makevars` ou variável de ambiente. **Ponto pendente:** o README ainda não documenta isso.

***

### Novos Itens Identificados na v3

**NOVO-v3-01: `SetPassiveOpenMp` não é chamada em `TuneBoostTree` antes da otimização principal - apenas dentro de `RunCvManual`.**

`TuneBoostTree_SetPassiveOpenMp()` é chamada em duas linhas: 1309 (`RunCvManual`) e 1731 (`FitBoostTreeModel` provavelmente via outro fluxo). Porém, `TuneBoostTree()` não a chama diretamente no início. O problema: o otimizador Bayesiano (`rBayesianOptimization`) executa `objective()` iterativamente, e `RunCvManual` é chamada dentro de `objective` - portanto `SetPassiveOpenMp` acaba sendo chamada na primeira iteração e permanece configurada para as seguintes. Isso funciona na prática, mas é frágil: se o ambiente for reconfigurado entre chamadas (por ex. outro pacote redefinindo `OMP_WAIT_POLICY`), as iterações subsequentes não terão proteção. **Sugestão:** mover a chamada para o início de `TuneBoostTree()`, logo após `TuneBoostTree_ResolveControl()`.

***

**NOVO-v3-02: `engine_boost_tree` permanece como parâmetro formal snake_case em TODAS as funções exportadas e em `FitBoostTreeModel` (função pública exportada).**

Este é o item de maior impacto de nomenclatura que permanece sem resolução. `FitBoostTreeModel` é uma função **exportada** (consta no NAMESPACE), usada diretamente pelo usuário final. O parâmetro `engine_boost_tree = "lightgbm"` viola o padrão camelCase definido pelo projeto. As funções internas (`@noRd`) com o mesmo nome são aceitáveis como técnicos, mas a função pública não deve expor snake_case.

```r
# ATUAL (violacao em funcao pública exportada):
FitBoostTreeModel <- function(formula, dataTrain, hyperparameters,
                               featureTypes = NULL, targetLevels = NULL,
                               scalePosWeight = NULL, nThreads = 8L,
                               seed = 42L, verbose = 0L,
                               engine_boost_tree = "lightgbm") { ... }

# SUGESTAO (camelCase correto):
FitBoostTreeModel <- function(formula, dataTrain, hyperparameters,
                               featureTypes = NULL, targetLevels = NULL,
                               scalePosWeight = NULL, nThreads = 8L,
                               seed = 42L, verbose = 0L,
                               engineBoostTree = "lightgbm") {
  engine_boost_tree <- engineBoostTree  # alias interno para compatibilidade
  ...
}
```


***

**NOVO-v3-03: Bug de logica no Quicksort Fortran - `pivot` é indice, mas `before()` é chamada com `pivot` onde se espera um indice.**

Esta é a descoberta mais crítica da v3. No Quicksort do Fortran:

```fortran
pivot = index_vector((left + right) / 2)  ! pivot é um ÍNDICE (inteiro que aponta para uma posição no vetor original)

do while (before(index_vector(i_left), pivot))  ! OK: compara dois índices
do while (before(pivot, index_vector(i_right))) ! OK: compara dois índices
```

A função `before(left_index, right_index)` usa `predicted(left_index)` e `predicted(right_index)` - ou seja, acessa `predicted` pela posição. O `pivot` é um elemento de `index_vector`, portanto também é um indice válido no vetor `predicted`. **Isso está correto.** O quicksort ordena indices pelo valor de `predicted[index]`, e `before()` recebe indices em ambos os argumentos. Falso alarme confirmado por inspeção.

***

**NOVO-v3-04: Recalculo desnecessario de `totalCores` dentro de `RunCvManual` - dupla chamada a `DetectCpuBudget`.**

`TuneBoostTree_ResolveParallel` (linha ~923) já calculou `totalCores` e retornou `workerThreads` resolvido. Porém, `TuneBoostTree_RunCvManual` recalcula `totalCores` internamente:

```r
# TuneBoostTree_RunCvManual (linha 1303):
totalCores <- TuneBoostTree_DetectCpuBudget()   # <- chamada redundante
nWorkers   <- min(max(1L, as.integer(nWorkersFolds)), length(balancedFolds))
workerThreads <- max(1L, floor(totalCores / nWorkers))   # <- recalculo redundante
workerThreads <- min(as.integer(nThreads), workerThreads)
```

O `nThreads` passado para `RunCvManual` já é o `workerThreads` resolvido por `FinalizeParallel`. O recalculo é no máximo redundante, mas pode sobrescrever a decisão de `FinalizeParallel` em casos onde `nFolds < nWorkers` (o `min(..., length(balancedFolds))` recalcula o teto de workers sem considerar os folds balanceados reais). Em cenários onde `nWorkersFolds = 10` mas `balancedFolds` tem 8 elementos (por ajuste de estratificação), o `workerThreads` recalculado pode divergir do calculado em `FinalizeParallel`. **Sugestão:** remover o recalculo interno e usar `nThreads` diretamente como `workerThreads`:

```r
TuneBoostTree_RunCvManual <- function(..., nThreads, nWorkersFolds, ...) {
  nWorkers      <- min(max(1L, as.integer(nWorkersFolds)), length(balancedFolds))
  workerThreads <- as.integer(nThreads)  # já resolvido por FinalizeParallel
  ...
}
```


***

**NOVO-v3-05: README não documenta `TBTB_OPT_FLAGS` nem como ativar AVX-512 no servidor HPC.**

A secao "Distribuicao compilada do pacote" do README menciona apenas `R CMD build` e `R CMD INSTALL`, sem informar como o usuario HPC ativa as flags de performance. Como o pacote é explicitamente descrito como "preparado para execução dedicada em servidor Intel Xeon Platinum 8260", a ausência dessa instrução é uma lacuna de documentação crítica para o uso correto em producao.

**Sugestão de adição ao README:**

```markdown
## Performance máxima em servidores HPC

Para compilar com otimizações AVX-512 e vetorização agressiva no Intel Xeon Platinum 8260 (Oracle Linux 9.7 com GCC/gfortran), crie ou edite `~/.R/Makevars`:

```makefile
TBTB_OPT_FLAGS = -O3 -march=native -funroll-loops -ffast-math
```

Então reinstale o pacote a partir do source:

```bash
R CMD INSTALL --preclean TuneBoostTreeBayesian_*.tar.gz
```

```

***

**NOVO-v3-06: `TuneBoostTreeBayesian` é alias via funcao wrapper - inconsistencia com `TuneBoostTreeLimbo` que é alias direto.**

```r
# TuneBoostTreeBayesian: wrapper com corpo de funcao (copia os args explicitamente)
TuneBoostTreeBayesian <- function(...) { TuneBoostTree(...) }

# TuneBoostTreeLimbo: alias direto por atribuicao
TuneBoostTreeLimbo <- TuneBoostTreeOptimizerLimbo
```

O alias direto (`TuneBoostTreeLimbo <- TuneBoostTreeOptimizerLimbo`) é mais eficiente e semanticamente correto para aliases. O wrapper de `TuneBoostTreeBayesian` cria um frame de chamada extra desnecessário. Padronize:

```r
#' @rdname TuneBoostTree
#' @export
TuneBoostTreeBayesian <- TuneBoostTree
```


***

### Tabela de Status Consolidada (v3)

| Item | Severidade | v1 | v2 | v3 |
| :-- | :-- | :-- | :-- | :-- |
| install_limbo.sh vazio | CRITICO | Pendente | Resolvido | Resolvido |
| as.matrix esparso desnecessario | CRITICO | Pendente | Resolvido | Resolvido |
| cacheEnv sem on.exit | ALTO | Pendente | Pendente | **Resolvido** |
| balance_fn redundante | MEDIO | Novo | Pendente | **Resolvido** |
| Validacao oversubscription | MEDIO | Novo | Pendente | **Resolvido** |
| src/Makevars ausente | CRITICO | Pendente | Pendente | **Resolvido (TBTB_OPT_FLAGS)** |
| mc.set.seed / OMP_WAIT_POLICY | ALTO | Pendente | Resolvido | Resolvido |
| physicalCores vs. logicos | ALTO | Pendente | Resolvido | Resolvido |
| CI runner macOS | MEDIO | Pendente | Resolvido | Resolvido |
| lightgbm/cli ausentes no CI | MEDIO | Pendente | Resolvido | Resolvido |
| Aliases NAMESPACE sem @rdname | BAIXO | Pendente | Resolvido | Resolvido |
| README sem doc TBTB_OPT_FLAGS | MEDIO | - | - | **Novo - Pendente** |
| engine_boost_tree em FitBoostTreeModel (publica) | MEDIO | - | - | **Novo - Pendente** |
| SetPassiveOpenMp fora do fluxo principal | BAIXO | - | - | **Novo - Pendente** |
| Recalculo redundante de totalCores em RunCvManual | BAIXO | - | - | **Novo - Pendente** |
| TuneBoostTreeBayesian como wrapper em vez de alias direto | BAIXO | - | - | **Novo - Pendente** |

