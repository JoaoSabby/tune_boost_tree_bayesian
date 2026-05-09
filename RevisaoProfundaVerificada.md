# Verificação dos apontamentos da RevisaoProfunda.md

Esta verificação confronta cada apontamento da auditoria original com o código atual do pacote e registra a decisão aplicada. Classificação usada: **válido**, **parcialmente válido** ou **não confirmado**.

## Vulnerabilidades e bugs críticos

| Item | Veredito | Ação |
| --- | --- | --- |
| BUG CRITICO-01: `as.data.frame(data)` destruiria `sparseMatrix` passada diretamente a `TuneBoostTree()` | **Não confirmado** | `TuneBoostTree()` exige `data.frame`, `tibble` ou `data.table` antes da conversão. Uma `sparseMatrix` direta não chega à conversão porque falha na validação de entrada. Não alterado para evitar aceitar uma entrada sem coluna alvo compatível com fórmula. |
| BUG CRITICO-02: conversão incondicional de `preparedTrain$xMatrix` com `as.matrix()` em folds balanceados | **Válido** | Removida a densificação de treino. Também foi adicionada conversão de predição que preserva `sparseMatrix` e só usa `as.matrix()` para entradas densas. |
| Risco de data leakage no preparo de folds | **Parcialmente válido** | O fluxo prepara o alvo e os folds de forma estratificada antes do balanceamento. O balanceamento é aplicado apenas em `trainData`, então não há vazamento direto dos dados de validação. A função `TuneBoostTree_PrepareMatrix()` ainda ajusta codificação numérica por base, portanto transformações categóricas complexas seguem sendo um ponto para evolução futura. |
| Robustez do fallback do Limbo | **Parcialmente válido** | O fallback é robusto quando `fallback = TRUE`. Em configuração estrita (`strict_limbo = TRUE`), falhar sem executável é comportamento esperado. O script `inst/scripts/install_limbo.sh` não está vazio no código atual, então esse ponto da auditoria original estava desatualizado. |
| `cacheEnv` sem evicção | **Parcialmente válido** | O cache é local a uma chamada de `TuneBoostTree()` e seu crescimento é limitado pelo número de avaliações do otimizador, não é retenção infinita entre chamadas. Ainda assim, a chave foi reforçada com nomes dos parâmetros e formatação numérica estável para reduzir risco de colisão ambígua. |
| Race condition por closure em `TuneBoostTree_EvaluateCv` | **Não confirmado** | As avaliações do otimizador são sequenciais e os workers de folds recebem argumentos explícitos. Não há escrita concorrente em `parameterNames`, `boost` ou `cacheEnv` pelos workers. |

## Otimizações HPC e NUMA

| Item | Veredito | Ação |
| --- | --- | --- |
| Oversubscription por usar CPUs lógicas | **Parcialmente válido** | `TuneBoostTree_DetectCpuBudget()` já usa `parallel::detectCores(logical = FALSE)`, então o diagnóstico de uso de 96 threads lógicas não se confirma no código atual. Mantida a divisão por workers, com mitigação adicional de espera passiva de OpenMP. |
| `mclapply` sem `mc.set.seed = FALSE` | **Válido** | Como cada fold já recebe `seed + foldId`, foi definido `mc.set.seed = FALSE` nas chamadas `mclapply()` de treino e predição para evitar manipulação adicional do RNG pelo fork. |
| Ausência de política OpenMP | **Válido como mitigação segura** | Adicionada configuração de `OMP_WAIT_POLICY=passive` e `GOMP_SPINCOUNT=0` somente quando o usuário ainda não definiu essas variáveis. Isso evita spin-wait agressivo sem sobrescrever tuning externo do ambiente HPC. |
| Afinidade NUMA com `numactl` ou `taskset` | **Parcialmente válido, não aplicado** | A sugestão depende da topologia e do escalonador do cluster. Forçar afinidade dentro do pacote R pode conflitar com SLURM, cgroups, containers e políticas locais. Recomendado documentar execução externa com `numactl` quando necessário. |
| `src/Makevars` com `-O3 -march=native` | **Não aplicado por segurança** | `-march=native` em pacote R reduz portabilidade, pode quebrar builds em CRAN/CI e gerar binários incompatíveis. Flags agressivas devem ser configuradas pelo ambiente do usuário em `~/.R/Makevars` ou módulos HPC, não impostas pelo pacote. |

## Arquivos C e Fortran

| Item | Veredito | Ação |
| --- | --- | --- |
| Risco de memory leak em C | **Não confirmado** | A rotina C usa `R_alloc()`, que é gerenciado por R, e retorna apenas `ScalarReal()`. Não há alocação manual com `malloc()` que exigiria `free()`. |
| Falta de `PROTECT` em C | **Não crítico no código atual** | A função não mantém `SEXP` recém-alocados entre chamadas que possam disparar GC de forma problemática. `ScalarReal()` é retornado diretamente. |
| Fortran pouco vetorizado | **Parcialmente válido** | O gargalo principal é ordenação (`quicksort_indices`), não uma região vetorizável simples. Uma troca futura por ordenação iterativa ou backend C único pode melhorar robustez, mas não foi alterada nesta correção pontual. |

## CI/CD e instalação

| Item | Veredito | Ação |
| --- | --- | --- |
| Workflow em `macos-latest` incompatível com alvo Linux/HPC | **Válido** | Alterado para um job em container `oraclelinux:9` sobre runner `ubuntu-22.04`, aproximando o CI do ambiente Oracle Linux indicado em `SystemRequirements`. |
| `lightgbm`, `cli` e `data.table` ausentes em `extra-packages` | **Não crítico** | O workflow atual instala dependências do `DESCRIPTION` com `remotes::install_deps(dependencies = TRUE)`. A ausência anterior em `extra-packages` não era falha funcional e deixou de se aplicar após a troca para container Oracle Linux. |
| `install_limbo.sh` vazio | **Não confirmado** | O script atual possui validação, `set -Eeuo pipefail`, modo dry-run, suporte a `dnf`/`yum`/`apt-get` e escrita idempotente de variáveis de ambiente. Foram adicionados smoke tests do instalador, instalação real no workflow e testes do contrato ask/tell do Limbo. |

## Documentação

| Item | Veredito | Ação |
| --- | --- | --- |
| `mtry = NULL` em `TuneBoostTreeSearchSpace()` ambíguo | **Parcialmente válido** | A documentação já informa que o padrão de `TuneBoostTreeBoostParams()` é `"default"`, salvo configuração explícita. Não alterado. |
| `bestThreshold` poderia não conter `metric` | **Não confirmado** | `TuneBoostTree_OptimizeThreshold()` sempre retorna `threshold`, `metric` e `score`; o backend PR-AUC não altera essa estrutura. |
| `TuneBoostTreeUltraConfig()` com documentação pouco detalhada | **Válido como melhoria futura** | Não é bug de execução. Pode ser expandido em rodada de documentação, preferencialmente regenerando roxygen. |
| `initial` sem esquema detalhado de warm-start | **Válido como melhoria futura** | Não é bug de execução. O contrato mínimo é validado por `TuneBoostTree_DeduplicateInitGrid()`. |

## Nomenclatura

| Item | Veredito | Ação |
| --- | --- | --- |
| `engine_boost_tree` em snake_case | **Válido, não alterado nesta rodada** | O nome aparece também em funções exportadas como `FitBoostTreeModel()` e `PredictBoostTreeModel()`. Renomear quebraria compatibilidade de API. Sugestão segura: em versão futura, introduzir `engineBoostTree` como alias e deprecar `engine_boost_tree`. |
| Campos nativos como `scale_pos_weight` | **Aceitável** | O nome é nativo de XGBoost/LightGBM e parte do contrato do hiperparâmetro. Manter reduz tradução desnecessária e risco de erro. |

## Resumo das correções aplicadas

1. Preservação de matrizes esparsas nos folds balanceados e nas matrizes de predição do LightGBM.
2. Configuração passiva de espera OpenMP, sem sobrescrever variáveis já definidas pelo usuário ou pelo escalonador HPC.
3. `mc.set.seed = FALSE` em forks, mantendo a semeadura explícita por fold.
4. Chaves de cache com nomes de parâmetros e formatação numérica estável.
5. CI migrado de macOS para container Oracle Linux 9 sobre runner Ubuntu 22.04 para maior aderência ao alvo Linux/HPC.
6. Instalador do Limbo ampliado para Oracle Linux via `dnf`/`yum`, com smoke test no CI, instalação real no workflow, adaptador ask/tell de referência e teste de otimização com `fallback = FALSE`.
