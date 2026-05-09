# Verificação dos apontamentos da RevisaoProfunda2.md

Esta verificação confronta cada apontamento da re-auditoria com o código atual do pacote e registra a decisão aplicada. Classificação usada: **confirmado**, **parcialmente válido**, **não confirmado** ou **não aplicado por segurança**.

## Itens marcados como resolvidos na re-auditoria

| Item | Veredito | Decisão |
| --- | --- | --- |
| BUG-CRITICO-02: preservação de matrizes esparsas | **Confirmado** | O fluxo atual preserva `sparseMatrix` em objetos de engine e só usa matriz densa quando necessário para predição. Sem alteração adicional. |
| HPC-02: `mc.set.seed = FALSE` e espera passiva OpenMP | **Confirmado** | As chamadas de `mclapply()` já usam `mc.set.seed = FALSE`; `OMP_WAIT_POLICY` e `GOMP_SPINCOUNT` só são definidos quando ausentes. Sem alteração adicional. |
| HPC-01: detecção de núcleos físicos | **Confirmado com melhoria** | A detecção já priorizava núcleos físicos. Foi adicionada reserva conservadora de até 2 núcleos para o processo R principal e o sistema operacional. |
| CI-01/02/03: runner e dependências | **Não verificado nesta rodada** | A re-auditoria referencia workflow externo; a árvore local não contém arquivos `.github` rastreados. Nenhuma alteração feita. |
| ARQT-01: aliases documentados | **Confirmado** | Os aliases `TuneBoostTreeLimbo` e `TuneBoostTreeRBayesianOptimization` estão associados aos respectivos `@rdname`. Sem alteração adicional. |
| `install_limbo.sh` não vazio | **Confirmado** | O script possui lógica de instalação e testes smoke existentes. Sem alteração adicional. |

## Itens parcialmente resolvidos ou pendentes

| Item | Veredito | Decisão |
| --- | --- | --- |
| BUG-CRITICO-01: `as.data.frame(data)` em `TuneBoostTree()` | **Não confirmado como bug** | A API documentada aceita objetos tabulares (`data.frame`, tibble e data.table). `sparseMatrix` direta não tem coluna resposta compatível com fórmula e continua rejeitada antes da conversão. Sem alteração funcional. |
| BUG-CRITICO-03: `cacheEnv` sem limpeza explícita | **Parcialmente válido** | O cache é local à chamada e não deveria persistir no retorno, mas a limpeza explícita é barata e reduz retenções acidentais. Foi adicionado `on.exit()` para esvaziar `cacheEnv`. |
| BUG-CRITICO-05: race condition por closure | **Não confirmado** | O otimizador avalia candidatos sequencialmente e o paralelismo ocorre dentro da CV, com argumentos passados explicitamente aos workers. Sem alteração. |
| BUG-CRITICO-04: possível data leakage em `PrepareMatrix()` | **Parcialmente válido como alerta** | `PrepareMatrix()` não aprende estatísticas de pré-processamento, mas a codificação por `data.matrix()` pode depender dos níveis presentes quando há variáveis categóricas de caracteres. O fluxo principal prepara a matriz completa antes de fatiar folds não balanceados, mantendo consistência; em folds balanceados, recomenda-se evolução futura para um encoder treinado no fold de treino e aplicado ao teste. Não alterado nesta rodada por exigir mudança de contrato e testes mais amplos. |

## Itens novos da v2

| Item | Veredito | Decisão |
| --- | --- | --- |
| NOVO-01: coexistência de `balanceFn` e `balance_fn` | **Válido** | `TuneBoostTreeImbalance()` agora emite apenas `balanceFn`. A resolução de configuração ainda aceita listas legadas com `balance_fn`, emitindo aviso de depreciação. |
| NOVO-02: falta de reserva de cores para R/SO | **Válido como mitigação segura** | `TuneBoostTree_DetectCpuBudget()` agora subtrai até 2 núcleos quando houver mais de 1 núcleo detectado. |
| NOVO-03: ausência de aviso para oversubscription manual | **Válido** | Foi centralizada a finalização de paralelismo e adicionado aviso quando `workers * threads_per_worker` excede 2 vezes o orçamento de CPU detectado. |
| NOVO-04: `src/Makevars` ausente e sugestão de `-march=native` | **Parcialmente válido; sugestão original não aplicada por segurança** | A ausência de `Makevars` impedia um ponto padronizado para flags opcionais. Foram criados `src/Makevars` e `src/Makevars.win`, mas sem forçar `-march=native`, `-ffast-math` ou outras flags agressivas. Usuários HPC podem optar por `TBTB_OPT_FLAGS`, preservando portabilidade de CRAN/CI e binários. |

## Resumo das alterações seguras aplicadas

1. Limpeza explícita de `cacheEnv` ao sair de `TuneBoostTree()`.
2. Remoção do campo redundante `balance_fn` dos objetos novos de `TuneBoostTreeImbalance()`, com compatibilidade legada no resolver.
3. Reserva conservadora de CPU para R/SO na detecção de orçamento de núcleos.
4. Aviso de oversubscription para configurações manuais de `workers` e `threads_per_worker`.
5. Criação de `src/Makevars` e `src/Makevars.win` com flags opcionais via `TBTB_OPT_FLAGS`, sem impor otimizações não portáveis.
