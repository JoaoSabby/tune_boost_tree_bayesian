# Verificação dos apontamentos do `RevisaoProfunda3.md`

Esta conferência revisa cada apontamento da auditoria v3 contra o estado atual do pacote e registra a decisão tomada: confirmar como válido e corrigir, confirmar como válido apenas para documentação, ou classificar como falso positivo/sem ação.

## Itens pendentes herdados da v2

| Item | Veredito | Evidência e decisão |
| --- | --- | --- |
| `BUG-CRITICO-03` — limpeza de `cacheEnv` com `on.exit` | Válido como já resolvido | A revisão v3 está correta: a limpeza com `on.exit()` e `all.names = TRUE` já existe no fluxo de otimização. Nenhuma alteração adicional foi necessária. |
| `NOVO-01` — `balance_fn` redundante | Válido como já resolvido | A migração com aviso de depreciação para `balanceFn` preserva compatibilidade e evita dualidade silenciosa. Mantido sem alteração. |
| `NOVO-03` — validação de oversubscription manual | Válido como já resolvido | A finalização de paralelismo já emite aviso quando `workers * threads_per_worker` excede o orçamento configurado. Mantido sem alteração. |
| `NOVO-04` — ausência de `src/Makevars` | Válido como resolvido no código; documentação estava pendente | `src/Makevars` usa `TBTB_OPT_FLAGS ?=` para não impor flags específicas de CPU em builds portáveis. A lacuna real era documentar o opt-in HPC; o README foi atualizado. |

## Novos itens da v3

### `NOVO-v3-01` — `SetPassiveOpenMp` fora do início de `TuneBoostTree()`

**Veredito: válido.** Embora `TuneBoostTree_SetPassiveOpenMp()` já fosse chamada dentro dos fluxos de CV, a configuração é uma salvaguarda de sessão/processo e deve ser aplicada no início do fluxo público principal, logo após a resolução de `control`. A correção reduz fragilidade caso outro pacote altere variáveis OpenMP antes da primeira iteração ou entre avaliações.

**Solução aplicada:** chamada explícita a `TuneBoostTree_SetPassiveOpenMp()` no início de `TuneBoostTree()` após `TuneBoostTree_ResolveControl()`.

### `NOVO-v3-02` — `engine_boost_tree` em função pública

**Veredito: válido, com escopo ampliado para APIs públicas de fit e predict.** O nome `engine_boost_tree` ainda era exposto em `FitBoostTreeModel()` e `PredictBoostTreeModel()`, destoando do padrão camelCase do pacote. A troca direta quebraria código de usuários, então a solução segura é oferecer `engineBoostTree` como argumento canônico e manter `engine_boost_tree` via `...` com aviso de depreciação.

**Solução aplicada:**

- `FitBoostTreeModel()` agora recebe `engineBoostTree` como argumento formal.
- `PredictBoostTreeModel()` agora recebe `engineBoostTree` como argumento formal.
- `engine_boost_tree` continua aceito via `...`, com `cli_warn()`, e falha cedo se usado junto com `engineBoostTree`.
- Documentação e exemplos foram migrados para `engineBoostTree`.
- Testes cobrem tanto a nova API quanto a compatibilidade legada com aviso.

### `NOVO-v3-03` — suposto bug no Quicksort Fortran

**Veredito: falso positivo.** A análise v3 está correta ao concluir que não há bug. `pivot` recebe um elemento de `index_vector`, isto é, um índice do vetor original; `before(left_index, right_index)` também espera índices do vetor original e acessa `predicted()` por esses índices. Portanto, a comparação é semanticamente consistente.

**Decisão:** nenhuma alteração aplicada no Fortran.

### `NOVO-v3-04` — recálculo redundante de `totalCores` em `RunCvManual`

**Veredito: válido.** `TuneBoostTree_ResolveParallel()`/`TuneBoostTree_FinalizeParallel()` já resolve `threads_per_worker`; recalcular `totalCores` dentro do executor de CV reabre a decisão e pode divergir quando o número efetivo de folds muda. O executor deve apenas limitar workers pelo número de folds disponíveis e usar o orçamento de threads recebido.

**Solução aplicada:** `TuneBoostTree_RunCvManual()` passou a usar `nThreads` diretamente como `workerThreads`. A mesma correção foi aplicada em `TuneBoostTree_RunCvPredictions()`, que tinha o mesmo padrão redundante durante a etapa de otimização de threshold.

### `NOVO-v3-05` — README sem documentação de `TBTB_OPT_FLAGS`

**Veredito: válido.** O design de `src/Makevars` é correto e portável, mas o usuário HPC precisa de instrução explícita para optar por flags locais.

**Solução aplicada:** adicionada seção de README explicando `TBTB_OPT_FLAGS`, exemplo em `~/.R/Makevars`, reinstalação com `--preclean` e alerta para usar `-march=native` apenas em builds locais para a mesma CPU.

### `NOVO-v3-06` — `TuneBoostTreeBayesian` como wrapper em vez de alias direto

**Veredito: válido, baixo risco.** O wrapper só encaminhava todos os argumentos para `TuneBoostTree()` e adicionava um frame desnecessário. Como a função é um alias histórico, atribuição direta preserva a semântica desejada e padroniza com `TuneBoostTreeLimbo`.

**Solução aplicada:** `TuneBoostTreeBayesian <- TuneBoostTree`.

## Resultado consolidado

| Item | Veredito final | Ação |
| --- | --- | --- |
| `BUG-CRITICO-03` | Já resolvido | Sem mudança |
| `NOVO-01` | Já resolvido | Sem mudança |
| `NOVO-03` | Já resolvido | Sem mudança |
| `NOVO-04` | Código resolvido; doc pendente | README atualizado |
| `NOVO-v3-01` | Válido | `SetPassiveOpenMp` antecipado em `TuneBoostTree()` |
| `NOVO-v3-02` | Válido | API pública migrada para `engineBoostTree` com compatibilidade legada |
| `NOVO-v3-03` | Falso positivo | Sem mudança no Fortran |
| `NOVO-v3-04` | Válido | Removido recálculo de `totalCores` em CV manual e predições de CV |
| `NOVO-v3-05` | Válido | README atualizado com `TBTB_OPT_FLAGS` |
| `NOVO-v3-06` | Válido | Alias direto para `TuneBoostTreeBayesian` |
