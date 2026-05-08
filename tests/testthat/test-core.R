library(testthat)
library(TuneBoostTreeBayesian)
library(data.table)
library(tibble)
library(Matrix)

# 1. Testes de API e Validação de Inputs
test_that("Validadores de configuração rejeitam inputs perigosos", {
  expect_error(TuneBoostTreeBoostParams(trees = -10L), "positive integer")
  expect_error(TuneBoostTreeSearchSpace(mtry = c(1.5, 2.0)), "fractions in")
  expect_error(TuneBoostTreeCv(folds = 1L), "greater than or equal to 2")
  expect_error(TuneBoostTreeLimbo(acquisition = ""), "non-empty string")
  expect_error(TuneBoostTreeImbalance(scale_pos_weight = -5), "positive and finite")
})

# 2. Testes de Ingestão e Preparação de Dados
test_that("A ingestão de dados suporta múltiplos formatos tabulares de forma idêntica", {
  df <- data.frame(y = sample(c(0, 1), 100, replace = TRUE), x1 = rnorm(100), x2 = runif(100))
  tb <- as_tibble(df)
  dt <- as.data.table(df)
  
  res_df <- TuneBoostTreeBayesian(y ~ x1 + x2, df, initial = 2L, nIter = 0L, control = TuneBoostTreeControl(parallel = FALSE))
  res_tb <- TuneBoostTreeBayesian(y ~ x1 + x2, tb, initial = 2L, nIter = 0L, control = TuneBoostTreeControl(parallel = FALSE))
  res_dt <- TuneBoostTreeBayesian(y ~ x1 + x2, dt, initial = 2L, nIter = 0L, control = TuneBoostTreeControl(parallel = FALSE))
  
  expect_true(is.list(res_df$bestHyperparameters))
  expect_equal(res_df$bestHyperparameters, res_tb$bestHyperparameters, tolerance = 1e-4)
  expect_equal(res_tb$bestHyperparameters, res_dt$bestHyperparameters, tolerance = 1e-4)
})

test_that("Matrizes altamente esparsas disparam a conversão segura para dgCMatrix", {
  set.seed(42)
  mat_sparse <- data.frame(y = sample(0:1, 50, replace = TRUE), x1 = rbinom(50, 1, 0.05), x2 = rbinom(50, 1, 0.05))
  
  # A função interna deve converter para sparseMatrix quando a densidade de zeros > 0.7
  prep <- TuneBoostTreeBayesian:::TuneBoostTree_PrepareMatrix(y ~ ., mat_sparse)
  expect_true(inherits(prep$xMatrix, "sparseMatrix"))
})

# 3. Testes de Validação Cruzada e Estratificação
test_that("O particionamento de folds preserva a estratificação das classes", {
  y_imbalanced <- c(rep(0, 90), rep(1, 10))
  folds <- SplitDataBoostTreeFolds(y_imbalanced, nFolds = 5L)
  
  # Verificar se cada fold tem exatamente 2 amostras positivas (10 / 5)
  pos_counts <- vapply(folds, function(idx) sum(y_imbalanced[idx] == 1), integer(1))
  expect_true(all(pos_counts == 2L))
})

# 4. Testes de Robustez do Scoring PR-AUC (Blindagem de Backends)
test_that("Os backends de PR-AUC (C, Fortran e R base) produzem resultados equivalentes", {
  actual <- sample(c(0L, 1L), 200, replace = TRUE)
  predicted <- runif(200)
  
  score_c <- TuneBoostTreeBayesian:::TuneBoostTree_CalculatePrAuc(actual, predicted, backend = "c")
  score_f <- TuneBoostTreeBayesian:::TuneBoostTree_CalculatePrAuc(actual, predicted, backend = "fortran")
  score_r <- TuneBoostTreeBayesian:::TuneBoostTree_CalculatePrAuc(actual, predicted, backend = "r")
  
  expect_equal(score_c, score_f, tolerance = 1e-7)
  expect_equal(score_f, score_r, tolerance = 1e-7)
  
  # Testar comportamento face a inputs inválidos
  expect_true(is.na(TuneBoostTreeBayesian:::TuneBoostTree_CalculatePrAuc(integer(0), numeric(0))))
  expect_true(is.na(TuneBoostTreeBayesian:::TuneBoostTree_CalculatePrAuc(c(0,0,0), c(0.1,0.2,0.3)))) # Sem classe positiva
})

# 5. Testes de Integração e Warm-Start (Limbo)
test_that("O sistema de Warm-Start deduplica históricos e processa data.frames de forma estável", {
  bounds <- TuneBoostTreeSearchSpace()
  param_names <- names(bounds)
  
  hist_grid <- data.frame(
    learn_rate = c(0.1, 0.1, 0.05),
    tree_depth = c(4, 4, 6),
    min_n = c(10, 10, 20),
    sample_size = c(0.8, 0.8, 0.9),
    mtry = c(0.7, 0.7, 0.5),
    loss_reduction = c(0, 0, 1),
    max_bin = c(256, 256, 128),
    Value = c(0.85, 0.90, 0.88) # O segundo é duplicado, mas com melhor score
  )
  
  dedup <- TuneBoostTreeBayesian:::TuneBoostTree_DeduplicateInitGrid(hist_grid, bounds)
  
  expect_equal(nrow(dedup), 2L)
  # Deve manter a entrada duplicada com o Value mais alto (0.90)
  expect_equal(max(dedup$Value[dedup$learn_rate == 0.1]), 0.90)
})

test_that("Fallback seguro é ativado quando o Limbo não está disponível em Strict Mode = FALSE", {
  df <- data.frame(y = sample(0:1, 50, replace=TRUE), x = rnorm(50))
  
  expect_warning(
    TuneBoostTreeBayesian(
      y ~ x, df, initial = 2L, nIter = 1L, 
      optimizer = TuneBoostTreeLimbo(command = "caminho_inexistente", fallback = TRUE)
    )
  )
})

test_that("Strict Limbo Mode falha previsivelmente e aborta a execução", {
  df <- data.frame(y = sample(0:1, 50, replace=TRUE), x = rnorm(50))
  
  expect_error(
    TuneBoostTreeBayesianUltra(
      y ~ x, df, initial = 2L, nIter = 1L, 
      command = "caminho_inexistente", strict_limbo = TRUE
    )
  )
})

# 6. Teste de Fluxo Completo: Treino e Previsão
test_that("O pipeline completo treina, extrai metadados e gera probabilidades corretas", {
  df <- data.frame(y = sample(c("Não", "Sim"), 150, replace = TRUE), x1 = rnorm(150), x2 = runif(150))
  
  # Simular output do tuner
  best_params <- list(learn_rate = 0.1, tree_depth = 4, min_n = 5, sample_size = 0.8, 
                      mtry = 1, loss_reduction = 0, max_bin = 64, trees = 10, stop_iter = 5)
  
  modelo <- FitBoostTreeModel(y ~ x1 + x2, df, best_params)
  
  expect_equal(modelo$positiveClass, "Sim")
  expect_equal(modelo$negativeClass, "Não")
  
  preds <- PredictBoostTreeModel(modelo, df[1:5, ])
  
  expect_true(all(c("predictedClass", "probabilityFirstClass", "probabilitySecondClass") %in% names(preds)))
  expect_true(all(preds$probabilitySecondClass >= 0 & preds$probabilitySecondClass <= 1))
})
  
