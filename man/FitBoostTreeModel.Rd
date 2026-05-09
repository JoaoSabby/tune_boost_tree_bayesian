\name{FitBoostTreeModel}
\alias{FitBoostTreeModel}
\title{Ajustar modelo boosted tree final}
\usage{
FitBoostTreeModel(
  formula,
  dataTrain,
  hyperparameters,
  featureTypes = NULL,
  targetLevels = NULL,
  scalePosWeight = NULL,
  nThreads = 8L,
  seed = 42L,
  verbose = 0L,
  engine = "lightgbm"
)
}
\arguments{
\item{formula}{Fórmula de duas faces com uma variável resposta binária e preditores numéricos.}
\item{dataTrain}{data.frame, tibble ou data.table de treino. Deve conter a resposta e todos os preditores da fórmula, com exatamente duas classes na resposta.}
\item{hyperparameters}{Lista nomeada retornada por \code{TuneBoostTree()} em \code{bestHyperparameters}, ou lista equivalente com nomes canônicos. Deve conter os hiperparâmetros usados pela engine; se \code{threshold} estiver presente, ele será armazenado para a classificação padrão em \code{PredictBoostTreeModel()}.}
\item{featureTypes}{Vetor opcional de tipos de features do XGBoost, alinhado aos preditores. Use \code{NULL} para deixar a engine inferir o tratamento padrão.}
\item{targetLevels}{Ordenação opcional dos dois níveis da resposta. Use o valor retornado por \code{TuneBoostTree()} quando quiser preservar exatamente a mesma referência de classe positiva/negativa.}
\item{scalePosWeight}{Peso opcional da classe positiva. Quando \code{NULL}, é calculado como negativos/positivos no treino; quando \code{hyperparameters$scale_pos_weight} existe, esse valor prevalece.}
\item{nThreads}{Inteiro positivo com o número de threads internas da engine no ajuste final.}
\item{seed}{Inteiro usado como semente repassada aos parâmetros nativos da engine.}
\item{verbose}{Verbosidade da chamada final da engine. Use \code{0L}/\code{-1L} para saída compacta.}
\item{engine}{Nome da engine, \code{"xgboost"} ou \code{"lightgbm"}.}
}
\description{Ajusta o modelo final LightGBM ou XGBoost com hiperparâmetros canônicos, prepara a matriz numérica no mesmo padrão do tuner e devolve metadados necessários para predição e avaliação.}
\value{Lista nomeada com \code{model}, \code{params}, \code{featureNames}, \code{featureTypes}, \code{targetLevels}, \code{targetName}, \code{negativeClass}, \code{positiveClass}, \code{formulaInfo}, \code{nRounds}, \code{threshold} e \code{engine}.}
