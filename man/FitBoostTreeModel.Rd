\name{FitBoostTreeModel}
\alias{FitBoostTreeModel}
\title{Ajustar modelo boosted tree}
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
  engineBoostTree = "lightgbm",
  ...
)
}
\arguments{
\item{formula}{Fórmula de duas faces com uma variável resposta binária e preditores numéricos.}
\item{dataTrain}{data.frame de treino. A resposta deve ter exatamente duas classes.}
\item{hyperparameters}{Lista nomeada retornada por \code{TuneBoostTree()} ou lista equivalente com nomes canônicos de hiperparâmetros. Se \code{threshold} estiver presente, ele é armazenado para classificação padrão no momento da predição.}
\item{featureTypes}{Vetor opcional de tipos de features do XGBoost.}
\item{targetLevels}{Ordenação opcional de dois níveis da variável resposta.}
\item{scalePosWeight}{Peso opcional da classe positiva; é calculado quando \code{NULL}.}
\item{nThreads}{Inteiro com número de threads da engine.}
\item{seed}{Inteiro usado como semente aleatória.}
\item{verbose}{Verbosidade da engine.}
\item{engineBoostTree}{Nome da engine, \code{"xgboost"} ou \code{"lightgbm"}.}
\item{...}{Argumentos legados. \code{engine_boost_tree} ainda é aceito com aviso de depreciação e será migrado para \code{engineBoostTree}.}
}
\description{Ajusta o modelo final LightGBM ou XGBoost com hiperparâmetros canônicos e isola a tradução de parâmetros no limite da engine.}
\value{Lista nomeada com o modelo nativo em \code{model}, parâmetros efetivamente usados em \code{params}, nomes/tipos de features, níveis e nomes das classes, metadados da fórmula, número de rodadas em \code{nRounds}, \code{threshold} e \code{engine}.}
