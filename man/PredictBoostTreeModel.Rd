\name{PredictBoostTreeModel}
\alias{PredictBoostTreeModel}
\title{Predizer com modelo boosted tree}
\usage{PredictBoostTreeModel(modelObj, newdata, threshold = NULL, engineBoostTree = NULL, ...)}
\arguments{
\item{modelObj}{Objeto de modelo retornado por \code{FitBoostTreeModel()}.}
\item{newdata}{Novo data.frame contendo todas as colunas preditoras.}
\item{threshold}{Limiar de probabilidade da classe positiva. Quando \code{NULL}, usa \code{modelObj$threshold} se existir; caso contrário, usa \code{0.5}.}
\item{engineBoostTree}{Sobrescrita opcional da engine; por padrão usa \code{modelObj$engine}.}
\item{...}{Argumentos legados. \code{engine_boost_tree} ainda é aceito com aviso de depreciação e será migrado para \code{engineBoostTree}.}
}
\description{Retorna classes preditas e probabilidades das duas classes para um modelo boosted tree ajustado, usando o threshold tunado por padrão quando disponível.}
\value{Tibble com \code{predictedClass}, \code{probabilityFirstClass} e \code{probabilitySecondClass}. A segunda probabilidade corresponde à classe positiva armazenada no modelo.}
