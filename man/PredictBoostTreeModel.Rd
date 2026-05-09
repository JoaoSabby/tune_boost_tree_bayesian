\name{PredictBoostTreeModel}
\alias{PredictBoostTreeModel}
\title{Predizer com modelo boosted tree}
\usage{PredictBoostTreeModel(modelObj, newdata, threshold = NULL, engine = NULL)}
\arguments{
\item{modelObj}{Objeto retornado por \code{FitBoostTreeModel()}. Deve conter o modelo nativo, metadados de features, níveis da resposta e engine.}
\item{newdata}{data.frame, tibble ou data.table com todas as colunas preditoras usadas no ajuste. Colunas extras são ignoradas; colunas ausentes geram erro.}
\item{threshold}{Limiar de probabilidade da classe positiva. Quando \code{NULL}, usa \code{modelObj$threshold} se existir; caso contrário, usa \code{0.5}. Deve estar estritamente entre 0 e 1.}
\item{engine}{Sobrescrita opcional da engine; por padrão usa \code{modelObj$engine}. Normalmente deve permanecer \code{NULL}.}
}
\description{Retorna classes preditas e probabilidades das duas classes para um modelo boosted tree ajustado, usando o threshold tunado por padrão quando disponível.}
\value{Tibble com \code{predictedClass}, \code{probabilityFirstClass} e \code{probabilitySecondClass}. A segunda probabilidade corresponde à classe positiva armazenada no modelo.}
